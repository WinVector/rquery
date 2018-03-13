

quantile_col <- function(db, incoming_table_name, probs, ci) {
  qcount <- paste0("
     SELECT\n
        COUNT(1)\n
     FROM\n
        ", DBI::dbQuoteIdentifier(incoming_table_name), "\n
     WHERE\n
        ", DBI::dbQuoteIdentifier(incoming_table_name), " IS NOT NULL\n")
  nrows <- DBI::dbGetQuery(db, qcount)
  if(nrows<1) {
    return(rep(NA, length(probs)))
  }
  indexes <- round(as.numeric(stats::quantile(seq_len(nrows)), probs = probs))
  indexes <- pmax(1, indexes)
  indexes <- pmin(nrows, indexes)
  indexes_str <- paste(indexes, collapse = ", ")
  q <- paste0("
     SELECT\n
        *\n
      FROM (\n
        SELECT\n
           ", DBI::dbQuoteIdentifier(ci), ",\n
           COUNT(1) AS idx_rquery OVER (ORDER BY ", DBI::dbQuoteIdentifier(ci), ")\n
        FROM\n
           ", DBI::dbQuoteIdentifier(incoming_table_name), ",\n
        WHERE
           ", DBI::dbQuoteIdentifier(ci), " IS NOT NULL\n
      ) rquery_qtable\n
     WHERE\n
        idx_rquery IN (", indexes_str, ")\n
     ORDER BY\n
        idx_rquery\n")
  r <- DBI::dbGetQuery(db, q)
  r[[ci]]
}

quantile_cols <- function(db, incoming_table_name, probs, probs_name, cols) {
  qtable <- data.frame(probs = probs)
  colnames(qtable) <- probs_name
  for(ci in cols) {
    qi <- quantile_col(db, incoming_table_name, probs, ci)
    qtable[[ci]] <- qi
  }
  qtable
}

#' Compute quantiles over non-NULL values
#' (without interpolation, needs a database with window functions).
#'
#' Note: not finished yet.
#'
#'
#' @param source source to select from (relop or data.frame).
#' @param cols character, compute quantiles for these columns
#' @param ... force later arguments to be bound by name
#' @param probs_name character, column name to write probs in.
#' @param probs numeric quantiles to compute
#' @param incoming_table_name character, name of incoming table.
#' @param outgoing_table_name character, name of table to write.
#' @param overwrite logical, if TRUE overwrite tables
#' @param temporary logical, if TRUE use temporary tables
#' @return table of quantiles
#'
#' @seealso \code{\link{rsummary}}, \code{\link{non_sql_node}}
#'
#' @export
#'
quantile_node <- function(source,
                          cols,
                          ...,
                          probs_name = "probs",
                          probs = seq(0, 1, 0.25),
                          incoming_table_name = mk_tmp_name_source("qin")(),
                          outgoing_table_name = mk_tmp_name_source("qout")(),
                          overwrite = TRUE,
                          temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::quantile_node.relop")
  if(probs_name %in% cols) {
    stop("rquery::quantile_node.relop probs_name must be disjoint from cols")
  }
  have <- column_names(source)
  check_have_cols(have, cols, "rquery::quantile_node.relop cols")
  force(cols)
  force(probs_name)
  force(probs)
  f <- function(db,
                incoming_table_name,
                outgoing_table_name) {
    qtable <- quantile_cols(db, incoming_table_name, probs, probs_name, cols)
    dbi_copy_to(db,
                table_name = outgoing_table_name,
                d = qtable,
                overwrite = overwrite,
                temporary = temporary)
  }
  nd <- non_sql_node(source,
                     f,
                     incoming_table_name = incoming_table_name,
                     columns_used = cols,
                     probs_name = probs_name,
                     probs = probs,
                     outgoing_table_name = outgoing_table_name,
                     columns_produced = c(probs_name, cols),
                     display_form = paste0("quantile_node(., ",
                                           incoming_table_name,
                                           ", ",
                                           outgoing_table_name,
                                           ")"),
                     orig_columns = FALSE,
                     overwrite = overwrite,
                     temporary = temporary)
  nd
}
