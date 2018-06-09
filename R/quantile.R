

quantile_col <- function(db, incoming_table_name, probs, ci) {
  qcount <- paste0("
     SELECT
        COUNT(1)
     FROM
        ", quote_identifier(db, incoming_table_name), "
     WHERE
        ", quote_identifier(db, ci), " IS NOT NULL")
  nrows <- as.numeric(rq_get_query(db, qcount)[[1]][[1]])
  # was coming back s integer64 and messing up pmax(), pmin()
  if(nrows<1) {
    return(rep(NA, length(probs)))
  }
  indexes <- round((nrows+0.5)*probs)
  indexes <- pmax(1, indexes)
  indexes <- pmin(nrows, indexes)
  # deal with repeated indexes
  # also make sure index 1 is present so we get something back
  # if there is only one value
  uindexes <- sort(unique(c(1, indexes, nrows)))
  indexes_str <- paste(uindexes, collapse = ", ")
  unpack <- match(indexes, uindexes)
  q <- paste0("
     SELECT
        *
      FROM (
        SELECT
           ", quote_identifier(db, ci), ",
           ROW_NUMBER() OVER (ORDER BY ", quote_identifier(db, ci), ")  AS idx_rquery
        FROM
           ", quote_identifier(db, incoming_table_name), "
        WHERE
           ", quote_identifier(db, ci), " IS NOT NULL
      ) rquery_qtable
     WHERE
        idx_rquery IN (", indexes_str, ")
     ORDER BY
        idx_rquery")
  r <- rq_get_query(db, q)
  r[[ci]][unpack]
}

# same semantics as DB fn.
quantile_col_d <- function(d, probs, ci) {
  dcol <- d[[ci]][!is.na(d[[ci]])]
  nrows <- length(dcol)
  # was coming back s integer64 and messing up pmax(), pmin()
  if(nrows<1) {
    return(rep(NA, length(probs)))
  }
  indexes <- round((nrows+0.5)*probs)
  indexes <- pmax(1, indexes)
  indexes <- pmin(nrows, indexes)
  # deal with repeated indexes
  # also make sure index 1 is present so we get something back
  # if there is only one value
  uindexes <- sort(unique(c(1, indexes, nrows)))
  indexes_str <- paste(uindexes, collapse = ", ")
  unpack <- match(indexes, uindexes)
  r <- sort(dcol)[uindexes]
  r[unpack]
}

#' Compute quantiles of specified columns
#' (without interpolation, needs a database with window functions).
#'
#' @param db database connection
#' @param incoming_table_name name of table to compute quantiles of
#' @param ... force later arguments to bind by name
#' @param probs numeric, probabilites to compute quantiles of
#' @param probs_name character name for probability column
#' @param cols charancter, columns to compute quantiles of
#' @return data.frame of quantiles
#'
#' @seealso \code{\link{quantile_node}}, \code{\link{rsummary}}
#'
#' @export
#'
quantile_cols <- function(db, incoming_table_name,
                          ...,
                          probs = seq(0, 1, 0.25),
                          probs_name = "quantile_probability",
                          cols = rq_colnames(db, incoming_table_name)) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::quantile_cols")
  qtable <- data.frame(probs = probs)
  colnames(qtable) <- probs_name
  for(ci in cols) {
    qi <- quantile_col(db, incoming_table_name, probs, ci)
    qtable[[ci]] <- qi
  }
  qtable
}

quantile_cols_d <- function(d,
                            ...,
                            probs = seq(0, 1, 0.25),
                            probs_name = "quantile_probability",
                            cols = column_names(d)) {
  if(!is.data.frame(d)) {
    stop("rquery::quantile_cols_d: d must be a data.frame")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::quantile_cols_d")
  qtable <- data.frame(probs = probs)
  colnames(qtable) <- probs_name
  for(ci in cols) {
    qi <- quantile_col_d(d, probs, ci)
    qtable[[ci]] <- qi
  }
  qtable
}


#' Compute quantiles over non-NULL values
#' (without interpolation, needs a database with window functions).
#'
#' Please see \url{https://github.com/WinVector/rquery/blob/master/extras/Summary_Example.md} for an example.
#'
#' This is a non_sql_node, so forces the materialization of
#' the calculation prior to it losing narrowing optimizations.
#'
#' @param source source to select from (relop or data.frame).
#' @param cols character, compute quantiles for these columns (NULL indicates all columns).
#' @param ... force later arguments to be bound by name
#' @param probs_name character, column name to write probs in.
#' @param probs numeric quantiles to compute
#' @param tmp_name_source wrapr::mk_tmp_name_source(), temporary name generator.
#' @param temporary logical, if TRUE use temporary tables
#' @return table of quantiles
#'
#' @seealso \code{\link{quantile_cols}}, \code{\link{rsummary}}, \code{\link{non_sql_node}}
#'
#'
#' @export
#'
quantile_node <- function(source,
                          cols = NULL,
                          ...,
                          probs_name = "quantile_probability",
                          probs = seq(0, 1, 0.25),
                          tmp_name_source = wrapr::mk_tmp_name_source("qn"),
                          temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::quantile_node.relop")
  if(probs_name %in% cols) {
    stop("rquery::quantile_node.relop probs_name must be disjoint from cols")
  }
  have <- column_names(source)
  if(!is.null(cols)) {
    check_have_cols(have, cols, "rquery::quantile_node.relop cols")
  } else {
    cols <- have
  }
  force(cols)
  force(probs_name)
  force(probs)
  force(temporary)
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name) {
    qtable <- quantile_cols(db, incoming_table_name,
                            probs = probs,
                            probs_name = probs_name,
                            cols = cols)
    rq_copy_to(db,
               table_name = outgoing_table_name,
               d = qtable,
               overwrite = TRUE,
               temporary = temporary)
  }
  f_df <- function(d) {
    quantile_cols_d(d,
                    probs = probs,
                    probs_name = probs_name,
                    cols = cols)
  }
  nd <- non_sql_node(source,
                     f_db = f_db,
                     f_df = f_df,
                     incoming_table_name = incoming_table_name,
                     outgoing_table_name = outgoing_table_name,
                     columns_produced = c(probs_name, cols),
                     display_form = paste0("quantile_node(.)"),
                     pass_using = TRUE,
                     orig_columns = FALSE,
                     temporary = temporary)
  nd
}
