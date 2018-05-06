
#' Make an orderby node (not a relational operation).
#'
#' Order a table by a set of columns (not general expressions) and
#' limit number of rows in that order.
#'
#' Note: this is a relational operator in that it takes a table that
#' is a relation (has unique rows) to a table that is still a relation.
#' However, most relational systems do not preserve row order in storage or between
#' operations.  So without the limit set this is not a useful operator except
#' as a last step prior ot pulling data to an in-memory \code{data.frame} (
#' which does preserve row order).
#'
#'
#' @param source source to select from.
#' @param cols order by column names.
#' @param ... force later arguments to be bound by name
#' @param rev_cols order by reverse of these column names.
#' @param limit number limit row count.
#' @return order_by node.
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- dbi_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'   optree <- orderby(d, rev_cols = "AUC", limit=4)
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
orderby <- function(source,
                     cols = NULL,
                     ...,
                     rev_cols = NULL,
                     limit = NULL) {
  UseMethod("orderby", source)
}

#' @export
orderby.relop <- function(source,
                    cols = NULL,
                    ...,
                    rev_cols = NULL,
                    limit = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::orderby.relop")
  have <- column_names(source)
  check_have_cols(have, unique(c(cols, rev_cols)), "rquery::orderby")
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            orderby = cols,
            rev_orderby = rev_cols,
            limit = limit)
  r <- relop_decorate("relop_orderby", r)
  r
}

#' @export
orderby.data.frame <- function(source,
                    cols = NULL,
                    ...,
                    rev_cols = NULL,
                    limit = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::orderby.data.frame")
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- orderby(dnode,
                   orderby = cols,
                   rev_orderby = rev_cols,
                   limit = limit)
  return(enode)
}




#' @export
format_node.relop_orderby <- function(node) {
  ot <- c(node$orderby)
  if(length(node$rev_orderby)>0) {
    ot <- c(ot, paste0("desc(", node$rev_orderby, ")"))
  }
  paste0("orderby(., ",
         ifelse(length(ot)>0,
                paste(ot, collapse = ", "),
                ""),
         ifelse((length(node$limit)>0) && (length(node$orderby)>0),
                paste0(", LIMIT ",
                       format(ceiling(node$limit), scientific = FALSE)),
                ""),
         ")",
         "\n")
}



calc_used_relop_orderby <- function (x, ...,
                                      using = NULL,
                                      contract = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::calc_used_relop_orderby")
  if(length(using)<=0) {
    using <- column_names(x)
  }
  consuming <- x$orderby
  using <- unique(c(using, consuming))
  missing <- setdiff(using, column_names(x$source[[1]]))
  if(length(missing)>0) {
    stop(paste("rquery::calc_used_relop_orderby unknown columns",
               paste(missing, collapse = ", ")))
  }
  using
}

#' @export
columns_used.relop_orderby <- function (x, ...,
                                        using = NULL,
                                        contract = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::columns_used.relop_orderby")
  cols <- calc_used_relop_orderby(x,
                                  using = using,
                                  contract = contract)
  return(columns_used(x$source[[1]],
                      using = cols,
                      contract = contract))
}


#' @export
to_sql.relop_orderby <- function (x,
                                  db,
                                  ...,
                                  limit = NULL,
                                  source_limit = NULL,
                                  indent_level = 0,
                                  tnum = mk_tmp_name_source('tsql'),
                                  append_cr = TRUE,
                                  using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::to_sql.relop_orderby")
  cols1 <- column_names(x$source[[1]])
  cols <- vapply(cols1,
                 function(ci) {
                   quote_identifier(db, ci)
                 }, character(1))
  ot <- vapply(x$orderby,
               function(ci) {
                 quote_identifier(db, ci)
               }, character(1))
  if(length(x$rev_orderby)>0) {
    rev_ot <- vapply(x$rev_orderby,
                     function(ci) {
                       paste(quote_identifier(db, ci), "DESC")
                     }, character(1))
    ot <- c(ot, rev_ot)
  }
  subcols <- calc_used_relop_orderby(x, using=using)
  qlimit = limit
  if(!getDBOption(db, "use_pass_limit", TRUE)) {
    qlimit = NULL
  }
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        limit = qlimit,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = subcols)
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT * FROM (\n",
         subsql, "\n",
         prefix, ") ",
         tab,
         ifelse(length(ot)>0,
                paste0(" ORDER BY ", paste(ot, collapse = ", ")),
                ""))
  if(!is.null(x$limit)) {
    limit <- min(limit, x$limit)
  }
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}

