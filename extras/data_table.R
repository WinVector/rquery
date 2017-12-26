
#' Return data.table implementation of operation tree.
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments (not used)
#' @param env environment to evaluate in
#' @return data.table operation tree
#'
#' @export
#'
to_data_table <- function (x,
                    ...,
                    env = parent.frame()) {
  UseMethod("to_data_table", x)
}

#' Wrap a data.table for rquery pipeline.
#'
#' @param dt data.table or data.frame
#' @param table_name name of data
#' @return rquery node
#'
data_table_source <- function(dt, table_name = deparse(substitute(dt))) {
  table_source(table_name = table_name,
               columns = colnames(dt),
               db_info = rquery_db_info(indentifier_quote_char = '`',
                                        string_quote_char = '"'))
}

#' @export
to_data_table.relop_table_source <- function (x,
                                              ...,
                                              env = parent.frame()) {
  x$table_name
}

#' @export
to_data_table.relop_extend <- function (x,
                                 ...,
                                 env = parent.frame()) {
  expr <- to_data_table(x$source[[1]], env = env)
  n <- length(x$parsed)
  byi <- ""
  if(length(x$partitionby)>0) {
    # TODO: deal with more than one partition column
    if(length(x$partitionby)!=1) {
      stop("to_data_table.relop_extend does not yet support more than one partition column")
    }
    byi <- paste0(" ,", x$partitionby[[1]])
  }
  for(i in 1:n) {
    expri <- paste(x$parsed[[i]]$symbols_produced,
                   ":=",
                   gsub("=", "==",
                        as.character(x$parsed[[i]]$parsed),
                        fixed = TRUE))
    stmti <- paste0("[, ", expri, byi, "]")
    expr <- paste0(expr, stmti)
  }
  expr
}
