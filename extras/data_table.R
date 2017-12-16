
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
  r <- list(source = list(),
            table_name = table_name,
            parsed = NULL,
            columns = colnames(dt),
            dbqi = function(id) {  paste0('`', id, '`')  },
            dbqs = function(s) { paste0('"', s, '"') })
  class(r) <- c("relop_table_source", "relop")
  r
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
  # turn on data.table semantics via data.table:::cedta()
  # https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package
  .datatable.aware <- TRUE
  # data.table has in-place mutate semantics
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
    expri <- paste(names(x$parsed[[i]]$parsed),
                   ":=",
                   gsub("=", "==",
                        as.character(x$parsed[[i]]$parsed),
                        fixed = TRUE))
    stmti <- paste0("[, ", expri, byi, "]")
    expr <- paste0(expr, stmti)
  }
  expr
}
