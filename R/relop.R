
# define relational op, basis for this package.
# in addition to methods below, our nodes implement: format() and print().


#' Return DBI connection.
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments
#' @return DBI handle
#'
#' @export
#'
dbi_connection <- function (x, ...) {
  UseMethod("dbi_connection", x)
}



#' Return column names
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments
#' @return column names
#'
#' @export
#'
column_names <- function (x, ...) {
  UseMethod("column_names", x)
}


#' Return SQL command of operation chain
#'
#' @param x rquery operation tree.
#' @param indent_level level to indent
#' @param tnum temp sub-query name generators
#' @param append_cr logical if TRUE end with CR
#' @param ... generic additional arguments
#' @return SQL command
#'
#' @export
#'
to_sql <- function (x,
                    indent_level = 0,
                    tnum = cdata::makeTempNameGenerator('tsql'),
                    append_cr = TRUE,
                    ...) {
  UseMethod("to_sql", x)
}
