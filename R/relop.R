
# define relational op, basis for this package.
# in addition to methods below, our nodes implement: format() and print().


#' Return column names
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments
#' @return DBI handle
#'
#' @export
#'
column_names <- function (x, ...) {
  UseMethod("column_names", x)
}


#' Return SQL command of operation chain
#'
#' @param x rquery operation tree.
#' @param db DBI database connection
#' @param indent_level level to indent
#' @param tnum temp sub-query name generators
#' @param append_cr logical if TRUE end with CR
#' @param ... generic additional arguments
#' @return SQL command
#'
#' @export
#'
to_sql <- function (x,
                    db,
                    indent_level = 0,
                    tnum = cdata::makeTempNameGenerator('tsql'),
                    append_cr = TRUE,
                    ...) {
  UseMethod("to_sql", x)
}
