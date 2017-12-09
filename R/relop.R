
# define relational op, basis for this package.
# in addition to methods below, our nodes implement: format() and print().


#' Quote an idnetifier.
#'
#' @param x rquery operation tree.
#' @param id character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted identifier
#'
#' @export
#'
quote_identifier <- function (x, id, ...) {
  UseMethod("quote_identifier", x)
}

#' Quote a string
#'
#' @param x rquery operation tree.
#' @param s character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted string
#'
#' @export
#'
quote_string <- function (x, s, ...) {
  UseMethod("quote_string", x)
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
                    tnum = mkTempNameGenerator('tsql'),
                    append_cr = TRUE,
                    ...) {
  UseMethod("to_sql", x)
}
