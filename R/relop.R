
# define relational op, basis for this package.
# in addition to methods below, our nodes implement: format() and print().
#
# Each node should be a list with named entries:
#   source: list of nodes supplying values to this node.
#   table_name: character name of table if a concrete table or view.


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
#' @return vector of column names
#'
#' @export
#'
column_names <- function (x, ...) {
  UseMethod("column_names", x)
}

#' Return columns used
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments (not used)
#' @param using character, if not NULL set of columns used from above.
#' @param contract logical, if TRUE perform unused value elimination.
#' @return vector of table qualified column names.
#'
#' @export
#'
columns_used <- function (x, ...,
                          using = NULL,
                          contract = FALSE) {
  UseMethod("columns_used", x)
}


#' Return SQL command of operation chain
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments (not used)
#' @param indent_level level to indent
#' @param tnum temp sub-query name generators
#' @param append_cr logical if TRUE end with CR
#' @param using character, if not NULL set of columns used from above.
#' @return SQL command
#'
#' @export
#'
to_sql <- function (x,
                    ...,
                    indent_level = 0,
                    tnum = mkTempNameGenerator('tsql'),
                    append_cr = TRUE,
                    using = NULL) {
  UseMethod("to_sql", x)
}
