
#' Build a db information stand-in
#'
#' @param indentifier_quote_char character
#' @param string_quote_char character
#' @return rquery_db_info object
#'
#' @export
#'
rquery_db_info <- function(indentifier_quote_char,
                           string_quote_char) {
  # does not handle quotes inside strings
  r <- list(indentifier_quote_char = indentifier_quote_char,
            string_quote_char = string_quote_char,
            dbqi = function(id) { paste0(indentifier_quote_char,
                                         id,
                                         indentifier_quote_char) },
            dbqs = function(s) { paste0(string_quote_char,
                                        s,
                                        string_quote_char) },
            dbql = function(s) { format(s, scientific = 11) })
  class(r) <- "rquery_db_info"
  r
}

#' Quote an identifier.
#'
#' @param x database handle or rquery_db_info object.
#' @param id character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted identifier
#'
#' @noRd
#'
quote_identifier <- function (x, id, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::quote_identifier")
  if("rquery_db_info" %in% class(x)) {
    return(x$dbqi(id))
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  as.character(DBI::dbQuoteIdentifier(x, id))
}

#' Quote a string
#'
#' @param x database handle or rquery_db_info object.
#' @param s character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted string
#'
#' @noRd
#'
quote_string <- function (x, s, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::quote_string")
  s <- as.character(s)
  if("rquery_db_info" %in% class(x)) {
    return(x$dbqs(s))
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  as.character(DBI::dbQuoteString(x, s))
}

#' Quote a value
#'
#' @param x database handle or rquery_db_info object.
#' @param s character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted string
#'
#' @noRd
#'
quote_literal <- function (x, s, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::quote_literal")
  if(is.character(s) || is.factor(s)) {
    quote_string(x, as.character(s))
  }
  if("rquery_db_info" %in% class(x)) {
    return(x$dbql(s))
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  as.character(DBI::dbQuoteLiteral(x, s))
}

