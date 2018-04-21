
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
#' @param x DBI database handle or rquery_db_info object.
#' @param id character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted identifier
#'
#' @noRd
#'
quote_identifier <- function (x, id, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  if("rquery_db_info" %in% class(x)) {
    return(x$dbqi(id))
  }
  as.character(DBI::dbQuoteIdentifier(x, id))
}

#' Quote a string
#'
#' @param x DBI database handle or rquery_db_info object.
#' @param s character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted string
#'
#' @noRd
#'
quote_string <- function (x, s, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  s <- as.character(s)
  if("rquery_db_info" %in% class(x)) {
    return(x$dbqs(s))
  }
  as.character(DBI::dbQuoteString(x, s))
}

#' Quote a value
#'
#' @param x DBI database handle or rquery_db_info object.
#' @param s character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted string
#'
#' @noRd
#'
quote_literal <- function (x, s, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  if(is.character(s) || is.factor(s)) {
    quote_string(x, as.character(s))
  }
  if("rquery_db_info" %in% class(x)) {
    return(x$dbql(s))
  }
  as.character(DBI::dbQuoteLiteral(x, s))
}

