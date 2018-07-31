
#' Build a db information stand-in
#'
#' @param ... force all arguments to be by name.
#' @param connection connection handle to database or Spark.
#' @param is_dbi if TRUE the database connection can be used with DBI.
#' @param identifier_quote_char character, quote to put around identifiers.
#' @param string_quote_char character, quote to put around strings.
#' @param overrides named list of functions to place in info.
#' @param note character note to add to display form.
#' @param connection_options names list of per-connection options.
#' @return rquery_db_info object
#'
#' @export
#'
rquery_db_info <- function(...,
                           connection = NULL,
                           is_dbi = FALSE,
                           identifier_quote_char = NULL,
                           string_quote_char = NULL,
                           overrides = NULL,
                           note = "",
                           connection_options = list()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::rquery_db_info")
  if("rquery_db_info" %in% class(connection)) {
    stop("rquery::rquery_db_info connection is already of class rquery_db_info")
  }
  # does not handle quotes inside strings
  r <- list(
    connection = connection,
    is_dbi = is_dbi,
    identifier_quote_char = identifier_quote_char,
    string_quote_char = string_quote_char,
    note = note,
    connection_options = connection_options,
    dbqi = function(id) {
      paste0(identifier_quote_char,
             id,
             identifier_quote_char)
    },
    dbqt = function(id, qualifiers = NULL) {
      paste(paste0(identifier_quote_char,
                   c(qualifiers, id),
                   identifier_quote_char),
            collapse = ".")
    },
    dbqs = function(s) {
      paste0(string_quote_char,
             s,
             string_quote_char)
    },
    dbql = function(o) {
      if(is.character(o) || is.factor(o)) {
        return(paste0(string_quote_char,
                      as.character(o),
                      string_quote_char))
      }
      format(o, scientific = 11)
    })
  if(is_dbi) {
    if(!requireNamespace("DBI", quietly = TRUE)) {
      stop("rquery::rquery_db_info is_dbi=TRUE requeries DBI package")
    }
    r$quote_identifier <- function(x, id) {
      as.character(DBI::dbQuoteIdentifier(r$connection, as.character(id)))
    }
    r$quote_table_name <- function(x, id, ..., qualifiers) {
      wrapr::stop_if_dot_args(substitute(list(...)),
                              "r$quote_table_name")
      if("schema" %in% names(qualifiers)) {
        dbi_id <- DBI::Id(schema = qualifiers$schema, table = as.character(id))
      } else {
        dbi_id <- as.character(id) # sparklyr ‘0.8.4’ does not implement DBI::dbQuoteIdentifier for DBI::Id
      }
      as.character(DBI::dbQuoteIdentifier(r$connection, dbi_id))
    }
    r$quote_string <- function(x, s) {
      DBI::dbQuoteString(r$connection, as.character(s))
    }
    r$quote_literal <- function(x, o) {
      if(is.character(o) || is.factor(o)) {
        return(DBI::dbQuoteString(r$connection, as.character(o)))
      }
      as.character(DBI::dbQuoteLiteral(r$connection, o))
    }
  }
  for(ni in names(overrides)) {
    r[[ni]] <- overrides[[ni]]
  }
  class(r) <- "rquery_db_info"
  r
}

#' @export
format.rquery_db_info <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::format.rquery_db_info")
  paste0("rquery_db_info(",rq_connection_name(x), ", is_dbi=", x$is_dbi, ", note=\"", x$note, "\")")
}

#' @export
print.rquery_db_info <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::print.rquery_db_info")
  print(format(x))
}


rquery_default_db_info <- rquery_db_info(identifier_quote_char = '"',
                                         string_quote_char = "'",
                                         is_dbi = FALSE)

#' Quote an identifier.
#'
#' @param x database handle or rquery_db_info object.
#' @param id character to quote
#' @return quoted identifier
#'
#' @export
#'
quote_identifier <- function(x, id) {
  if("rquery_db_info" %in% class(x)) {
    f <- x$quote_identifier
    if(!is.null(f)) {
      return(f(x, id))
    }
    return(x$dbqi(id))
  }
  if(requireNamespace("DBI", quietly = TRUE)) {
    return(as.character(DBI::dbQuoteIdentifier(x, id)))
  }
  rquery_default_db_info$dbqi(id)
}

#' Quote a table name.
#'
#' @param x database handle or rquery_db_info object.
#' @param id character to quote
#' @param ... not used, force later arguments to bind by name.
#' @param qualifiers named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @return quoted identifier
#'
#' @export
#'
quote_table_name <- function(x, id,
                             ...,
                             qualifiers) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::quote_table_name")
  if("rquery_db_info" %in% class(x)) {
    f <- x$quote_table_name
    if(!is.null(f)) {
      return(f(x, id, qualifiers = qualifiers))
    }
    return(x$dbqt(id, qualifiers = qualifiers))
  }
  if(requireNamespace("DBI", quietly = TRUE)) {
    if("schema" %in% names(qualifiers)) {
      dbi_id <- DBI::Id(schema = qualifiers[["schema"]], table = as.character(id))
    } else {
      dbi_id <- as.character(id) # sparklyr ‘0.8.4’ does not implement DBI::dbQuoteIdentifier for DBI::Id
    }
    return(as.character(DBI::dbQuoteIdentifier(x, dbi_id)))
  }
  rquery_default_db_info$dbqi(id)
}


#' Quote a string
#'
#' @param x database handle or rquery_db_info object.
#' @param s character to quote
#' @return quoted string
#'
#' @export
#'
quote_string <- function(x, s) {
  s <- as.character(s)
  if("rquery_db_info" %in% class(x)) {
    f <- x$quote_string
    if(!is.null(f)) {
      return(f(x, s))
    }
    return(x$dbqs(s))
  }
  if(requireNamespace("DBI", quietly = TRUE)) {
    return(as.character(DBI::dbQuoteString(x, s)))
  }
  rquery_default_db_info$dbqs(s)
}

#' Quote a value
#'
#' @param x database handle or rquery_db_info object.
#' @param o value to quote
#' @return quoted string
#'
#' @export
#'
quote_literal <- function(x, o) {
  if(is.character(o) || is.factor(o)) {
    return(quote_string(x, as.character(o)))
  }
  if("rquery_db_info" %in% class(x)) {
    f <- x$quote_literal
    if(!is.null(f)) {
      return(f(x, o))
    }
    return(x$dbql(o))
  }
  if(requireNamespace("DBI", quietly = TRUE)) {
    return(as.character(DBI::dbQuoteLiteral(x, o)))
  }
  rquery_default_db_info$dbql(o)
}

