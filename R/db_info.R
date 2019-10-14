

tree_rewriter <- function(x, db_info) {
  expr_map <- db_info$expr_map
  if(("pre_sql_sub_expr" %in% class(x)) && (length(x$toks)>0)) {
    # special case: zero argument call (unfortunately stored differently)
    if(isTRUE(x$toks[[1]]$is_zero_argument_call)) {
      key <- x$toks[[1]][["value"]]
      replacement <- expr_map[[key]]
      if(!is.null(replacement)) {
        x_translated <- x
        x_translated$toks <- replacement
        for(i in seq_len(length(replacement))) {
          if(is.numeric(replacement[[i]])) {
            x_translated$toks[[i]] <- x$toks[[replacement[[i]]]]
          }
        }
        return(x_translated)
      }
    }
    # first recurse
    for(i in seq_len(length(x$toks))) {
      x$toks[[i]] <- tree_rewriter(x$toks[[i]], db_info)
    }
    # now look for special cases
    if(("pre_sql_token" %in% class(x$toks[[1]])) &&
       (x$toks[[1]]$token_type == "function_name")) {
      key <- x$toks[[1]][["value"]]
      replacement <- expr_map[[key]]
      if(!is.null(replacement)) {
        x_translated <- x
        x_translated$toks <- replacement
        for(i in seq_len(length(replacement))) {
          if(is.numeric(replacement[[i]])) {
            x_translated$toks[[i]] <- x$toks[[replacement[[i]]]]
          }
        }
        return(x_translated)
      }
    }
  }
  x
}

#' Build a db information stand-in
#'
#' @param ... force all arguments to be by name.
#' @param connection connection handle to database or Spark.
#' @param is_dbi if TRUE the database connection can be used with DBI.
#' @param identifier_quote_char character, quote to put around identifiers.
#' @param string_quote_char character, quote to put around strings.
#' @param overrides named list of functions to place in info.
#' @param note character note to add to display form.
#' @param connection_options named list of per-connection options.
#' @param db_methods named list of to_sql methods.
#' @return rquery_db_info object
#'
#' @export
#'
rquery_db_info <- function(...,
                           connection = NULL,
                           is_dbi = FALSE,
                           identifier_quote_char = '"',
                           string_quote_char = "'",
                           overrides = NULL,
                           note = "",
                           connection_options = rq_connection_advice(connection),
                           db_methods = rquery_default_methods()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::rquery_db_info")
  force(connection_options)
  if("rquery_db_info" %in% class(connection)) {
    stop("rquery::rquery_db_info connection is already of class rquery_db_info")
  }
  cname <- rq_connection_name(connection)
  if(is_dbi) {
    if(!requireNamespace("DBI", quietly = TRUE)) {
      stop("rquery::rquery_db_info requires the DBI package for is_dbi=TRUE")
    }
  }
  # does not handle quotes inside strings
  r <- list(
    connection = connection,
    cname = cname,
    is_dbi = is_dbi,
    identifier_quote_char = identifier_quote_char,
    string_quote_char = string_quote_char,
    note = note,
    connection_options = connection_options,
    db_methods = db_methods,
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
             gsub(string_quote_char, paste0('\\', string_quote_char), s, fixed=TRUE),
             string_quote_char)
    },
    dbql = function(o) {
      if(is.character(o) || is.factor(o) || is.name(o)) {
        return(paste0(string_quote_char,
                      as.character(o),
                      string_quote_char))
      }
      format(o, scientific = 11)
    })
  if(is_dbi) {
    r$quote_identifier <- function(x, id) {
      connection <- x
      if("rquery_db_info" %in% class(x)) {
        connection <- x$connection
      }
      if(!is.null(connection)) {
        return(as.character(DBI::dbQuoteIdentifier(connection, as.character(id))))
      }
      paste0(identifier_quote_char,
             id,
             identifier_quote_char)
    }
    r$quote_table_name <- function(x, id, ..., qualifiers) {
      wrapr::stop_if_dot_args(substitute(list(...)),
                              "r$quote_table_name")
      connection <- x
      if("rquery_db_info" %in% class(x)) {
        connection <- x$connection
      }
      if(!is.null(connection)) {
        if("schema" %in% names(qualifiers)) {
          dbi_id <- DBI::Id(schema = qualifiers[["schema"]], table = as.character(id))
        } else {
          dbi_id <- as.character(id) # sparklyr '0.8.4' does not implement DBI::dbQuoteIdentifier for DBI::Id
        }
        return(as.character(DBI::dbQuoteIdentifier(connection, dbi_id)))
      }
      paste(paste0(identifier_quote_char,
                   c(qualifiers, id),
                   identifier_quote_char),
            collapse = ".")
    }
    r$quote_string <- function(x, s) {
      connection <- x
      if("rquery_db_info" %in% class(x)) {
        connection <- x$connection
      }
      if(!is.null(connection)) {
        return(as.character(DBI::dbQuoteString(connection, as.character(s))))
      }
      paste0(string_quote_char,
             gsub(string_quote_char, paste0('\\', string_quote_char), s, fixed=TRUE),
             string_quote_char)
    }
    r$quote_literal <- function(x, o) {
      connection <- x
      if("rquery_db_info" %in% class(x)) {
        connection <- x$connection
      }
      if(!is.null(connection)) {
        if(is.character(o) || is.factor(o)) {
          return(as.character(DBI::dbQuoteString(x$connection, as.character(o))))
        }
        return(as.character(DBI::dbQuoteLiteral(x$connection, o)))
      }
      if(is.character(o) || is.factor(o) || is.name(o)) {
        return(paste0(string_quote_char,
                      as.character(o),
                      string_quote_char))
      }
      format(o, scientific = 11)
    }
  }
  r$expr_map <- list(
    "as.Date" = list( # call is 1:as.Date 2:( 3:date_col 4:)
      pre_sql_fn("to_date"),
      pre_sql_token("("),
      3,  # the date column
      pre_sql_token(","),
      pre_sql_string("YYYY-MM-DD"),
      pre_sql_token(")")),
    "n" = list( # call is 1:n 2:( 3:)
      pre_sql_fn("COUNT"),
      pre_sql_token("("),
      pre_sql_token("1"),
      pre_sql_token(")")),
    "mean" = list( # call is 1:n 2:( 3:value 4:)
      pre_sql_fn("AVG"),
      pre_sql_token("("),
      3, # the value column
      pre_sql_token(")")),
    "cumsum" = list( # call is 1:n 2:( 3:value 4:)
      pre_sql_fn("SUM"),
      pre_sql_token("("),
      3, # the value column
      pre_sql_token(")")),
    "shift" = list( # call is 1:n 2:( 3:value 4:)
      pre_sql_fn("LAG"),
      pre_sql_token("("),
      3, # the value column
      pre_sql_token(")"))
  )
  r$tree_rewriter <- tree_rewriter
  # patch in suggested expression mappings
  key <- paste(c("rquery", cname, "expr_map"), collapse = ".")
  expr_map <- connection_options[[key]]
  if(length(expr_map)>0) {
    for(ni in names(expr_map)) {
      r$expr_map[[ni]] <- expr_map[[ni]]
    }
  }
  # patch in user overrides
  for(ni in names(overrides)) {
    r[[ni]] <- overrides[[ni]]
  }
  # declare our class and return value
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

#' An example \code{rquery_db_info} object useful for formatting \code{SQL} without a database connection.
#'
#' @return a rquery_db_info without a connection and vanilla settings.
#'
#' @export
#'
rquery_default_db_info <- function() {
  rquery_db_info(connection = NULL,
                 identifier_quote_char = '"',
                 string_quote_char = "'",
                 is_dbi = FALSE,
                 connection_options = rq_connection_advice(NULL),
                 db_methods = rquery_default_methods())
}


#' Return function mappings for a connection
#'
#' @param db a rquery_db_info
#' @param ... not used, force later arguments to bind by name
#' @param qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @return data.frame of function mappings
#'
#' @export
#'
#' @keywords internal
#'
rq_function_mappings <- function(db,
                                 ...,
                                 qualifiers = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_function_mappings")
  if(!("rquery_db_info" %in% class(db))) {
    stop("rquery::rq_function_mappings db must be of class rq_function_mappings")
  }
  # create zero row data.frame
  mp <- data.frame(fn_name = character(0),
                   sql_mapping = character(0),
                   stringsAsFactors = FALSE)
  # map in function re-writes
  expr_map <- db$expr_map
  if(length(expr_map)>0) {
    emp <- data.frame(fn_name = names(expr_map),
                      stringsAsFactors = FALSE)
    elst <- expr_map
    names(elst) <- NULL
    for(ei in seq_len(length(elst))) {
      elsti <- elst[[ei]]
      elsti <- vapply(elsti,
                      function(elstij) {
                        if("pre_sql" %in% class(elstij)) {
                          pre_sql_to_query(elstij, db, qualifiers = qualifiers)
                        } else {
                          paste0(".(", elstij, ")")
                        }
                      }, character(1))
      elsti <- paste(elsti, collapse = " ")
      elst[[ei]] <- elsti
    }
    emp$sql_mapping <- elst
    mp <- rbind(mp, emp)
  }
  mp
}



#' Quote an identifier.
#'
#' @param x database handle or rquery_db_info object.
#' @param id character to quote
#' @return quoted identifier
#'
#' @export
#'
quote_identifier <- function(x, id) {
  if(!is.null(x)) {
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
  }
  rquery_default_db_info()$dbqi(id)
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
                             qualifiers = character(0)) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::quote_table_name")
  if(!is.null(x)) {
    if("rquery_db_info" %in% class(x)) {
      f <- x$quote_table_name
      if(!is.null(f)) {
        return(f(x, id, qualifiers = qualifiers))
      }
      return(x$dbqt(id, qualifiers = qualifiers))
    }
    if("schema" %in% names(qualifiers)) {
      dbi_id <- DBI::Id(schema = qualifiers[["schema"]], table = as.character(id))
    } else {
      dbi_id <- as.character(id) # sparklyr '0.8.4' does not implement DBI::dbQuoteIdentifier for DBI::Id
    }
    if(requireNamespace("DBI", quietly = TRUE)) {
      return(as.character(DBI::dbQuoteIdentifier(x, dbi_id)))
    }
  }
  rquery_default_db_info()$dbqi(id)
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
  if(!is.null(x)) {
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
  }
  rquery_default_db_info()$dbqs(s)
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
  if(!is.null(x)) {
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
  }
  rquery_default_db_info()$dbql(o)
}


dispatch_to_sql_method <- function(
  method_name,
  x,
  db,
  ...,
  limit = NULL,
  source_limit = NULL,
  indent_level = 0,
  tnum = mk_tmp_name_source('tsql'),
  append_cr = TRUE,
  using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery:::dispatch_to_sql_method")
  if(!("relop" %in% class(x))) {
    stop("rquery::dispatch_to_sql_method, expect x to be of class relop")
  }
  # gaurantee we call the actual methods with an rquery_db_info
  if(!("rquery_db_info" %in% class(db))) {
    connection <- db
    db <- rquery_db_info(connection = connection,
                         identifier_quote_char = '"',
                         string_quote_char = "'",
                         is_dbi = TRUE,
                         db_methods = rquery_default_methods())
  }
  sql_method <- db$db_methods[[method_name]]
  if(is.null(sql_method)) {
    stop(paste("rquery::dispatch_to_sql_method, bad method name:",
               method_name))
  }
  sql_method(
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
}
