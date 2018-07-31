

# The idea is these arrangements of nodes are transient- so they
# can instantiate narrowing from the rel_op nodes.
# Some of these can also be implemented as modifiers on previous nodes
# to try and cut down the amount of query nesting.


#' pre_sql_identifier: abstract name of a column and where it is comming from
#'
#' represents a value from a given sub-table or sub-expression
#'  source is name of source
#'  name is name for term
#'
#' @param column_name character name of column
#' @return pre_sql_identifier
#'
#' @noRd
#'
pre_sql_identifier <- function(column_name) {
  t <- list(token_type = "column",
            column_name = column_name,
            is_zero_argument_call = FALSE)
  class(t) <- "pre_sql_token"
  t
}

#' pre_sql_string
#'
#' represents a string constant
#'   value character string
#'
#' @noRd
#'
pre_sql_string <- function(value) {
  t <- list(token_type = "string",
            value = value,
            is_zero_argument_call = FALSE)
  class(t) <- "pre_sql_token"
  t
}


#' pre_sql_token
#'
#' function name token
#'
#' @param value character, token string
#' @return pre_sql_token class
#'
#' @noRd
#'
pre_sql_fn <- function(value) {
  t <- list(token_type = "function_name",
            value = value,
            is_zero_argument_call = FALSE)
  class(t) <- "pre_sql_token"
  t
}

#' pre_sql_token
#'
#' general token
#'
#' @param value character, token string
#' @return pre_sql_token class
#'
#' @noRd
#'
pre_sql_token <- function(value) {
  t <- list(token_type = "token",
            value = value,
            is_zero_argument_call = FALSE)
  class(t) <- "pre_sql_token"
  t
}

#' pre_sql_expr
#'
#' represents an expression.  Unnamed list of pre_sql_terms and character.
#'
#' @param terms character, term vector
#' @return pre_sql_expr class
#'
#' @noRd
#'
pre_sql_expr <- function(terms) {
  t <- as.list(terms)
  names(t) <- NULL
  class(t) <- c("pre_sql_expr", "pre_sql")
  t
}



#' Return SQL transform of tokens.
#'
#' @param x parsed tokens.
#' @param db_info DBI connnection or rquery_db_info object
#' @param ... generic additional arguments (not used).
#' @param source_table character if not NULL name of source table.
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param using character, if not NULL set of columns used from above.
#' @return SQL command
#'
#' @noRd
#'
to_query <- function (x,
                      db_info,
                      ...,
                      source_table = NULL,
                      source_limit = NA_real_,
                      using = NULL) {
  UseMethod("to_query", x)
}

#' @export
format.pre_sql_token <- function(x, ...) {
  if(x$token_type == "column") {
    return(paste0("'", x$column_name, "'"))
  }
  if(x$token_type == "string") {
    return(paste0('"', paste(as.character(x$value), collapse = " "), '"'))
  }
  paste(as.character(x$value), collapse = " ")
}

#' Convert a pre_sql token object to SQL query text.
#'
#' @param x the pre_sql token
#' @param db_info representation of the database to convert to
#' @param ... force later arguments to be by name
#' @param source_table concrete table for query
#' @param source_limit numeric limit on rows from this source table
#' @param using TBD
#' @return SQL query text
#'
#' @noRd
#'
to_query.pre_sql_token <- function (x,
                                    db_info,
                                    ...,
                                    source_table = NULL,
                                    source_limit = NA_real_,
                                    using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  if((!is.null(x$is_zero_argument_call)) && (x$is_zero_argument_call)) {
    val <- paste(as.character(x$value), collapse = " ")
    zero_arg_fn_map <- getDBOption(db_info, "zero_arg_fn_map")
    if(!is.null(zero_arg_fn_map)) {
      if(val %in% names(zero_arg_fn_map)) {
        xlation <- zero_arg_fn_map[[val]]
        if(!is.null(xlation)) {
          val <- xlation
        }
      }
    }
    return(val)
  }
  if(x$token_type == "column") {
    if((!is.null(source_table)) && (!is.na(source_table))) {
      return(paste(quote_identifier(db_info, source_table),
                   quote_identifier(db_info, x$column_name),
                   sep = '.'))
    } else {
      return(quote_identifier(db_info, x$column_name))
    }
  }
  if(x$token_type == "string") {
    return(quote_string(db_info, paste(as.character(x$value), collapse = " ")))
  }
  if(x$token_type == "function_name") {
    fn_name_map <- getDBOption(db_info, "fn_name_map")
    if(!is.null(fn_name_map)) {
      nm <- paste(as.character(x$value), collapse = " ")
      if(nm %in% names(fn_name_map)) {
        rp <- fn_name_map[[nm]]
        if(!is.null(rp)) {
          return(rp)
        }
      }
    }
  }
  paste(as.character(x$value), collapse = " ")
}

#' Convert a pre_sql expr object to SQL query text.
#'
#' @param x the pre_sql expr
#' @param db_info representation of the database to convert to
#' @param ... force later arguments to be by name
#' @param source_table concrete table for query
#' @param source_limit numeric limit on rows from this source table
#' @param using TBD
#' @return SQL query text
#'
#' @noRd
#'
to_query.pre_sql_expr <- function (x,
                                   db_info,
                                   ...,
                                   source_table = NULL,
                                   source_limit = NA_real_,
                                   using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  terms <- vapply(x,
                  function(ti) {
                    to_query(ti,
                             db_info = db_info,
                             source_table = source_table,
                             source_limit = source_limit,
                             using = using)
                  }, character(1))
  paste(terms, collapse = " ")
}


