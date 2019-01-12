

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
#' @return pre_sql_token
#'
#' @keywords internal
#'
#' @export
#'
pre_sql_identifier <- function(column_name) {
  t <- list(token_type = "column",
            column_name = column_name,
            is_zero_argument_call = FALSE)
  class(t) <- c("pre_sql_token", "pre_sql")
  t
}

#' pre_sql_string
#'
#' represents a string constant
#'   value character string
#'
#' @param value string
#' @return pre_sql_token
#'
#' @keywords internal
#'
#'
#' @export
#'
pre_sql_string <- function(value) {
  t <- list(token_type = "string",
            value = value,
            is_zero_argument_call = FALSE)
  class(t) <- c("pre_sql_token", "pre_sql")
  t
}


#' pre_sql_token funtion name
#'
#' @param value function tname
#' @return pre_sql_token
#'
#' @param value character, token string
#' @return pre_sql_token
#'
#'
#' @keywords internal
#'
#'
#' @export
#'
pre_sql_fn <- function(value) {
  t <- list(token_type = "function_name",
            value = value,
            is_zero_argument_call = FALSE)
  class(t) <- c("pre_sql_token", "pre_sql")
  t
}

#' pre_sql_token
#'
#' general token
#'
#' @param value character, token string
#' @return pre_sql_token
#'
#' @keywords internal
#'
#'
#' @export
#'
pre_sql_token <- function(value) {
  t <- list(token_type = "token",
            value = value,
            is_zero_argument_call = FALSE)
  class(t) <- c("pre_sql_token", "pre_sql")
  t
}

#' pre_sql_sub_expr
#'
#' represents an expression.  Unnamed list of pre_sql_terms and character.
#'
#' @param terms list of pre_sql tokens
#' @param info named list of extra info with a name slot containing a single string without spaces.
#' @return pre_sql_sub_expr
#'
#' @export
#'
pre_sql_sub_expr <- function(terms, info = NULL) {
  cl <- class(terms)
  if((length(cl)!=1) || (cl!="list")) {
    stop("rquery::pre_sql_sub_expr terms must be a simple list")
  }
  for(ti in terms) {
    if(!("pre_sql" %in% class(ti))) {
      stop("pre_sql_sub_expr all terms must be of class pre_sql")
    }
  }
  t = list(toks = terms, info = info)
  class(t) <- c("pre_sql_sub_expr", "pre_sql")
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
#' @keywords internal
#'
#' @export
#'
pre_sql_to_query <- function (x,
                      db_info,
                      ...,
                      source_table = NULL,
                      source_limit = NA_real_,
                      using = NULL) {
  UseMethod("pre_sql_to_query", x)
}

#' @export
#'
#' @keywords internal
format.pre_sql_token <- function(x, ...) {
  pre_sql_to_query(x, rquery_default_db_info())
}

#' @export
#'
#' @keywords internal
print.pre_sql_token <- function(x, ...) {
  print(format(x))
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
#' @keywords internal
#'
#' @export
#'
pre_sql_to_query.pre_sql_token <- function (x,
                                    db_info,
                                    ...,
                                    source_table = NULL,
                                    source_limit = NA_real_,
                                    using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::pre_sql_to_query.pre_sql_token")
  if("rquery_db_info" %in% class(db_info)) {
    tree_rewriter <- db_info[["tree_rewriter"]]
    if(!is.null(tree_rewriter)) {
      x <- tree_rewriter(x, db_info)
    }
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
#' @keywords internal
#'
#' @export
#'
pre_sql_to_query.pre_sql_sub_expr <- function (x,
                                       db_info,
                                       ...,
                                       source_table = NULL,
                                       source_limit = NA_real_,
                                       using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::pre_sql_to_query.pre_sql_sub_expr")
  if("rquery_db_info" %in% class(db_info)) {
    tree_rewriter <- db_info[["tree_rewriter"]]
    if(!is.null(tree_rewriter)) {
      x <- tree_rewriter(x, db_info)
    }
  }
  terms <- lapply(x$toks,
                  function(ti) {
                    pre_sql_to_query(ti,
                             db_info = db_info,
                             source_table = source_table,
                             source_limit = source_limit,
                             using = using)
                  })
  terms <- as.character(unlist(terms, recursive = TRUE, use.names = FALSE))
  paste(terms, collapse = " ")
}


#' Structure of a pre_sql_sub_expr
#'
#' @param x a pre_sql_sub_expr
#' @return charcter presentation with {} denoting nesting
#'
#' @keywords internal
#'
#' @export
#'
str_pre_sql_sub_expr <- function(x) {
  # process leaf-cases
  if(!("pre_sql_sub_expr" %in% class(x))) {
    return(format(x))
  }
  # get sub-expressions
  subs <- vapply(x$toks, str_pre_sql_sub_expr, character(1))
  # mark
  paste0("{:_",
         x$info$name," ",
         paste(paste0(seq_len(length(subs)), "_{", subs, "}"), collapse = " "),
         " :}")
}

#' @export
#'
#' @keywords internal
#'
format.pre_sql_sub_expr <- function(x, ...) {
  pre_sql_to_query(x, rquery_default_db_info())
}


#' @export
#'
#' @keywords internal
#'
print.pre_sql_sub_expr <- function(x, ...) {
  cat(format(x))
}


