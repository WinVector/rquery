

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
            column_name = column_name)
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
            value = value)
  class(t) <- "pre_sql_token"
  t
}


#' pre_sql_token
#'
#' represents a string constant
#'   value character string
#'
#' @param value character, token string
#' @return pre_sql_token class
#'
#' @noRd
#'
pre_sql_token <- function(value) {
  t <- list(token_type = "token",
            value = value)
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

# pre_sql_op intermediate terms on the way to constructing a SQL query.
#
# Structure is:
#  ref_name name to refer to this node as.
#  exprs named list of pre_sql_expr s.
#  source_table chracter if not NULL table we are drawing from (not null implies sources is NULL).
#  sources named list of sources.
#  where_exprs list of  pre_sql_expr s.
#  group_terms list of pre_sql_identifier s.
#  order_terms list of pre_sql_identifier s.
#  rev_terms list of pre_sql_identifier s.
#  limit numeric






#' pre_sql_table representation of a table
#'
#' @param tablename characer name of table
#' @param columns character column names
#' @return pre_sql table details
#'
#' @noRd
#'
pre_sql_table <- function(tablename, columns) {
  exprs <- lapply(as.character(columns),
                  function(ci) {
                    pre_sql_expr(list(pre_sql_identifier(ci)))
                  })
  names(exprs) <- as.character(columns)
  t <- list(ref_name = tablename,
            exprs = exprs,
            source_table = tablename,
            sources = NULL,
            where_exprs = list(),
            group_terms = list(),
            order_terms = list(),
            rev_terms = list(),
            limit = NA_real_)

  class(t) <- c("pre_sql_op", "pre_sql")
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
    stop("unexpected arguemnts")
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
    stop("unexpected arguemnts")
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


values_used <- function(exprs) {
  found <- list()
  for(ei in exprs) {
    for(eij in ei) {
      if(eij$token_type == "column") {
        found[[eij$source]] <- unique(c(found[[eij$source]], eij$name))
      }
    }
  }
  found
}


build_subqs <- function(x,
                        db_info,
                        ...,
                        source_table = NULL,
                        source_limit = NA_real_,
                        using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  if(!is.null(x$source_table)) {
    subq <- list(quote_identifier(db_info, x$source_table))
    names(subq) <- x$source_table
  } else {
    exprs <- x$exprs
    if(!is.null(using)) {
      exprs <- x$exprs[using]
    }
    vals_used <- values_used(x$exprs)
    subq <- lapply(names(x$sources),
                  function(ni) {
                    si <- x$sources[[ni]]
                    subqi <- to_query(si,
                                      db_info = db_info,
                                      source_table = source_table,
                                      source_limit = source_limit,
                                      using = vals_used$ni)
                    subqi <- trimws(subqi, which = "right")
                    subqi <- gsub("\n", "\n ", subqi, fixed = TRUE)
                    subqi <- paste0(" ( ", subqi,
                                    " ) ", quote_identifier(db_info, ni))
                    subqi
                  })
    names(subq) <- names(x$sources)
  }
  subq
}


#' Return SQL implementation of operation tree.
#'
#' S3 method so join nodes can override this.
#'
#' @param x pre_sql_op operation tree.
#' @param db_info DBI connnection or rquery_db_info object
#' @param subqs chracter, array of rendered sub-queries
#' @param ... generic additional arguments (not used).
#' @param source_table character if not NULL name of soure table.
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param using character, if not NULL set of columns used from above.
#' @return SQL command
#'
#' @noRd
#'
place_subqs <- function (x,
                         db_info,
                         subqs,
                         ...,
                         source_table = NULL,
                         source_limit = NA_real_,
                         using = NULL) {
  UseMethod("place_subqs", x)
}

#' Convert a pre_sql op object to SQL query text.
#'
#' @param x the pre_sql op
#' @param db_info representation of the database to convert to
#' @param subqs subqueries
#' @param ... force later arguments to be by name
#' @param source_limit numeric limit on rows from this source table
#' @param using TBD
#' @return SQL query text
#'
#' @noRd
#'
place_subqs.pre_sql_op <- function (x,
                                    db_info,
                                    subqs,
                                    ...,
                                    source_limit = NULL,
                                    using = NULL) {
  if(length(subqs)!=1) {
    stop("expected length(subqs)==1")
  }
  subqs[[1]]
}

#' Convert a pre_sql op object to SQL query text.
#'
#' @param x the pre_sql op
#' @param db_info representation of the database to convert to
#' @param ... force later arguments to be by name
#' @param source_table concrete table for query
#' @param source_limit numeric limit on rows from this source table
#' @param using TBD
#' @return SQL query text
#'
#' @noRd
#'
to_query.pre_sql_op <- function (x,
                                 db_info,
                                 ...,
                                 source_table = NULL,
                                 source_limit = NA_real_,
                                 using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  subqs <- build_subqs(x = x,
                       db_info = db_info,
                       source_table = source_table,
                       source_limit = source_limit,
                       using = using)
  subqsq <- place_subqs(x,
                        db_info = db_info,
                        subqs = subqs,
                        source_table = source_table,
                        source_limit = source_limit,
                        using = using)
  exprs <- x$exprs
  if(!is.null(using)) {
    exprs <- x$exprs[using]
  }
  exprq <- vapply(names(exprs),
                  function(ni) {
                    ei <- exprs[[ni]]
                    paste(to_query(ei,
                                   db_info = db_info,
                                   source_table = source_table,
                                   source_limit = source_limit,
                                   using = using),
                          "AS",
                          quote_identifier(db_info, ni))
                  }, character(1))
  q <- paste0("SELECT \n ",
              paste(exprq, collapse = ",\n "),
              "\nFROM\n",
              " ", subqsq)
  if(length(x$where_exprs)>0) {
    # TODO: implement
    stop("to_query.pre_sq where terms not implemented yet")
  }
  if(length(x$group_terms)>0) {
    # TODO: implement
    stop("to_query.pre_sq group terms not implemented yet")
  }
  if(length(x$order_terms)>0) {
    # TODO: implement
    stop("to_query.pre_sq order terms not implemented yet")
  }
  limit <- NA
  if((!is.null(x$limit)) && (!is.na(x$limit))) {
    limit <- min(limit, x$limit, na.rm = TRUE)
  }
  if((!is.null(x$source_table)) && (!is.na(x$source_table)) &&
     (!is.null(source_limit)) && (!is.na(source_limit))) {
    limit <- min(limit, source_limit, na.rm = TRUE)
  }
  if(!is.na(limit)) {
    q <- paste0(q, "\n",
                "LIMIT ", limit)
  }
  q
}



#' Add derived expressions.
#'
#' @param x pre_sql_op node to derived column expressions
#' @param exprs named list of pre_sql_expr
#' @return modified node
#'
#' @noRd
#'
add_exprs <- function(x, exprs) {
  for(ni in names(exprs)) {
    x$exprs[[ni]] <- exprs[[ni]]
  }
  x
}

#' Add where expressions.
#'
#' @param x pre_sql_op node to add where conditions to
#' @param where_exprs list of pre_sql_expr
#' @return modified node
#'
#' @noRd
#'
add_where <- function(x, where_exprs) {
  x$where_exprs <- c(x$where_exprs, where_exprs)
  x
}


#' Add group terms.
#'
#' @param x pre_sql_op node to add where conditions to
#' @param group_terms charater list of grouping terms
#' @return modified node
#'
#' @noRd
#'
add_group_terms <- function(x, group_terms) {
  x$group_terms <- unique(c(x$group_terms, group_terms))
  x
}


#' Add limit condition to a pre_sql_op node.
#'
#' @param x pre_sql_op node to add limit condition to
#' @param limit numeric limit
#' @return modified node
#'
#' @noRd
#'
add_limit <- function(x, limit) {
  x$limit <- min(x$limit, limit, na.rm = TRUE)
  x
}

#' Add order by terms to a pre_sql_op node.
#'
#' @param x pre_sql_op node to add order terms to
#' @param order_terms character, order by terms
#' @param rev_terms character, reverse order by terms
#' @return modified node
#'
#' @noRd
#'
add_orderby <- function(x, order_terms = NULL, rev_terms = NULL) {
  x$order_terms <- unique(c(x$order_terms, order_terms))
  x$rev_terms <- unique(c(x$rev_terms, rev_terms))
  x
}



