

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
#' @param source pre_sql_op where the column is coming from
#' @param column_name character name of column
#' @return pre_sql_identifier
#'
#' @noRd
#'
pre_sql_identifier <- function(source, column_name) {
  t <- list(token_type = "column",
            column_name = column_name,
            source = source)
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
#  desc logical
#  limit numeric






#' pre_sql_table representation of a table
#'
#' @param tablename characer name of table
#' @param columns character column names
#'
#' @noRd
#'
pre_sql_table <- function(tablename, columns) {
  exprs <- lapply(as.character(columns),
                  function(ci) {
                    pre_sql_expr(list(pre_sql_identifier(tablename, ci)))
                  })
  names(exprs) <- as.character(columns)
  t <- list(ref_name = tablename,
            exprs = exprs,
            source_table = tablename,
            sources = NULL,
            where_exprs = list(),
            group_terms = list(),
            order_terms = list(),
            desc = FALSE,
            limit = NA_real_)

  class(t) <- c("pre_sql_op", "pre_sql")
  t
}


#' Return SQL implementation of operation tree.
#'
#' @param x pre_sql_op operation tree.
#' @param db database handle.
#' @param ... generic additional arguments (not used).
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param using character, if not NULL set of columns used from above.
#' @return SQL command
#'
#' @export
#'
to_query <- function (x,
                      db,
                      ...,
                      source_limit = NA_real_,
                      using = NULL) {
  UseMethod("to_query", x)
}

#' @noRd
#'
to_query.pre_sql_token <- function (x,
                                    db,
                                    ...,
                                    source_limit = NULL,
                                    using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  if(x$token_type == "column") {
    return(paste(DBI::dbQuoteIdentifier(db, x$source),
                 DBI::dbQuoteIdentifier(db, x$column_name),
                 sep = '.'))
  }
  if(x$token_type == "string") {
    return(DBI::dbQuoteString(db, paste(as.character(x$value), collapse = " ")))
  }
  paste(as.character(x$value), collapse = " ")
}

#' @export
format.pre_sql_token <- function(x, ...) {
  if(x$token_type == "column") {
    return(paste0("'", x$source, "'.'",
                 x$column_name, "'"))
  }
  if(x$token_type == "string") {
    return(paste0('"', paste(as.character(x$value), collapse = " "), '"'))
  }
  paste(as.character(x$value), collapse = " ")
}

#' @noRd
#'
to_query.pre_sql_expr <- function (x,
                                   db,
                                   ...,
                                   source_limit = NULL,
                                   using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  terms <- vapply(x,
                  function(ti) {
                    to_query(ti,
                             db = db,
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
                        db,
                        ...,
                        source_limit = NULL,
                        using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  if(!is.null(x$source_table)) {
    subq <- list(DBI::dbQuoteIdentifier(db, x$source_table))
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
                                      db = db,
                                      source_limit = source_limit,
                                      using = vals_used$ni)
                    subqi <- trimws(subqi, which = "right")
                    subqi <- gsub("\n", "\n ", subqi, fixed = TRUE)
                    subqi <- paste0(" ( ", subqi,
                                    " ) ", DBI::dbQuoteIdentifier(db, ni))
                    subqi
                  })
    names(subq) <- names(x$sources)
  }
  subq
}


#' Return SQL implementation of operation tree.
#'
#' @param x pre_sql_op operation tree.
#' @param db database handle.
#' @param subqs chracter, array of rendered sub-queries
#' @param ... generic additional arguments (not used).
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param using character, if not NULL set of columns used from above.
#' @return SQL command
#'
#' @noRd
#'
place_subqs <- function (x,
                         db,
                         subqs,
                         ...,
                         source_limit = NA_real_,
                         using = NULL) {
  UseMethod("place_subqs", x)
}

#' @noRd
#'
place_subqs.pre_sql_op <- function (x,
                                    db,
                                    subqs,
                                    ...,
                                    source_limit = NULL,
                                    using = NULL) {
  if(length(subqs)!=1) {
    stop("expected length(subqs)==1")
  }
  subqs[[1]]
}


#' @export
#'
to_query.pre_sql_op <- function (x,
                                 db,
                                 ...,
                                 source_limit = NULL,
                                 using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  subqs <- build_subqs(x = x,
                       db = db,
                       source_limit = source_limit,
                       using = using)
  subqsq <- place_subqs(x,
                        db = db,
                        subqs = subqs,
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
                                   db = db,
                                   source_limit = source_limit,
                                   using = using),
                          "AS",
                          DBI::dbQuoteIdentifier(db, ni))
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
#' @param desc logical set/clear desc
#' @return modified node
#'
#' @noRd
#'
add_order_by <- function(x, order_terms, desc = NA) {
  x$order_terms <- unique(c(x$order_terms, order_terms))
  if((!is.null(desc)) && (!is.na(desc))) {
    x$desc = desc
  }
  x
}



