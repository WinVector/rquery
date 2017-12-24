


#' pre_sql_identifier: abstract name of a column and where it is comming from
#'
#' represents a value from a given sub-table or sub-expression
#'  source is name of source
#'  name is name for term
#'
#' @param column_name character name of column
#' @param source pre_sql_op where the column is coming from
#' @return pre_sql_identifier
#'
#' @noRd
#'
pre_sql_identifier <- function(column_name, source) {
  t <- list(column_name = column_name,
            source = source)
  class(t) <- c("pre_sql_identifier", "pre_sql")
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
  t <- list(value = value)
  class(t) <- c("pre_sql_string", "pre_sql")
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
#' @param columns character column names
#' @param tablename characer name of table
#'
#' @noRd
#'
pre_sql_table <- function(columns, tablename) {
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

#' @export
#'
to_query.pre_sql_identifier <- function (x,
                                         db,
                                         ...,
                                         source_limit = NULL,
                                         using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  paste0(DBI::dbQuoteIdentifier(x$source), DBI::dbQuoteIdentifier(x$name), sep = '.')
}

#' @export
#'
to_query.pre_sql_string <- function (x,
                                     db,
                                     ...,
                                     source_limit = NULL,
                                     using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  if(length(x$value)<=0) {
    return("NULL")
  }
  DBI::dbQuoteString(db, paste(as.character(x$value), collapse = " "))
}

#' @export
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
                    if("pre_sql" %in% class(ti)) {
                      to_query(ti,
                               db = db,
                               source_limit = source_limit,
                               using = using)
                    } else {
                      paste(as.character(ti), collapse = " ")
                    }
                  }, character(1))
  paste(terms, collapse = " ")
}


values_used <- function(exprs) {
  found <- list()
  for(ei in exprs) {
    for(eij in ei) {
      if("pre_sql_identifier" %in% class(eij)) {
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
    subqi <- list(DBI::dbQuoteIdentifier(db, x$source_table))
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
  exprs <- x$exprs
  if(!is.null(using)) {
    exprs <- x$exprs[using]
  }
  exprq <- vapply(names(exprs),
                  function(ni) {
                    ei <- exprs[[ni]]
                    paste(to_query(ei),
                          "AS",
                          DBI::dbQuoteIdentifier(ni))
                  }, character(1))
  q <- paste0("SELECT \n",
              paste(exprq, collapse = "\n "),
              "FROM\n",
              " ", subqs[[1]])
  if(!is.null(x$where_exprs)) {
    # TODO: implement
    stop("to_query.pre_sq where terms not implemented yet")
  }
  if(!is.null(x$group_terms)) {
    # TODO: implement
    stop("to_query.pre_sq group terms not implemented yet")
  }
  if(!is.null(x$order_terms)) {
    # TODO: implement
    stop("to_query.pre_sq order terms not implemented yet")
  }
  limit <- NA
  if((!is.null(x$limit)) && (!is.null(x$limit))) {
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





