


#' Extend data by adding more columns.
#'
#' partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL).
#'
#' @param source source to select from.
#' @param parsed parsed assignment expressions.
#' @param ... force later arguments to bind by name
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (in window function) terms.
#' @param rev_orderby reverse order (in window function)
#' @return extend node.
#'
#'
#' @noRd
#'
extend_impl <- function(source, parsed,
                        ...,
                        partitionby = NULL,
                        orderby = NULL,
                        rev_orderby = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  have <- column_names(source)
  check_have_cols(have, partitionby, "rquery::extend partitionby")
  check_have_cols(have, orderby, "rquery::extend orderby")
  assignments <- unpack_assignments(source, parsed)
  r <- list(source = list(source),
            table_name = NULL,
            parsed = parsed,
            partitionby = partitionby,
            orderby = orderby,
            rev_orderby = rev_orderby,
            assignments = assignments,
            columns = names(assignments))
  r <- relop_decorate("relop_extend", r)
  r
}

#' Extend data by adding more columns list mode (can create multiple nodes)
#'
#' partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL).
#'
#' @param source source to select from.
#' @param parsed parsed assignment expressions.
#' @param ... force later arguments to bind by name
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (in window function) terms.
#' @param rev_orderby reverse ordering (in window function) terms.
#' @return extend node.
#'
#'
#' @noRd
#'
extend_impl_list <- function(source, parsed,
                             ...,
                             partitionby = NULL,
                             orderby = NULL,
                             rev_orderby = NULL) {
  parts <- partition_assignments(parsed)
  ndchain <- source
  for(parti in parts) {
    parsedi <- parsed[parti$origOrder]
    ndchain <- extend_impl(ndchain, parsedi,
                           partitionby = partitionby,
                           orderby = orderby,
                           rev_orderby = rev_orderby)
  }
  ndchain
}



#' Extend data by adding more columns.
#'
#' Create a node similar to a Codd extend relational operator (add derived columns).
#'
#' Partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL).
#'
#' @param source source to select from.
#' @param assignments new column assignment expressions.
#' @param ... force later arguments to bind by name
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (in window function) terms.
#' @param rev_orderby reverse ordering (in window function) terms.
#' @param env environment to look for values in.
#' @return extend node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- extend_se(d, c("v" := "AUC + R2", "x" := "pmax(AUC,v)"))
#' cat(format(eqn))
#' sql <- to_sql(eqn, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' # SQLite can not run the following query
#' eqn2 <- extend_se(d, "v" := "rank()",
#'               partitionby = "AUC", orderby = "R2")
#' sql2 <- to_sql(eqn2, my_db)
#' cat(sql2)
#'
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
extend_se <- function(source, assignments,
                   ...,
                   partitionby = NULL,
                   orderby = NULL,
                   rev_orderby = NULL,
                   env = parent.frame()) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  if(is.data.frame(source)) {
    tmp_name <- cdata::makeTempNameGenerator("rquery_tmp")()
    dnode <- table_source(tmp_name, colnames(source))
    dnode$data <- source
    enode <- extend_se(dnode,
                       assignments = assignments,
                       partitionby = partitionby,
                       orderby = orderby,
                       rev_orderby = rev_orderby,
                       env = env)
    return(enode)
  }
  parsed <- parse_se(source, assignments, env = env)
  extend_impl_list(source = source,
              parsed = parsed,
              partitionby = partitionby,
              orderby = orderby,
              rev_orderby = rev_orderby)
}



#' Extend data by adding more columns.
#'
#' Create a node similar to a Codd extend relational operator (add derived columns).
#'
#' Partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL).
#'
#' @param source source to select from.
#' @param ... new column assignment expressions.
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (in window function) terms.
#' @param rev_orderby reverse ordering (in window function) terms.
#' @param env environment to look for values in.
#' @return extend node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- extend_nse(d, v := ifelse(AUC>0.5, R2, 1.0))
#' cat(format(eqn))
#' sql <- to_sql(eqn, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
extend_nse <- function(source,
                   ...,
                   partitionby = NULL,
                   orderby = NULL,
                   rev_orderby = NULL,
                   env = parent.frame()) {
  if(is.data.frame(source)) {
    tmp_name <- cdata::makeTempNameGenerator("rquery_tmp")()
    dnode <- table_source(tmp_name, colnames(source))
    dnode$data <- source
    enode <- extend_nse(dnode,
                        ...,
                        partitionby = partitionby,
                        orderby = orderby,
                        rev_orderby = rev_orderby,
                        env = env)
    return(enode)
  }
  exprs <-  eval(substitute(alist(...)))
  parsed <- parse_nse(source, exprs, env = env)
  extend_impl_list(source = source,
              parsed = parsed,
              partitionby = partitionby,
              orderby = orderby,
              rev_orderby = rev_orderby)
}


#' @export
column_names.relop_extend <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  sort(unique(c(column_names(x$source[[1]]), x$columns)))
}


#' @export
format.relop_extend <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  pterms <- ""
  if(length(x$partitionby)>0) {
    pterms <- paste0(",\n  p= ",
                     paste(x$partitionb, collapse = ", "))
  }
  oterms <- ""
  ocols <- NULL
  if(length(x$orderby)>0) {
    ocols <- vapply(x$orderby,
                    function(ci) {
                      paste0("\"", ci, "\"")
                    }, character(1))
  }
  if(length(x$rev_orderby)>0) {
    rcols <- vapply(x$rev_orderby,
                    function(ci) {
                      paste0("\"", ci, "\" DESC")
                    }, character(1))
    ocols <- c(ocols, rcols)
  }
  if(length(ocols)>0) {
    oterms <- paste0(",\n  o= ",
      paste(x$orderby, collapse = ", "))
  }
  origTerms <- vapply(x$parsed,
                      function(pi) {
                        paste(as.character(pi$presentation), collapse = ' ')
                      }, character(1))
  aterms <- paste(origTerms, collapse = ",\n  ")
  paste0(trimws(format(x$source[[1]]), which="right"),
         " %.>%\n ",
         "extend(.,\n  ",
         aterms,
         pterms,
         oterms,
         ")",
         "\n")
}



calc_used_relop_extend <- function (x,
                                    using = NULL,
                                    contract = FALSE) {
  cols <- column_names(x)
  if(length(using)>0) {
    cols <- using
  }
  producing <- merge_fld(x$parsed, "symbols_produced")
  expressions <- x$parsed
  if(contract) {
    expressions <- x$parsed[producing %in% cols]
  }
  cols <- setdiff(cols, producing)
  consuming <- merge_fld(expressions, "symbols_used")
  subusing <- unique(c(cols, consuming, x$partitionby, x$orderby))
  subusing
}

#' @export
columns_used.relop_extend <- function (x, ...,
                                       using = NULL,
                                       contract = FALSE) {
  if(length(list(...))>0) {
    stop("rquery:columns_used: unexpected arguemnts")
  }
  cols <- calc_used_relop_extend(x,
                                 using = using,
                                 contract = contract)
  columns_used(x$source[[1]],
               using = cols,
               contract = contract)
}


#' @export
to_sql.relop_extend <- function (x,
                                 db,
                                 ...,
                                 source_limit = NULL,
                                 indent_level = 0,
                                 tnum = mkTempNameGenerator('tsql'),
                                 append_cr = TRUE,
                                 using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  # re-quote expr
  re_quoted <- redo_parse_quoting(x$parsed, db)
  re_assignments <- unpack_assignments(x$source[[1]], re_quoted)
  # work on query
  using <- calc_used_relop_extend(x,
                                  using = using)
  subsql <- to_sql(x$source[[1]],
                   db = db,
                   source_limit = source_limit,
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE,
                   using = using)
  cols1 <- intersect(column_names(x$source[[1]]), using)
  cols1 <- setdiff(cols1, names(re_assignments)) # allow simple name re-use
  cols <- NULL
  if(length(cols1)>0) {
    cols <- vapply(cols1,
                   function(ci) {
                     quote_identifier(db, ci)
                   }, character(1))
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  derived <- NULL
  if(length(re_assignments)>0) {
    windowTerm <- ""
    if((length(x$partitionby)>0) || (length(x$orderby)>0) || (length(x$rev_orderby)>0)) {
      windowTerm <- "OVER ( "
      if(length(x$partitionby)>0) {
        pcols <- vapply(x$partitionby,
                        function(ci) {
                          quote_identifier(db, ci)
                        }, character(1))
        windowTerm <- paste0(windowTerm,
                             " PARTITION BY ",
                             paste(pcols, collapse = ", "))
      }
      ocols <- NULL
      if(length(x$orderby)>0) {
        ocols <- vapply(x$orderby,
                        function(ci) {
                          quote_identifier(db, ci)
                        }, character(1))
      }
      if(length(x$rev_orderby)>0) {
        rcols <- vapply(x$rev_orderby,
                        function(ci) {
                          paste(quote_identifier(db, ci), "DESC")
                        }, character(1))
        ocols <- c(ocols, rcols)
      }
      if(length(ocols)>0) {
        windowTerm <- paste0(windowTerm,
                             " ORDER BY ",
                             paste(ocols, collapse = ", "))
      }
      windowTerm <- paste(windowTerm, ")")
    }
    derived <- vapply(names(re_assignments),
                      function(ni) {
                        ei <- re_assignments[[ni]]
                        paste(ei,
                              windowTerm,
                              "AS", quote_identifier(db, ni))
                      }, character(1))
  }
  tab <- tnum()
  q <- paste0(prefix, "SELECT\n",
         prefix, " ", paste(c(cols, derived), collapse = paste0(",\n", prefix, " ")))
  q <- paste0(q, "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, " ) ", tab)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}

#' @export
#'
dim.relop_extend <- function(x) {
  ncol <- length(column_names(x))
  nrow <- nrow(x$source[[1]])
  c(nrow, ncol)
}
