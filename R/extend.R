


#' Extend data by adding more columns.
#'
#' partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL).
#'
#' @param source source to select from.
#' @param parsed parsed assignment expressions.
#' @param ... force later arguments to bind by name
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (window function) terms.
#' @param desc reverse order
#' @return extend node.
#'
#'
#' @noRd
#'
extend_impl <- function(source, parsed,
                        ...,
                        partitionby = NULL,
                        orderby = NULL,
                        desc = FALSE) {
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
            desc = desc,
            assignments = assignments,
            columns = names(assignments))
  class(r) <- c("relop_extend", "relop")
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
#' @param orderby ordering (window function) terms.
#' @param desc reverse order
#' @return extend node.
#'
#'
#' @noRd
#'
extend_impl_list <- function(source, parsed,
                             ...,
                             partitionby = NULL,
                             orderby = NULL,
                             desc = FALSE) {
  parts <- partition_assignments(parsed)
  ndchain <- source
  for(parti in parts) {
    parsedi <- parti$parsed[seq_len(nrow(parti))]
    ndchain <- extend_impl(ndchain, parsedi,
                           partitionby = partitionby,
                           orderby = orderby,
                           desc = desc)
  }
  ndchain
}



#' Extend data by adding more columns.
#'
#' Create a node similar to a Codd extend relational operator (add derived columns).
#'
#' Allows re-use of column names in the spirit of \code{\link[seplyr]{partition_mutate_se}},
#' though re-use can break relational property.
#' partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL).
#'
#' @param source source to select from.
#' @param assignments new column assignment expressions.
#' @param ... force later arguments to bind by name
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (window function) terms.
#' @param desc reverse order
#' @param env environment to look for values in.
#' @return extend node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- extend_se(d, c("v" := "AUC + R2", "x" := "max(AUC,v)"))
#' cat(format(eqn))
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' # SQLite can not run the following query
#' eqn2 <- extend_se(d, "v" := "rank()",
#'               partitionby = "AUC", orderby = "R2")
#' sql2 <- to_sql(eqn2)
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
                   desc = FALSE,
                   env = parent.frame()) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  parsed <- parse_se(source, assignments, env = env)
  extend_impl_list(source = source,
              parsed = parsed,
              partitionby = partitionby,
              orderby = orderby,
              desc = desc)
}



#' Extend data by adding more columns.
#'
#' Create a node similar to a Codd extend relational operator (add derived columns).
#'
#' Allows re-use of column names in the spirit of \code{\link[seplyr]{partition_mutate_qt}},
#' though re-use can break relational property.
#' partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL).
#'
#' @param source source to select from.
#' @param ... new column assignment expressions.
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (window function) terms.
#' @param desc reverse order
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
#' sql <- to_sql(eqn)
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
                   desc = FALSE,
                   env = parent.frame()) {
  exprs <-  eval(substitute(alist(...)))
  parsed <- parse_nse(source, exprs, env = env)
  extend_impl_list(source = source,
              parsed = parsed,
              partitionby = partitionby,
              orderby = orderby,
              desc = desc)
}


#' @export
column_names.relop_extend <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  c(column_names(x$source[[1]]), x$columns)
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
  if(length(x$orderby)>0) {
    oterms <- paste0(",\n  o= ",
      paste(x$orderby, collapse = ", "),
      ifelse(x$desc, " DESC", ""))
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
  if(length(using)>=0) {
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
                                 ...,
                                 indent_level = 0,
                                 tnum = mkTempNameGenerator('tsql'),
                                 append_cr = TRUE,
                                 using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  using <- calc_used_relop_extend(x,
                                  using = using)
  subsql <- to_sql(x$source[[1]],
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE,
                   using = using)
  cols1 <- intersect(column_names(x$source[[1]]), using)
  cols1 <- setdiff(cols1, names(x$assignments)) # allow simple name re-use
  cols <- NULL
  if(length(cols1)>0) {
    cols <- vapply(cols1,
                   function(ci) {
                     quote_identifier(x, ci)
                   }, character(1))
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  derived <- NULL
  if(length(x$assignments)>0) {
    windowTerm <- ""
    if((length(x$partitionby)>0) || (length(x$orderby)>0)) {
      windowTerm <- "OVER ( "
      if(length(x$partitionby)>0) {
        pcols <- vapply(x$partitionby,
                        function(ci) {
                          quote_identifier(x, ci)
                        }, character(1))
        windowTerm <- paste0(windowTerm,
                             " PARTITION BY ",
                             paste(pcols, collapse = ", "))
      }
      if(length(x$orderby)>0) {
        ocols <- vapply(x$orderby,
                        function(ci) {
                          quote_identifier(x, ci)
                        }, character(1))
        windowTerm <- paste0(windowTerm,
                             " ORDER BY ",
                             paste(ocols, collapse = ", "))
        if(x$desc) {
          windowTerm <- paste(windowTerm, "DESC")
        }
      }
      windowTerm <- paste(windowTerm, ")")
    }
    derived <- vapply(names(x$assignments),
                      function(ni) {
                        ei <- x$assignments[[ni]]
                        paste(ei,
                              windowTerm,
                              "AS", quote_identifier(x, ni))
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
