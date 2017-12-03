



#' Extend data by adding more columns.
#'
#' partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL).
#'
#' @param source source to select from.
#' @param assignments new column assignment expressions.
#' @param ... force later arguments to bind by name
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (window function) terms.
#' @param desc reverse order
#' @return extend node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- extend(d, "v" := "AUC + R2")
#' print(eqn)
#' sql <- to_sql(eqn, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' # SQLite can not run the following query
#' eqn2 <- extend(d, "v" := "rank()",
#'               partitionby = "AUC", orderby = "R2")
#' sql2 <- to_sql(eqn2, my_db)
#' cat(sql2)
#'
#' @export
#'
extend <- function(source, assignments,
                   ...,
                   partitionby = NULL,
                   orderby = NULL,
                   desc = FALSE) {
  if(length(assignments)<=0) {
    stop("rquery::extend must generate at least 1 column")
  }
  if(length(names(assignments))!=length(unique(names(assignments)))) {
    stop("rquery::extend generated column names must be unique")
  }
  syms <- lapply(assignments,
                 function(ai) {
                  find_symbols(parse(text=ai))
                 })
  needs <- unique(c(unlist(syms), partitionby, orderby))
  have <- column_names(source)
  missing <- setdiff(needs, have)
  if(length(missing)>0) {
    stop(paste("rquery::extend missing columns",
               paste(missing, collapse = ", ")))
  }
  gint <- intersect(names(assignments), have)
  if(length(gint)>0) {
    stop(paste("rquery::extend re-used column names:",
               paste(gint, collapse = ", ")))
  }
  r <- list(source = list(source),
            partitionby = partitionby,
            orderby = orderby,
            desc = desc,
            columns = names(assignments),
            assignments = assignments)
  class(r) <- "relop_extend"
  r
}


#' @export
column_names.relop_extend <- function (x, ...) {
  c(column_names(x$source[[1]]), x$columns)
}


#' @export
format.relop_extend <- function(x, ...) {
  pterms <- ""
  if(length(x$partitionby)>0) {
    pterms <- paste0(";p: ",
                     paste(x$partitionb, collapse = ", "))
  }
  oterms <- ""
  if(length(x$orderby)>0) {
    oterms <- paste0(";o: ",
      paste(x$orderby, collapse = ", "),
      ifelse(x$desc, " DESC", ""))
  }
  aterms <- paste(paste(names(x$assignments),
                        ":=",
                        x$assignments), collapse = ", ")
  paste0(format(x$source[[1]]),
         " %.>% ",
         "extend(., ",
         aterms,
         pterms,
         oterms,
         ")")
}

#' @export
print.relop_extend <- function(x, ...) {
  print(format(x),...)
}


#' @export
to_sql.relop_extend <- function(x,
                                db,
                                indent_level = 0,
                                tnum = cdata::makeTempNameGenerator('tsql'),
                                append_cr = TRUE,
                                ...) {
  cols1 <- column_names(x$source[[1]])
  cols <- NULL
  if(length(cols1)>0) {
    cols <- vapply(cols1,
                   function(ci) {
                     DBI::dbQuoteIdentifier(db, ci)
                   }, character(1))
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  derived <- NULL
  if(length(x$assignments)>0) {
    windowTerm <- ""
    if((length(x$partitionby)>0) || (length(x$orderby)>0)) {
      windowTerm <- " OVER ( "
      if(length(x$partitionby)>0) {
        pcols <- vapply(x$partitionby,
                        function(ci) {
                          DBI::dbQuoteIdentifier(db, ci)
                        }, character(1))
        windowTerm <- paste0(windowTerm,
                             " PARTITION BY ",
                             paste(pcols, collapse = ", "))
      }
      if(length(x$orderby)>0) {
        ocols <- vapply(x$orderby,
                        function(ci) {
                          DBI::dbQuoteIdentifier(db, ci)
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
                              "AS", DBI::dbQuoteIdentifier(db, ni))
                      }, character(1))
  }
  subsql <- to_sql(x$source[[1]],
                   db = db,
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE)
  tab <- tnum()
  q <- paste0(prefix, "SELECT\n",
         prefix, " ", paste(c(cols, derived), collapse = paste0(",\n", prefix, " ")))
  q <- paste0(q, "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
