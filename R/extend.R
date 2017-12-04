


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
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- extend_se(d, "v" := "AUC + R2")
#' print(eqn)
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
  n <- length(parsed)
  if(n<=0) {
    stop("rquery::extend_imp must generate at least 1 column")
  }
  nms <-  character(n)
  assignments <- character(n)
  uses <- vector(n, mode='list')
  for(i in 1:n) {
    si <- parsed[[i]]
    if(length(si$symbols_produced)!=1) {
      stop("each assignment must be of the form name := expr")
    }
    nms[[i]] <- si$symbols_produced
    assignments[[i]] <- si$parsed
    uses[[i]] <- si$symbols_used
  }
  names(assignments) <- nms
  if(n!=length(unique(names(assignments)))) {
    stop("rquery::extend_imp generated column names must be unique")
  }
  have <- column_names(source)
  overwritten <- intersect(nms, have)
  if(length(overwritten)>0) {
    stop(paste("rquery::extend_imp overwriting columns:",
               paste(overwritten, collapse = ", ")))
  }
  missing <- setdiff(unlist(uses), have)
  if(length(missing)>0) {
    stop(paste("rquery::extend_imp refering to unknown columns:",
               paste(overwritten, collapse = ", ")))
  }
  r <- list(source = list(source),
            partitionby = partitionby,
            orderby = orderby,
            desc = desc,
            assignments = assignments,
            columns = names(assignments),
            parsed = parsed)
  class(r) <- "relop_extend"
  r
}




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
#' @param env environment to look for values in.
#' @return extend node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- extend_se(d, "v" := "AUC + R2")
#' print(eqn)
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
  n <- length(assignments)
  if(n<=0) {
    stop("rquery::extend_se must generate at least 1 column")
  }
  if(n!=length(unique(names(assignments)))) {
    stop("rquery::extend_se generated column names must be unique")
  }
  have <- column_names(source)
  db <- dbi_connection(source)
  parsed <- vector(n, mode='list')
  for(i in 1:n) {
    ni <- names(assignments)[[i]]
    ai <- assignments[[ni]]
    ei <- parse(text = paste(ni, ":=", ai))[[1]]
    parsed[[i]] <- prepForSQL(ei,
                              colnames = have,
                              db = db,
                              env = env)
  }
  extend_impl(source = source,
              parsed = parsed,
              partitionby = partitionby,
              orderby = orderby,
              desc = desc)
}



#' Extend data by adding more columns.
#'
#' partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL). Note: not sure about string
#' constants at this point.
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
#' eqn <- extend_nse(d, v := AUC + R2)
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
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
  n <- length(exprs)
  if(n<=0) {
    stop("rquery::extend_nse must have at least 1 assigment")
  }
  have <- column_names(source)
  db <- dbi_connection(source)
  parsed <- lapply(exprs,
                   function(ei) {
                     prepForSQL(ei,
                                colnames = have,
                                db = db,
                                env = env)
                   })
  extend_impl(source = source,
              parsed = parsed,
              partitionby = partitionby,
              orderby = orderby,
              desc = desc)
}


#' @export
dbi_connection.relop_extend <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  dbi_connection(x$source[[1]])
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
    pterms <- paste0("; p: ",
                     paste(x$partitionb, collapse = ", "))
  }
  oterms <- ""
  if(length(x$orderby)>0) {
    oterms <- paste0("; o: ",
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
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  print(format(x),...)
}


#' @export
to_sql.relop_extend <- function(x,
                                indent_level = 0,
                                tnum = cdata::makeTempNameGenerator('tsql'),
                                append_cr = TRUE,
                                ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  db <- dbi_connection(x)
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
