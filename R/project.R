


#' project data by grouping, summarizing, and adding more columns.
#'
#' TODO: re-factor to look like extend().
#'
#' @param source source to select from.
#' @param groupby grouping columns.
#' @param assignments new column assignment expressions.
#' @return project node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- project(d, "AUC", "v" := "max(R2)")
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' @export
#'
project <- function(source, groupby, assignments) {
  if(length(assignments)<=0) {
    stop("rquery::project must generate at least 1 column")
  }
  if(length(assignments)!=length(unique(assignments))) {
    stop("rquery::project generated column names must be unique")
  }
  r <- list(source = list(source),
            groupby = groupby,
            columns = c(groupby, names(assignments)),
            assignments = assignments)
  class(r) <- "relop_project"
  r
}

#' @export
dbi_connection.relop_project <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  dbi_connection(x$source[[1]])
}


#' @export
column_names.relop_project <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  x$columns
}


#' @export
format.relop_project <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  aterms <- paste(paste(names(x$assignments),
                        ":=",
                        x$assignments), collapse = ", ")
  paste0(format(x$source[[1]]),
         " %.>% ",
         "project(., ",
         aterms,
         ";g ",
         paste(x$groupby, collapse = ", "),
         ")")
}

#' @export
print.relop_project <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  print(format(x),...)
}


#' @export
to_sql.relop_project <- function(x,
                                 indent_level = 0,
                                 tnum = cdata::makeTempNameGenerator('tsql'),
                                 append_cr = TRUE,
                                 ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  db <- dbi_connection(x)
  cols1 <- x$groupby
  cols <- NULL
  if(length(cols1)>0) {
    cols <- vapply(cols1,
                   function(ci) {
                     DBI::dbQuoteIdentifier(db, ci)
                   }, character(1))
  }
  derived <- NULL
  if(length(x$assignments)>0) {
    derived <- vapply(names(x$assignments),
                      function(ni) {
                        ei <- x$assignments[[ni]]
                        paste(ei, "AS", DBI::dbQuoteIdentifier(db, ni))
                      }, character(1))
  }
  subsql <- to_sql(x$source[[1]],
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE)
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT ",
              paste(c(cols, derived), collapse = ", "),
              " FROM (\n",
              subsql,
              " ) ", tab)
  if(length(cols)>0) {
    q <- paste0(q,
               "\n",
               prefix, "GROUP BY\n",
               prefix, " ", paste(cols, collapse = " AND "))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
