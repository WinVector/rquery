
#' Make a natural_join node.
#'
#' @param a source to select from.
#' @param b source to select from.
#' @param jointype type of join ('INNER', 'LEFT', 'RIGHT', 'FULL').
#' @return natural_join node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d1 <- dbi_copy_to(my_db, 'd1',
#'                  data.frame(AUC = 0.6, R2 = 0.2))
#' d2 <- dbi_copy_to(my_db, 'd2',
#'                  data.frame(AUC = 0.6, D = 0.3))
#' eqn <- natural_join(d1, d2)
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' @export
#'
natural_join <- function(a, b,
                         jointype = 'INNER') {
  usesa <- column_names(a)
  usesb <- column_names(b)
  by = intersect(usesa, usesb)
  r <- list(source = list(a, b),
            by = by,
            jointype = jointype)
  class(r) <- "relop_natural_join"
  r
}

#' @export
dbi_connection.relop_natural_join <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  dbi_connection(x$source[[1]])
}


#' @export
column_names.relop_natural_join <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  unique(c(column_names(x$source[[1]]),
           column_names(x$source[[2]])))
}


#' @export
format.relop_natural_join <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  a <- format(x$source[[1]])
  b <- format(x$source[[2]])
  paste0(a,
         " %.>% ",
         "natural_join(., ",
         b,
         "; ",
         x$jointype,
         "; by ",
         paste(x$by, collapse = ", "),
         ")")
}

#' @export
print.relop_natural_join <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  print(format(x),...)
}



#' @export
to_sql.relop_natural_join <- function(x,
                                      indent_level = 0,
                                      tnum = cdata::makeTempNameGenerator('tsql'),
                                      append_cr = TRUE,
                                      ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  db <- dbi_connection(x)
  subsqla <- to_sql(x$source[[1]],
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE)
  subsqlb <- to_sql(x$source[[2]],
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE)
  taba <- tnum()
  tabb <- tnum()
  bexpr <- NULL
  bterms <- setdiff(column_names(x$source[[1]]),
                    column_names(x$source[[2]]))
  if(length(bterms)>0) {
    bcols <- vapply(bterms,
                   function(ci) {
                     DBI::dbQuoteIdentifier(db, ci)
                   }, character(1))
    bexpr <- paste(",",
                   paste(bcols, collapse = ", "))
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT ",
         taba,
         ".*",
         bexpr,
         " FROM (\n",
         subsqla, "\n",
         prefix, ") ",
         taba, "\n",
         prefix, x$jointype,
         " JOIN (\n",
         subsqlb, "\n",
         prefix, ") ",
         tabb)
  if(length(x$by)>0) {
    bt <- vapply(x$by,
                 function(ci) {
                   DBI::dbQuoteIdentifier(db, ci)
                 }, character(1))
    mt <- paste(paste(paste(taba, bt, sep='.'),
                      paste(tabb, bt, sep='.'), sep = ' = '),
                collapse = ' AND ')
    q <- paste0(q, "\n",
                prefix, "ON\n",
                prefix, " ", mt)
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
