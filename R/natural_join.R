
#' Make a natural_join node.
#'
#' Natural join is a join by identity on all common columns.
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
#' cat(format(eqn))
#' sql <- to_sql(eqn, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
natural_join <- function(a, b,
                         jointype = 'INNER') {
  usesa <- column_names(a)
  usesb <- column_names(b)
  by = intersect(usesa, usesb)
  r <- list(source = list(a, b),
            table_name = NULL,
            parsed = NULL,
            by = by,
            jointype = jointype)
  class(r) <- c("relop_natural_join", "relop")
  r
}



#' @export
format.relop_natural_join <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  a <- trimws(format(x$source[[1]]), which = "right")
  b <- trimws(format(x$source[[2]]), which = "right")
  b <- gsub("\n", "\n  ", b, fixed = TRUE)
  paste0(a,
         " %.>%\n ",
         "natural_join(.,\n",
         "  ", b, ",\n",
         "  j= ",
         x$jointype,
         ", by= ",
         paste(x$by, collapse = ", "),
         ")",
         "\n")
}



calc_used_relop_natural_join <- function (x, ...,
                                          using = NULL,
                                          contract = FALSE) {
  cols <- column_names(x)
  if(length(using)>0) {
    missing <- setdiff(using, cols)
    if(length(missing)>0) {
      stop(paste("rquery::calc_used_relop_natural_join unkown columns",
                 paste(missing, collapse = ", ")))

    }
    cols <- using
  }
  cols <- unique(c(cols, x$by))
  cols
}

#' @export
columns_used.relop_natural_join <- function (x, ...,
                                             using = NULL,
                                             contract = FALSE) {
  using <- calc_used_relop_natural_join(x,
                                        using=using,
                                        contract = contract)
  c1 <- intersect(using, column_names(x$source[[1]]))
  s1 <- columns_used(x$source[[1]],
                     using = c1,
                     contract = contract)
  c2 <- intersect(using, column_names(x$source[[2]]))
  s2 <- columns_used(x$source[[2]],
                     using = c2,
                     contract = contract)
  merge_columns_used(s1, s2)
}


#' @export
to_sql.relop_natural_join <- function (x,
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
  using <- calc_used_relop_natural_join(x,
                                        using=using)
  c1 <- intersect(using, column_names(x$source[[1]]))
  subsqla <- to_sql(x$source[[1]],
                    db = db,
                    source_limit = source_limit,
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE,
                    using = c1)
  c2 <- intersect(using, column_names(x$source[[2]]))
  subsqlb <- to_sql(x$source[[2]],
                    db = db,
                    source_limit = source_limit,
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE,
                    using = c2)
  taba <- tnum()
  tabb <- tnum()
  bexpr <- NULL
  bterms <- setdiff(column_names(x$source[[2]]),
                    column_names(x$source[[1]]))
  if(length(bterms)>0) {
    bcols <- vapply(bterms,
                   function(ci) {
                     quote_identifier(db, ci)
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
                   quote_identifier(db, ci)
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
