
#' Make a select rows node.
#'
#' @param source source to select from.
#' @param expr expression to select rows.
#' @return select columns node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- select_rows(d, "AUC >= 0.5")
#' print(eqn)
#' sql <- to_sql(eqn, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' @export
#'
select_rows <- function(source, expr) {
  needs <- find_symbols(parse(text=expr))
  have <- column_names(source)
  missing <- setdiff(needs, have)
  if(length(missing)>0) {
    stop(paste("rquery::select_rows missing columns",
               paste(missing, collapse = ", ")))
  }
  r <- list(source = list(source),
            expr = expr)
  class(r) <- "relop_select_rows"
  r
}

#' @export
column_names.relop_select_rows <- function (x, ...) {
  column_names(x$source[[1]])
}

#' @export
format.relop_select_rows <- function(x, ...) {
  paste0(format(x$source[[1]]),
         " %.>% ",
         "select_rows(., ", x$expr, ")")
}

#' @export
print.relop_select_rows <- function(x, ...) {
  print(format(x),...)
}



#' @export
to_sql.relop_select_rows <- function(x,
                                     db,
                                     indent_level = 0,
                                     tnum = cdata::makeTempNameGenerator('tsql'),
                                     append_cr = TRUE,
                                     ...) {
  cols <- vapply(x$columns,
                 function(ci) {
                   DBI::dbQuoteIdentifier(db, ci)
                 }, character(1))
  subsql <- to_sql(x$source[[1]],
                   db = db,
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE)
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT * FROM (\n",
         subsql, "\n",
         prefix, ") ",
         tab, "\n",
         prefix, "WHERE ",
         x$expr)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
