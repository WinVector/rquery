
#' Make a select columns node (not a relational operation).
#'
#' @param source source to select from.
#' @param columns list of distinct column names.
#' @return select columns node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- select_columns(d, 'AUC')
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' @export
#'
select_columns <- function(source, columns) {
  if(length(columns)<=0) {
    stop("rquery::select_columns must select at least 1 column")
  }
  needs <- columns
  have <- column_names(source)
  missing <- setdiff(needs, have)
  if(length(missing)>0) {
    stop(paste("rquery::select_columns missing columns",
               paste(missing, collapse = ", ")))
  }
  r <- list(source = list(source),
            columns = columns)
  class(r) <- "relop_select_columns"
  r
}


#' @export
dbi_connection.relop_select_columns <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  dbi_connection(x$source[[1]])
}

#' @export
column_names.relop_select_columns <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  x$columns
}

#' @export
format.relop_select_columns <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  paste0(format(x$source[[1]]),
         " %.>% ",
         "select_columns(., ", paste(x$columns, collapse = ", "), ")")
}

#' @export
print.relop_select_columns <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  print(format(x),...)
}


#' @export
to_sql.relop_select_columns <- function(x,
                                        indent_level = 0,
                                        tnum = cdata::makeTempNameGenerator('tsql'),
                                        append_cr = TRUE,
                                        ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  db <- dbi_connection(x)
  cols <- vapply(x$columns,
                 function(ci) {
                   DBI::dbQuoteIdentifier(db, ci)
                 }, character(1))
  subsql <- to_sql(x$source[[1]],
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE)
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT\n",
         prefix, " ", paste(cols, collapse = ", "), "\n",
         prefix, "FROM (\n",
         subsql, "\n",
         prefix, ") ",
         tab)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
