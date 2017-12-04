
#' Make an order_by node (not a relational operation).
#'
#' @param source source to select from.
#' @param orderby order by names.
#' @return select columns node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- order_by(d, "AUC")
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' @export
#'
order_by <- function(source, orderby) {
  if(length(orderby)<=0) {
    stop("rquery::order_by must have at least one order by term")
  }
  have <- column_names(source)
  missing <- setdiff(orderby, have)
  if(length(missing)>0) {
    stop(paste("rquery::order_by missing columns",
               paste(missing, collapse = ", ")))
  }
  r <- list(source = list(source),
            orderby = orderby)
  class(r) <- "relop_order_by"
  r
}


#' @export
dbi_connection.relop_order_by <- function (x, ...) {
  dbi_connection(x$source[[1]])
}

#' @export
column_names.relop_order_by <- function (x, ...) {
  column_names(x$source[[1]])
}


#' @export
format.relop_order_by <- function(x, ...) {
  paste0(format(x$source[[1]]),
         " %.>% ",
         "order_by(., ",
         paste(x$orderby, collapse = ", "),
         ifelse(x$desc, " DESC", ""),
         ")")
}

#' @export
print.relop_order_by <- function(x, ...) {
  print(format(x),...)
}




#' @export
to_sql.relop_order_by <- function(x,
                                  indent_level = 0,
                                  tnum = cdata::makeTempNameGenerator('tsql'),
                                  append_cr = TRUE,
                                  ...) {
  db <- dbi_connection(x)
  cols1 <- column_names(x$source[[1]])
  cols <- vapply(cols1,
                 function(ci) {
                   DBI::dbQuoteIdentifier(db, ci)
                 }, character(1))
  ot <- vapply(x$orderby,
               function(ci) {
                 DBI::dbQuoteIdentifier(db, ci)
               }, character(1))
  subsql <- to_sql(x$source[[1]],
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE)
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT * FROM (\n",
         subsql, "\n",
         prefix, ") ",
         tab,
         " ORDER BY ",
         paste(ot, collapse = ", "),
         ifelse(x$desc, " DESC", ""))
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
