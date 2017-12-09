
#' Make an order_by node (not a relational operation).
#'
#'
#' @param source source to select from.
#' @param orderby order by column names.
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
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
order_by <- function(source, orderby) {
  if(length(orderby)<=0) {
    stop("rquery::order_by must have at least one order by term")
  }
  # TODO: add desc argument
  have <- column_names(source)
  check_have_cols(have, orderby, "rquery::order_by orderby")
  r <- list(source = list(source),
            orderby = orderby)
  class(r) <- "relop_order_by"
  r
}


#' @export
quote_identifier.relop_order_by <- function (x, id, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_identifier(x$source[[1]], id)
}

#' @export
quote_string.relop_order_by <- function (x, s, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_string(x$source[[1]], s)
}

#' @export
column_names.relop_order_by <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  column_names(x$source[[1]])
}


#' @export
format.relop_order_by <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  paste0(format(x$source[[1]]),
         " %.>% ",
         "order_by(., ",
         paste(x$orderby, collapse = ", "),
         ifelse(x$desc, " DESC", ""),
         ")")
}

#' @export
print.relop_order_by <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  print(format(x),...)
}




#' @export
to_sql.relop_order_by <- function(x,
                                  indent_level = 0,
                                  tnum = mkTempNameGenerator('tsql'),
                                  append_cr = TRUE,
                                  ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  cols1 <- column_names(x$source[[1]])
  cols <- vapply(cols1,
                 function(ci) {
                   quote_identifier(x, ci)
                 }, character(1))
  ot <- vapply(x$orderby,
               function(ci) {
                 quote_identifier(x, ci)
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
