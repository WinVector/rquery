
#' Make a select rows node.
#'
#' @param source source to select from.
#' @param expr expression to select rows.
#' @param env environment to look for values in.
#' @return select columns node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- select_rows_se(d, "AUC >= 0.5")
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' @export
#'
select_rows_se <- function(source, expr,
                        env = parent.frame()) {
  have <- column_names(source)
  vnam <- setdiff(paste("rquery_select_condition", 1:(length(have)+1), sep = "_"),
                  have)[[1]]
  parsed <- parse_se(source, vnam := expr, env = env)
  assignments <- unpack_assignments(source, parsed)
  parsed[[1]]$symbols_produced <- character(0)
  r <- list(source = list(source),
            parsed = parsed,
            expr = assignments)
  class(r) <- "relop_select_rows"
  r
}

#' Make a select rows node.
#'
#' @param source source to select from.
#' @param expr expression to select rows.
#' @param env environment to look for values in.
#' @return select columns node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- select_rows_nse(d, AUC >= 0.5)
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' @export
#'
select_rows_nse <- function(source, expr,
                            env = parent.frame()) {
  have <- column_names(source)
  vnam <- setdiff(paste("rquery_select_condition", 1:(length(have)+1), sep = "_"),
                  have)[[1]]
  exprq <- substitute(expr)
  parsed <- parse_nse(source, list(exprq), env = env)
  parsed[[1]]$symbols_produced <- vnam
  assignments <- unpack_assignments(source, parsed)
  parsed[[1]]$symbols_produced <- character(0)
  r <- list(source = list(source),
            parsed = parsed,
            expr = assignments)
  class(r) <- "relop_select_rows"
  r
}


#' @export
dbi_connection.relop_select_rows <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  dbi_connection(x$source[[1]])
}


#' @export
column_names.relop_select_rows <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  column_names(x$source[[1]])
}

#' @export
format.relop_select_rows <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  paste0(format(x$source[[1]]),
         " %.>% ",
         "select_rows(., ", x$parsed[[1]]$presentation, ")")
}

#' @export
print.relop_select_rows <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  print(format(x),...)
}



#' @export
to_sql.relop_select_rows <- function(x,
                                     indent_level = 0,
                                     tnum = mkTempNameGenerator('tsql'),
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
