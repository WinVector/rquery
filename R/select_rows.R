
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
#' cat(format(eqn))
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
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
            table_name = NULL,
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
#' cat(format(eqn))
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
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
quote_identifier.relop_select_rows <- function (x, id, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_identifier(x$source[[1]], id)
}

#' @export
quote_string.relop_select_rows <- function (x, s, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_string(x$source[[1]], s)
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
  paste0(trimws(format(x$source[[1]]), which="right"),
         " %.>%\n ",
         "select_rows(., ", x$parsed[[1]]$presentation, ")",
         "\n")
}

#' @export
print.relop_select_rows <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  txt <- format(x)
  txt <- trimws(gsub("[ \t\r\n]+", " ", txt), which = "both")
  print(txt, ...)
}

#' @export
columns_used.relop_select_rows <- function (x, ...,
                                         using = NULL,
                                         contract = FALSE) {
  if(length(using)<=0) {
    return(columns_used(x$source[[1]],
                        using = NULL,
                        contract = contract))
  }
  consuming <- merge_fld(x$parsed, "symbols_used")
  return(columns_used(x$source[[1]],
                      using = unique(c(using, consuming)),
                      contract = contract))
}


#' @export
to_sql.relop_select_rows <- function(x,
                                     ...,
                                     indent_level = 0,
                                     tnum = mkTempNameGenerator('tsql'),
                                     append_cr = TRUE,
                                     column_restriction = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  cols <- vapply(x$columns,
                 function(ci) {
                   quote_identifier(x, ci)
                 }, character(1))
  subsql <- to_sql(x$source[[1]],
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE,
                   column_restriction = column_restriction)
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
