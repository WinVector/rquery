
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
#' cat(format(eqn))
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
select_columns <- function(source, columns) {
  if(length(columns)<=0) {
    stop("rquery::select_columns must select at least 1 column")
  }
  have <- column_names(source)
  check_have_cols(have, columns, "rquery::select_columns columns")
  r <- list(source = list(source),
            table_name = NULL,
            columns = columns)
  class(r) <- "relop_select_columns"
  r
}


#' @export
quote_identifier.relop_select_columns <- function (x, id, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_identifier(x$source[[1]], id)
}

#' @export
quote_string.relop_select_columns <- function (x, s, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_string(x$source[[1]], s)
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
  paste0(trimws(format(x$source[[1]]), which = "right"),
         " %.>%\n ",
         "select_columns(., ", paste(x$columns, collapse = ", "), ")",
         "\n")
}

#' @export
print.relop_select_columns <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  print(format(x),...)
}

#' @export
columns_used.relop_select_columns <- function (x, ...,
                                               using = NULL,
                                               contract = FALSE) {
  if(length(using)<=0) {
    return(columns_used(x$source[[1]],
                        using = x$columns,
                        contract = contract))
  }
  missing <- setdiff(using, x$columns)
  if(length(missing)>0) {
    stop(paste("rquery::columns_used unknown columns",
               paste0(missing, collapse = ", ")))
  }
  return(columns_used(x$source[[1]],
                      using = using,
                      contract = contract))
}

#' @export
to_sql.relop_select_columns <- function(x,
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
  q <- paste0(prefix, "SELECT\n",
         prefix, " ", paste(cols, collapse = paste0(",\n", prefix, " ")), "\n",
         prefix, "FROM (\n",
         subsql, "\n",
         prefix, ") ",
         tab)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
