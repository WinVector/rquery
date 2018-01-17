
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
#' sql <- to_sql(eqn, my_db)
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
  if(is.data.frame(source)) {
    tmp_name <- cdata::makeTempNameGenerator("rquery_tmp")()
    dnode <- table_source(tmp_name, colnames(source))
    dnode$data <- source
    enode <- select_columns(dnode, columns)
    return(enode)
  }
  have <- column_names(source)
  check_have_cols(have, columns, "rquery::select_columns columns")
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            columns = columns)
  r <- relop_decorate("relop_select_columns", r)
  r
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


calc_using_relop_select_columns <- function(x, ...,
                                            using = NULL,
                                            contract = FALSE) {
  cols <- x$columns
  if(length(using)>0) {
    missing <- setdiff(using, x$columns)
    if(length(missing)>0) {
      stop(paste("rquery:columns_used request for unknown columns",
                 paste(missing, collapse = ", ")))
    }
    cols <- intersect(cols, using)
  }
  cols
}

#' @export
columns_used.relop_select_columns <- function (x, ...,
                                               using = NULL,
                                               contract = FALSE) {
  cols <- calc_using_relop_select_columns(x,
                                          using = using,
                                          contract = contract)
  return(columns_used(x$source[[1]],
                      using = cols,
                      contract = contract))
}

#' @export
to_sql.relop_select_columns <- function (x,
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
  using <- calc_using_relop_select_columns(x,
                                           using = using)
  subsql <- to_sql(x$source[[1]],
                   db = db,
                   source_limit = source_limit,
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE,
                   using = using)
  cols <- vapply(x$columns,
                 function(ci) {
                   quote_identifier(db, ci)
                 }, character(1))
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


#' @export
#'
dim.relop_select_columns <- function(x) {
  ncol <- length(column_names(x))
  nrow <- nrow(x$source[[1]])
  c(nrow, ncol)
}


