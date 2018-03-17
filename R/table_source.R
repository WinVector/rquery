

#' Table data source.
#'
#' Build structures (table name, column names, and quoting
#' strategy) needed to represent data from a remote table.
#'
#' Generate a query that returns contents of a table, we
#' could try to eliminate this (replace the query with the table name),
#' but there are features one can work with with the query in place and
#' SQL optimizers likely make this zero-cost anyway.
#'
#' @param table_name character, name of table
#' @param columns character, column names of table
#' @return a relop representation of the data
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   dbi_copy_to(my_db,
#'              'd',
#'              data.frame(AUC = 0.6, R2 = 0.2),
#'              overwrite = TRUE,
#'              temporary = TRUE)
#'   d <- table_source('d',
#'                     columns = c("AUC", "R2"))
#'   print(d)
#'   sql <- to_sql(d, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @seealso \code{\link{dbi_table}}
#'
#' @export
#'
table_source <- function(table_name, columns) {
  r <- list(source = list(),
            table_name = table_name,
            parsed = NULL,
            columns = columns,
            data = NULL)
  r <- relop_decorate("relop_table_source", r)
  r
}





#' DBI data source.
#'
#' Build structures (table name, column names, and quoting
#' strategy) needed to represent data from a remote table.
#'
#' Generate a query that returns contents of a table, we
#' could try to eliminate this (replace the query with the table name),
#' but there are features one can work with with the query in place and
#' SQL optimizers likely make this zero-cost anyway.
#'
#' @param db database connection
#' @param table_name name of table
#' @return a relop representation of the data
#'
#' @seealso \code{\link{table_source}}
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   dbi_copy_to(my_db,
#'               'd',
#'               data.frame(AUC = 0.6, R2 = 0.2),
#'               overwrite = TRUE,
#'               temporary = TRUE)
#'   d <- dbi_table(my_db, 'd')
#'   print(d)
#'   sql <- to_sql(d, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   cols <- columns_used(d)
#'   print(cols)
#'
#'   sql2 <- to_sql(d, my_db, using = "AUC")
#'   cat(sql2)
#'   print(DBI::dbGetQuery(my_db, sql2))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
dbi_table <- function(db, table_name) {
  table_source(table_name = table_name,
               columns = dbi_colnames(db, table_name))
}


#' @export
tables_used.relop_table_source <- function(node, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::tables_used.relop_table_source")
  node$table_name
}

#' @export
column_names.relop_table_source <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::column_names.relop_table_source")
  x$columns
}


columns_used_relop_table_source <- function (x,
                                             ...,
                                             using = NULL,
                                             contract = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::columns_used_relop_table_source")
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
columns_used.relop_table_source <- function (x, ...,
                                             using = NULL,
                                             contract = FALSE) {
  cols <- columns_used_relop_table_source(x, using = using, contract=contract)
  r <- list(cols)
  names(r) <- x$table_name
  r
}

#' @export
to_sql.relop_table_source <- function (x,
                                       db,
                                       ...,
                                       source_limit = NULL,
                                       indent_level = 0,
                                       tnum = mk_tmp_name_source('tsql'),
                                       append_cr = TRUE,
                                       using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::to_sql.relop_table_source")
  prefix <- paste(rep(' ', indent_level), collapse = '')
  tabnam <- quote_identifier(db,  x$table_name)
  cols <- columns_used_relop_table_source(x, using = using)
  qcols <- vapply(cols,
                  function(ui) {
                    quote_identifier(db, ui)
                  }, character(1))
  qcols <- paste(quote_identifier(db, x$table_name), qcols, sep = '.')
  qt <- paste(qcols, collapse = paste0(",\n", prefix, " "))
  q <- paste0(prefix,
              "SELECT\n",
              prefix, " ", qt, "\n",
              prefix, "FROM\n",
              prefix, " ", tabnam)
  if(!is.null(source_limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(source_limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}

#' @export
format.relop_table_source <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::format.relop_table_source")
  sym <- ""
  if(!is.null(x$data)) {
    sym <- "+"
  }
  paste0("table", sym, "('", x$table_name, "')",
         "\n")
}


