

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
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DBI::dbWriteTable(my_db,
#'                   'd',
#'                   data.frame(AUC = 0.6, R2 = 0.2),
#'                   overwrite = TRUE,
#'                   temporary = TRUE)
#' d <- table_source('d',
#'                   columns = c("AUC", "R2"))
#' print(d)
#' sql <- to_sql(d, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
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



#' List fields from a dbi connection
#'
#' @param my_db DBI connection
#' @param tableName character table name
#' @return character list of column names
#'
#' @noRd
#'
listFields <- function(my_db, tableName) {
  # fails intermitnently, and sometimes gives wrong results
  # filed as: https://github.com/tidyverse/dplyr/issues/3204
  # tryCatch(
  #   return(DBI::dbListFields(my_db, tableName)),
  #   error = function(e) { NULL })
  # below is going to have issues to to R-column name conversion!
  q <- paste0("SELECT * FROM ",
              DBI::dbQuoteIdentifier(my_db, tableName),
              " LIMIT 1")
  v <- DBI::dbGetQuery(my_db, q)
  colnames(v)
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
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' DBI::dbWriteTable(my_db,
#'                   'd',
#'                   data.frame(AUC = 0.6, R2 = 0.2),
#'                   overwrite = TRUE,
#'                   temporary = TRUE)
#' d <- dbi_table(my_db, 'd')
#' print(d)
#' sql <- to_sql(d, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' cols <- columns_used(d)
#' print(cols)
#'
#' sql2 <- to_sql(d, my_db, using = "AUC")
#' cat(sql2)
#' DBI::dbGetQuery(my_db, sql2)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
dbi_table <- function(db, table_name) {
  table_source(table_name = table_name,
               columns = listFields(db, table_name))
}


#' @export
tables_used.relop_table_source <- function(node, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  r <- list(node)
  names(r) <- node$table_name
  r
}

#' @export
column_names.relop_table_source <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  x$columns
}


columns_used_relop_table_source <- function (x, ...,
                                             using = NULL,
                                             contract = FALSE) {
  if(length(list(...))>0) {
    stop("columns_used_relop_table_source: unexpected arguemnts")
  }
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
                                       tnum = mkTempNameGenerator('tsql'),
                                       append_cr = TRUE,
                                       using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
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
    q <- paste(q, "LIMIT", source_limit)
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}

#' @export
format.relop_table_source <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  sym <- ""
  if(!is.null(x$data)) {
    sym <- "+"
  }
  paste0("table", sym, "('", x$table_name, "')",
         "\n")
}

#' @export
print.relop_table_source <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  txt <- format(x)
  txt <- trimws(gsub("[ \t\r\n]+", " ", txt), which = "both")
  print(txt, ...)
}


#' Local table to DBI data source.
#'
#' @param db database connection.
#' @param table_name name of table to create.
#' @param d data.frame to copy to database.
#' @param ... force later argument to be by name
#' @param overwrite passed to \code{\link[DBI]{dbWriteTable}}.
#' @param temporary passed to \code{\link[DBI]{dbWriteTable}}.
#' @param rowidcolumn character, name to land row-ids.
#' @return a relop representation of the data
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' sql <- to_sql(d, my_db)
#' cat(sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
dbi_copy_to <- function(db, table_name, d,
                        ...,
                        overwrite = FALSE,
                        temporary = TRUE,
                        rowidcolumn = NULL) {
  if(length(list(...))>0) {
    stop("rquery::dbi_table unexpeced arguments")
  }
  if(!is.null(rowidcolumn)) {
    d[[rowidcolumn]] <- 1:nrow(d)
  }
  if(overwrite) {
    DBI::dbWriteTable(db,
                      table_name,
                      d,
                      overwrite = overwrite,
                      temporary = temporary)
  } else {
    # sparklyr does not take overwrite argument
    DBI::dbWriteTable(db,
                      table_name,
                      d,
                      temporary = temporary)
  }
  dbi_table(db, table_name)
}


#' @export
#'
to_pre_sql.relop_table_source  <- function (x,
                                             ...) {
  if(length(list(...))>0) {
    stop("rquery::to_pre_sqlt.relop_table_source  unexpeced arguments")
  }
  pre_sql_table(x$table_name, x$columns)
}

#' @export
#'
dim.relop_table_source <- function(x) {
  rowcount <- NA
  if(!is.null(x$data)) {
    rowcount <- nrow(x$data)
  }
  c(rowcount, length(column_names(x)))
}


