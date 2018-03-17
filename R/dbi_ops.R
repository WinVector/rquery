

# work around common not fully DBI databases issues

#' List fields from a dbi connection
#'
#' @param my_db DBI connection
#' @param tableName character table name
#' @return character list of column names
#'
#' @export
#'
dbi_colnames <- function(my_db, tableName) {
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


#' Remove table
#'
#' @param db database connection.
#' @param table_name character, name of table to create.
#' @return logical TRUE if table existed, else FALSE
#'
#' @export
#'
dbi_remove_table <- function(db, table_name) {
  if(!is.null(table_name)) {
    if(DBI::dbExistsTable(db, table_name)) {
      DBI::dbExecute(db,
                     paste0("DROP TABLE ",
                            quote_identifier(db, table_name)))
      # Could also try  DBI::dbRemoveTable(db, table_name)
      return(TRUE)
    }
  }
  return(FALSE)
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
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- dbi_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'
#'   sql <- to_sql(d, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, "SELECT * FROM d"))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
dbi_copy_to <- function(db, table_name, d,
                        ...,
                        overwrite = FALSE,
                        temporary = TRUE,
                        rowidcolumn = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::dbi_copy_to")
  if(!is.null(rowidcolumn)) {
    d[[rowidcolumn]] <- seq_len(nrow(d))
  }
  # sparklyr 0.7.0 does not take overwrite or row.names arguments
  if(overwrite) {
    dbi_remove_table(db, table_name)
  }
  is_spark <- length(intersect(c("spark_connection", "spark_shell_connection"),
                               class(db)))>=1
  if(is_spark) {
    DBI::dbWriteTable(db,
                      table_name,
                      d,
                      temporary = temporary,
                      append = FALSE)
  } else {
    DBI::dbWriteTable(db,
                      table_name,
                      d,
                      temporary = temporary,
                      append = FALSE,
                      overwrite = overwrite,
                      row.names = FALSE)
  }
  dbi_table(db, table_name)
}

#' Count rows and return as numeric
#'
#' @param db database connetion
#' @param table_name character, name of table
#' @return numeric row count
#'
#' @export
#'
dbi_nrow <- function(db, table_name) {
  nrowst <- DBI::dbGetQuery(
    db,
    paste0("SELECT COUNT(1) FROM ",
           DBI::dbQuoteIdentifier(db,
                                  table_name)))
  # integer64 was coming back from RPostgres
  # and that does not work as numeric in pmin()
  nrows <- as.numeric(nrowst[[1]][[1]])
  nrows
}
