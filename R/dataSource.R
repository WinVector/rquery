
# basic data sources

#' DBI data source.
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
#' sql <- to_sql(d)
#' cat(sql)
#'
#' @export
#'
dbi_table <- function(db, table_name) {
  columns <- cdata:::listFields(db, table_name)
  r <- list(columns = columns,
            table_name = table_name,
            db = db)
  class(r) <- "relop_dbi_table"
  r
}


#' @export
dbi_connection.relop_dbi_table <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  x$db
}

#' @export
column_names.relop_dbi_table <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  x$columns
}

#' @export
to_sql.relop_dbi_table <- function (x,
                                    indent_level = 0,
                                    tnum = cdata::makeTempNameGenerator('tsql'),
                                    append_cr = TRUE,
                                    ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  db <- dbi_connection(x)
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix,
         "SELECT * FROM ",
         DBI::dbQuoteIdentifier(db, x$table_name))
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}

#' @export
format.relop_dbi_table <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  paste0("dbi_table('", x$table_name, "')")
}

#' @export
print.relop_dbi_table <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  print(format(x),...)
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
#' sql <- to_sql(d)
#' cat(sql)
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

