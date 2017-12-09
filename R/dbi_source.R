
# dbi data source

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
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
dbi_table <- function(db, table_name) {
  columns <- listFields(db, table_name)
  r <- list(columns = columns,
            table_name = table_name,
            db = db)
  class(r) <- "relop_dbi_table"
  r
}


#' @export
quote_identifier.relop_dbi_table <- function (x, id, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  id <- as.character(id)
  if(length(id)!=1) {
    stop("rquery::quote_identifier length(id)!=1")
  }
  DBI::dbQuoteIdentifier(x$db, id)
}

#' @export
quote_string.relop_dbi_table <- function (x, s, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  s <- as.character(s)
  if(length(s)!=1) {
    stop("rquery::quote_string length(s)!=1")
  }
  DBI::dbQuoteString(x$db, s)
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
                                    tnum = mkTempNameGenerator('tsql'),
                                    append_cr = TRUE,
                                    ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  tabnam <- quote_identifier(x,  x$table_name)
  q <- paste0(prefix,
         "SELECT * FROM ",
         tabnam)
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

