
#' Quick look at remote data
#'
#' @param my_db database handle
#' @param tableName name of table to look at
#' @param ... not used, force later arguments to bind by name
#' @param displayRows number of rows to sample
#' @param countRows logical, if TRUE return row count.
#' @return str view of data
#'
#' @examples
#'
#' if ( requireNamespace("DBI", quietly = TRUE) &&
#'   requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(my_db,
#'                     'd',
#'                     data.frame(AUC = 0.6, R2 = 0.2),
#'                     overwrite = TRUE,
#'                     temporary = TRUE)
#'   rlook(my_db, 'd')
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
rstr <- function(my_db, tableName,
                 ...,
                 displayRows = 10,
                 countRows = TRUE) {
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery::rstr requires DBI package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::rstr")
  if("rquery_db_info" %in% class(my_db)) {
    my_db <- my_db$connection
  }
  if("relop_table_source" %in% class(tableName)) {
    tableName <- tableName$table_name
  }
  if(!is.character(tableName)) {
    stop("rquery::rstr tableName must be scalar string or relop_table_source")
  }
  if(length(tableName)!=1) {
    stop("rquery::rstr tableName must be scalar string or relop_table_source")
  }
  h <- DBI::dbGetQuery(my_db,
                       paste0("SELECT * FROM ",
                              DBI::dbQuoteIdentifier(my_db, tableName),
                              " LIMIT ", displayRows))
  cat(paste('table',
            DBI::dbQuoteIdentifier(my_db, tableName),
            paste(class(my_db), collapse = ' '),
            '\n'))
  if(countRows) {
    nrow <- DBI::dbGetQuery(my_db,
                            paste0("SELECT COUNT(1) FROM ",
                                   DBI::dbQuoteIdentifier(my_db, tableName)))[1,1, drop=TRUE]
    nrow <- as.numeric(nrow) # defend against Rpostgres integer64
    cat(paste(" nrow:", nrow, '\n'))
    if(nrow>displayRows) {
      cat(" NOTE: \"obs\" below is count of sample, not number of rows of data.\n")
    }
  } else {
    cat(" NOTE: \"obs\" below is count of sample, not number of rows of data.\n")
  }
  utils::str(h, list.len = length(h))
  invisible(NULL)
}

#' @rdname rstr
#' @export
rlook <- rstr
