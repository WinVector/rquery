
#' Quick look at remote data
#'
#' @param my_db DBI database handle
#' @param tableName name of table to look at
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
#'   qlook(my_db, 'd')
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
qlook <- function(my_db, tableName,
                  displayRows = 10,
                  countRows = TRUE) {
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("cdata::qlook requires DBI package")
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
