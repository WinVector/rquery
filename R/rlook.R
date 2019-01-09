
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
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::rstr")
  connection <- my_db
  if("rquery_db_info" %in% class(my_db)) {
    connection <- my_db$connection
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
  q_table_name <- quote_identifier(my_db, tableName)
  h <- rq_get_query(my_db,
                    paste0("SELECT * FROM ",
                           q_table_name,
                           " LIMIT ", displayRows))
  cat(paste('table',
            q_table_name,
            paste(class(my_db), collapse = ' '),
            '\n'))
  if(countRows) {
    nrow <- rq_get_query(my_db,
                         paste0("SELECT COUNT(1) FROM ",
                                q_table_name))[1,1, drop=TRUE]
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
