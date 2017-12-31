

#' Execture node_tree in an enviroment where d is the only data.
#'
#' Default DB uses RSQLite (so some functions are not supported).
#' Functionality is through \code{"wrapr_applicable"}: \url{https://winvector.github.io/wrapr/articles/wrapr_applicable.html}.
#'
#' @param pipe_left_arg data.frame
#' @param pipe_right_arg rquery rel_op operation tree.
#' @param pipe_environment environment to look for "winvector_temp_db_handle" in.
#' @return data.frame result
#'
#' @examples
#'
#' winvector_temp_db_handle <- list(
#'   db = DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' )
#' RSQLite::initExtension(winvector_temp_db_handle$db)
#'
#' d <- data.frame(AUC = 0.6, R2 = 0.2, D = NA, z = 2)
#' q <- table_source("d", c("AUC", "R2", "D")) %.>%
#' 	extend_nse(., c := sqrt(R2))
#' rquery_apply_to_data_frame(d, q)
#' # # with wrapr version 1.1.0 or greater:
#' # d %.>% q
#'
#' DBI::dbDisconnect(winvector_temp_db_handle$db)
#'
#' @export
#'
rquery_apply_to_data_frame <- function(pipe_left_arg,
                                       pipe_right_arg,
                                       pipe_environment = parent.frame()) {
  d <- pipe_left_arg
  node_tree <- pipe_right_arg
  env <- pipe_environment
  tabName <- tables_used(node_tree)
  if(length(tabName)!=1) {
    stop("rquery::rquery_apply_to_data_frame node_tree must reference exactly one table.")
  }
  need_close <- FALSE
  db_handle <- base::mget("winvector_temp_db_handle",
                          envir = env,
                          ifnotfound = list(NULL),
                          inherits = TRUE)[[1]]
  if(is.null(db_handle)) {
    my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    need_close = TRUE
  } else {
    my_db <- db_handle$db
  }
  dR <- dbi_copy_to(my_db,
                    tabName,
                    d,
                    temporary = TRUE,
                    overwrite = FALSE)
  sql <- to_sql(node_tree, my_db)
  res <- DBI::dbGetQuery(my_db, sql)
  x <- DBI::dbExecute(my_db, paste("DROP TABLE", tabName))
  if(need_close) {
    DBI::dbDisconnect(my_db)
  }
  res
}
