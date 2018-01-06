

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
#' d <- data.frame(AUC = 0.6, R2 = c(0.1, 0.2), D = NA, z = 2)
#' q <- table_source("d", c("AUC", "R2", "D")) %.>%
#' 	extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., "R2", desc = TRUE)
#'
#' rquery_apply_to_data_frame(d, q)
#'
#' execute(q, data = d)
#'
#' d %.>% q
#' # run (and build result for) ad-hoc query
#' d %.>%
#'   extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., "R2", desc = TRUE) %.>%
#'   execute(.)
#' # print ad-hoc query (result only available for printing)
#' d %.>%
#'   extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., "R2", desc = TRUE)
#'
#' DBI::dbDisconnect(winvector_temp_db_handle$db)
#' winvector_temp_db_handle <- NULL
#'
#' @export
#'
rquery_apply_to_data_frame <- function(pipe_left_arg,
                                       pipe_right_arg,
                                       pipe_environment = parent.frame()) {
  d <- pipe_left_arg
  node_tree <- pipe_right_arg
  env <- pipe_environment
  tabs <- tables_used(node_tree)
  tabName <- c()
  if(length(tabs)!=1) {
    for(ni in names(tabs)) {
      ti <- tabs[[ni]]
      if(is.null(ti$data)) {
        tabName <- c(tabName, ni)
      }
    }
  } else {
    tabName <- names(tabs)[[1]]
  }
  if(length(tabName)!=1) {
    stop("rquery::rquery_apply_to_data_frame node_tree must reference exactly one table or exactly one unbound table.")
  }
  need_close <- FALSE
  db_handle <- base::mget("winvector_temp_db_handle",
                          envir = env,
                          ifnotfound = list(NULL),
                          inherits = TRUE)[[1]]
  if(is.null(db_handle)) {
    my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::initExtension(my_db)
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

#' Attempt to execute a pipeline (assuming it has local data)
#'
#' @param node_tree rquery relop pipeline.
#' @param ... force later arguments to bind by name.
#' @param env environment to work in.
#' @param data data.frame to evaluate.
#' @return executed pipleline or NULL if not executable.
#'
#' @examples
#'
#' winvector_temp_db_handle <- list(
#'   db = DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' )
#' RSQLite::initExtension(winvector_temp_db_handle$db)
#'
#' d <- data.frame(AUC = 0.6, R2 = c(0.1, 0.2), D = NA, z = 2)
#' q <- table_source("d", c("AUC", "R2", "D")) %.>%
#' 	extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., "R2", desc = TRUE)
#'
#' rquery_apply_to_data_frame(d, q)
#'
#' execute(q, data = d)
#'
#' d %.>% q
#' # run (and build result for) ad-hoc query
#' d %.>%
#'   extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., "R2", desc = TRUE) %.>%
#'   execute(.)
#' # print ad-hoc query (result only available for printing)
#' d %.>%
#'   extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., "R2", desc = TRUE)
#'
#' DBI::dbDisconnect(winvector_temp_db_handle$db)
#' winvector_temp_db_handle <- NULL
#'
#' @export
#'
execute <- function(node_tree,
               ...,
               env = parent.frame(),
               data = NULL) {
  if(length(list(...))>0) {
    stop("rquery: unexpected arguments")
  }
  tabs <- tables_used(node_tree)
  if( (length(tabs)==1) &&
     ((!is.null(data)) || (!is.null(tabs[[1]]$data))) ) {
    if(is.null(data)) {
      data <- tabs[[1]]$data
    }
    res <- rquery_apply_to_data_frame(data, node_tree, env)
    return(res)
  }
  NULL
}

#' @export
as.data.frame.relop <- function (x, row.names = NULL, optional = FALSE, ...) {
  execute(x, env = parent.frame())
}
