

#' Execture optree in an enviroment where d is the only data.
#'
#' Default DB uses RSQLite (so some functions are not supported).
#'
#' @param d data.frame
#' @param optree rquery rel_op operation tree.
#' @param env environment to look for "winvector_temp_db_handle" in.
#' @param result_limit numeric if not null limit result to this many rows.
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
#' optree <- table_source("d", c("AUC", "R2", "D")) %.>%
#' 	extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., rev_cols = "R2")
#'
#' rquery_apply_to_data_frame(d, optree)
#'
#' execute_data_frame(optree, data = d)
#'
#' d %.>% optree
#'
#' # run (and build result for) ad-hoc query
#' d %.>%
#'   extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., rev_cols = "R2") %.>%
#'   execute_data_frame(.)
#' # print ad-hoc query (result only available for printing)
#' d %.>%
#'   extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., rev_cols = "R2")
#'
#' DBI::dbDisconnect(winvector_temp_db_handle$db)
#' winvector_temp_db_handle <- NULL
#'
#' @export
#'
rquery_apply_to_data_frame <- function(d,
                                       optree,
                                       env = parent.frame(),
                                       result_limit = NULL) {
  if(!is.data.frame(d)) {
    stop("rquery::rquery_apply_to_data_frame d must be a data.frame")
  }
  tabs <- tables_used(optree)
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
    stop("rquery::rquery_apply_to_data_frame optree must reference exactly one table or exactly one unbound table.")
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
  sql <- to_sql(optree, my_db)
  if(!is.null(result_limit)) {
    sql <- paste(sql, "LIMIT", result_limit)
  }
  res <- DBI::dbGetQuery(my_db, sql)
  x <- DBI::dbExecute(my_db, paste("DROP TABLE", tabName))
  if(need_close) {
    DBI::dbDisconnect(my_db)
  }
  res
}

#' Attempt to execute a pipeline (assuming it has local data, or is passed local data).
#'
#' @param optree rquery relop pipeline.
#' @param ... force later arguments to bind by name.
#' @param env environment to work in.
#' @param data data.frame to evaluate.
#' @param result_limit numeric if not null limit result to this many rows.
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
#' optree <- table_source("d", c("AUC", "R2", "D")) %.>%
#' 	extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(.,  rev_cols = "R2")
#'
#' rquery_apply_to_data_frame(d, optree)
#'
#' execute_data_frame(optree, data = d)
#'
#' d %.>% optree
#' # run (and build result for) ad-hoc query
#' d %.>%
#'   extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., rev_cols = "R2") %.>%
#'   execute_data_frame(.)
#' # print ad-hoc query (result only available for printing)
#' d %.>%
#'   extend_nse(., c := sqrt(R2)) %.>%
#'   orderby(., rev_cols = "R2")
#'
#' DBI::dbDisconnect(winvector_temp_db_handle$db)
#' winvector_temp_db_handle <- NULL
#'
#' @export
#'
execute_data_frame <- function(optree,
               ...,
               env = parent.frame(),
               data = NULL,
               result_limit = NULL) {
  if(length(list(...))>0) {
    stop("rquery: unexpected arguments")
  }
  tabs <- tables_used(optree)
  if( (length(tabs)==1) &&
     ((!is.null(data)) || (!is.null(tabs[[1]]$data))) ) {
    if(is.null(data)) {
      data <- tabs[[1]]$data
    }
    res <- rquery_apply_to_data_frame(data, optree,
                                      env = env,
                                      result_limit = result_limit)
    return(res)
  }
  NULL
}

#' @export
as.data.frame.relop <- function (x,
                                 row.names = NULL,
                                 optional = FALSE,
                                 ...) {
  dotargs <- list(...)
  n <- NULL
  if(!is.null(dotargs$n)) {
    n <- dotargs$n
  }
  execute_data_frame(x,
          env = parent.frame(), result_limit = n)
}


#' @export
print.relop <- function(x, ...) {
  dotargs <- list(...)
  n <- NULL
  if(!is.null(dotargs$n)) {
    n <- dotargs$n
  }
  res <- execute_data_frame(x,
                 env = parent.frame(), result_limit = n)
  if(!is.null(res)) {
    print(res)
  } else {
    txt <- format(x)
    txt <- trimws(gsub("[ \t\r\n]+", " ", txt), which = "both")
    print(txt, ...)
  }
}

#' @importFrom utils head
NULL

#' @export
head.relop <- function(x, ...) {
  dotargs <- list(...)
  n <- 6
  if(!is.null(dotargs$n)) {
    n <- dotargs$n
  }
  res <- execute_data_frame(x,
                 env = parent.frame(), result_limit = n)
  if(!is.null(res)) {
    res
  } else {
    x
  }
}

