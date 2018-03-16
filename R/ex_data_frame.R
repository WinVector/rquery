

re_write_table_names <- function(op_tree, new_name) {
  if(!is.null(op_tree$table_name)) {
    op_tree$table_name <- new_name
  }
  for(i in seq_len(length(op_tree$source))) {
    op_tree$source[[i]] <- re_write_table_names(op_tree$source[[i]],
                                                new_name)
  }
  op_tree
}

#' Execture optree in an enviroment where d is the only data.
#'
#' Default DB uses RSQLite (so some functions are not supported).
#'
#' @param d data.frame
#' @param optree rquery rel_op operation tree.
#' @param env environment to look for "winvector_temp_db_handle" in.
#' @param limit numeric if not null limit result to this many rows.
#' @return data.frame result
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   winvector_temp_db_handle <- list(
#'     db = db
#'   )
#'   RSQLite::initExtension(winvector_temp_db_handle$db)
#'
#'   optree <- table_source("d", c("AUC", "R2", "D")) %.>%
#'   	extend_nse(., c := sqrt(R2)) %.>%
#'     orderby(., rev_cols = "R2")
#'
#'   d <- data.frame(AUC = 0.6, R2 = c(0.1, 0.2), D = NA, z = 2)
#'   rquery_apply_to_data_frame(d, optree) %.>%
#'      print(.)
#'
#'   winvector_temp_db_handle <- NULL
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#'
rquery_apply_to_data_frame <- function(d,
                                       optree,
                                       env = parent.frame(),
                                       limit = NULL) {
  if(!is.data.frame(d)) {
    stop("rquery::rquery_apply_to_data_frame d must be a data.frame")
  }
  if(!("relop" %in% class(optree))) {
    stop("rquery::rquery_apply_to_data_frame expect optree to be of class relop")
  }
  tabNames <- tables_used(optree)
  if(length(tabNames)!=1) {
    stop("rquery::rquery_apply_to_data_frame optree must reference exactly one table")
  }
  tmp_name_source <- mk_tmp_name_source('rqatmp')
  inp_name <- tmp_name_source()
  res_name <- tmp_name_source()
  optree <- re_write_table_names(optree, inp_name)
  need_close <- FALSE
  db_handle <- base::mget("winvector_temp_db_handle",
                          envir = env,
                          ifnotfound = list(NULL),
                          inherits = TRUE)[[1]]
  my_db <- NULL
  if(is.null(db_handle)) {
    if (requireNamespace("RSQLite", quietly = TRUE)) {
      my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      RSQLite::initExtension(my_db)
      need_close = TRUE
    }
  } else {
    my_db <- db_handle$db
  }
  if(is.null(my_db)) {
    stop("rquery::rquery_apply_to_data_frame no database")
  }
  dR <- dbi_copy_to(my_db,
                    inp_name,
                    d,
                    temporary = TRUE,
                    overwrite = FALSE)
  materialize(my_db,
              optree,
              table_name = res_name,
              overwrite = TRUE,
              temporary = TRUE)
  sql <- paste("SELECT * FROM",
               DBI::dbQuoteIdentifier(my_db, res_name))
  if(!is.null(limit)) {
    sql <- paste(sql, "LIMIT",
                 format(ceiling(limit), scientific = FALSE))
  }
  res <- DBI::dbGetQuery(my_db, sql)
  x <- DBI::dbExecute(my_db, paste("DROP TABLE", inp_name))
  x <- DBI::dbExecute(my_db, paste("DROP TABLE", res_name))
  if(need_close) {
    DBI::dbDisconnect(my_db)
  }
  res
}

grab_first_dat <- function(op_tree) {
  for(i in seq_len(length(op_tree$source))) {
    di <- grab_first_dat(op_tree$source[[i]])
    if(!is.null(di)) {
      return(di)
    }
  }
  op_tree$data
}

#' Attempt to execute a pipeline (assuming it has local data, or is passed local data).
#'
#' @param optree rquery relop pipeline.
#' @param ... force later arguments to bind by name.
#' @param env environment to work in.
#' @param data data.frame to evaluate.
#' @param limit numeric if not null limit result to this many rows.
#' @return executed pipleline or NULL if not executable.
#'
#'
#' @noRd
#'
execute_embeded_data_frame <- function(optree,
               ...,
               env = parent.frame(),
               data = NULL,
               limit = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::execute_embeded_data_frame")
  tabs <- tables_used(optree)
  if(length(tabs)!=1) {
    return(NULL)
  }
  data <- grab_first_dat(optree)
  if(is.null(data)) {
    return(NULL)
  }
  res <- rquery_apply_to_data_frame(data, optree,
                                    env = env,
                                    limit = limit)
  return(res)
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
  execute_embeded_data_frame(x,
          env = parent.frame(), limit = n)
}


#' @export
print.relop <- function(x, ...) {
  dotargs <- list(...)
  n <- NULL
  if(!is.null(dotargs$n)) {
    n <- dotargs$n
  }
  res <- execute_embeded_data_frame(x,
                 env = parent.frame(), limit = n)
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
  res <- execute_embeded_data_frame(x,
                 env = parent.frame(), limit = n)
  if(!is.null(res)) {
    res
  } else {
    x
  }
}

#' Execute pipeline treating pipe_left_arg as local data to
#' be copied into database.
#'
#' @param pipe_left_arg data.frame or DBI database connection
#' @param pipe_right_arg rquery relop operation tree
#' @param pipe_environment environment to execute in
#' @param pipe_name name of pipling symbol
#' @return data.frame
#'
#' @seealso \code{\link{rquery_apply_to_data_frame}}
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   # set up example database and
#'   # db execution helper
#'   db <- DBI::dbConnect(RSQLite::SQLite(),
#'                        ":memory:")
#'   RSQLite::initExtension(db)
#'   winvector_temp_db_handle <- list(db = db)
#'
#'   # operations pipeline/tree
#'   optree <- table_source("d", "x") %.>%
#'     extend_nse(., y = x*x)
#'
#'   # wrapr dot pipe wrapr_function dispatch
#'   # causes this statment to apply optree
#'   # to d.
#'   data.frame(x = 1:3) %.>% optree %.>% print(.)
#'
#'   # remote example
#'   dbi_copy_to(db, "d",
#'               data.frame(x = 7:8),
#'               overwrite = TRUE,
#'               temporary = TRUE)
#'
#'   # wrapr dot pipe wrapr_function dispatch
#'   # causes this statment to apply optree
#'   # to db.
#'   db %.>% optree %.>% print(.)
#'
#'   # clean up
#'   rm(list = "winvector_temp_db_handle")
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#'
wrapr_function.relop <- function(pipe_left_arg,
                                 pipe_right_arg,
                                 pipe_environment,
                                 pipe_name = NULL) {
  if(!("relop" %in% class(pipe_right_arg))) {
    stop("rquery::wrapr_function.relop expect pipe_right_arg to be of class relop")
  }
  if(is.data.frame(pipe_left_arg)) {
    return(rquery_apply_to_data_frame(pipe_left_arg,
                                      pipe_right_arg,
                                      pipe_environment))
  }
  # assume pipe_left_arg is a DB connection, execute and bring back result
  execute(pipe_left_arg, pipe_right_arg)
}
