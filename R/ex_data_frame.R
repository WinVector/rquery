

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

is_named_list_of_data_frames <- function(o) {
  if(!is.list(o)) {
    return(FALSE)
  }
  nms <- names(o)
  if(length(nms)!=length(o)) {
    return(FALSE)
  }
  for(ni in nms) {
    if(!is.data.frame(o[[ni]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Execute optree in an environment where d is the only data.
#'
#' Default DB uses RSQLite (so some functions are not supported).
#'
#' @param d data.frame or named list of data.frames.
#' @param optree rquery rel_op operation tree.
#' @param ... force later arguments to bind by name.
#' @param limit integer, if not NULL limit result to no more than this many rows.
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param allow_executor logical if TRUE allow any executor set as rquery.rquery_executor to be used.
#' @param env environment to look to.
#' @return data.frame result
#'
#' @examples
#'
#' # WARNING: example tries to change rquery.rquery_db_executor option to RSQLite and back.
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   RSQLite::initExtension(db)
#'   old_o <- options(list("rquery.rquery_db_executor" = list(db = db)))
#'
#'   optree <- mk_td("d", c("AUC", "R2", "D")) %.>%
#'   	extend_nse(., c %:=% sqrt(R2)) %.>%
#'     orderby(., cols = "R2", reverse = "R2")
#'
#'   d <- data.frame(AUC = 0.6, R2 = c(0.1, 0.2), D = NA, z = 2)
#'   v <- rquery_apply_to_data_frame(d, optree)
#'   print(v)
#'
#'   # now load up a table without an R2 column,
#'   # want to show this is caught
#'   d <- data.frame(z = 1)
#'   tryCatch(
#'      rquery_apply_to_data_frame(d, optree),
#'      error = function(e) { as.character(e) }
#'     ) %.>%
#'     print(.)
#'
#'   options(old_o)
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#'
rquery_apply_to_data_frame <- function(d,
                                       optree,
                                       ...,
                                       limit = NULL,
                                       source_limit = NULL,
                                       allow_executor = TRUE,
                                       env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::rquery_apply_to_data_frame")
  if(!("relop" %in% class(optree))) {
    stop("rquery::rquery_apply_to_data_frame expect optree to be of class relop")
  }
  if((!is.data.frame(d)) && (!is_named_list_of_data_frames(d)) && (!is.environment(d))) {
    stop("rquery::rquery_apply_to_data_frame d must be a data.frame, a named list of data.frames, or an environment")
  }
  tabNames <- tables_used(optree)
  executor <- NULL
  if(allow_executor) {
    executor <- getOption("rquery.rquery_executor", default = NULL)
  }
  if(!is.null(executor)) {
    tables <- NULL
    if(is.data.frame(d)) {
      if(length(tabNames)!=1) {
        stop("rquery::rquery_apply_to_data_frame optree must reference exactly one table a non-list is passed to rquery_executor")
      }
      tables <- list(x = d)
      names(tables) <- tabNames
    } else if(is_named_list_of_data_frames(d)) {
      tables <- d
    } else if(is.environment(d)) {
      tables <- as.list(d)
    }
    res <- executor$f(optree = optree,
                      tables = tables,
                      source_limit = source_limit,
                      env = env)
    if((!is.null(limit)) && (limit<nrow(res))) {
      res <- res[seq_len(limit), , drop = FALSE]
    }
    return(res)
  }
  my_db <- NULL
  rquery.rquery_db_executor <- getOption("rquery.rquery_db_executor", default = NULL)
  if(!is.null(rquery.rquery_db_executor)) {
    my_db <- rquery.rquery_db_executor$db
  }
  if(is.null(my_db)) {
    stop("rquery::rquery_apply_to_data_frame no default executor or database (please try library('rqdatatable'))")
  }
  if(length(tabNames)!=1) {
    stop("rquery::rquery_apply_to_data_frame optree must reference exactly one table when rquery.rquery_executor option is not set")
  }
  if(is_named_list_of_data_frames(d) && (length(d)==1)) {
    d <- d[[1]]
  }
  if(!is.data.frame(d)) {
    stop("rquery::rquery_apply_to_data_frame d must be a data.frame or list with one data.frame when rquery.rquery_executor option is not set")
  }
  cols_used <- columns_used(optree)[[tabNames]]
  missing <- setdiff(cols_used, colnames(d))
  if(length(missing)>0) {
    stop(paste("rquery::rquery_apply_to_data_frame d missing required columns:",
         paste(missing, collapse = ", ")))
  }
  d <- d[ , cols_used, drop = FALSE]
  if((!is.null(source_limit)) && (source_limit<nrow(d))) {
    d <- d[seq_len(source_limit), , drop = FALSE]
  }
  tmp_name_source <- mk_tmp_name_source('rqatmp')
  inp_name <- tmp_name_source()
  optree <- re_write_table_names(optree, inp_name)
  dR <- rq_copy_to(my_db,
                    inp_name,
                    d,
                    temporary = TRUE,
                    overwrite = FALSE)
  res <- execute(my_db, optree,
                 limit = limit,
                 overwrite = TRUE,
                 temporary = TRUE,
                 precheck = FALSE,
                 allow_executor = allow_executor,
                 env = env)
  rq_remove_table(my_db, inp_name)
  res
}

#' @export
print.relop <- function(x, ...) {
  txt <- format(x)
  txt <- trimws(gsub("[ \t\r\n]+", " ", txt), which = "both")
  print(txt, ...)
}


#' @export
summary.relop <- function(object, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::summary.relop")
  format(object)
}

#' @export
as.character.relop <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::as.character.relop")
  format(object)
}


#' Execute pipeline treating pipe_left_arg as local data to
#' be copied into database.
#'
#' @param pipe_left_arg left argument.
#' @param pipe_right_arg substitute(pipe_right_arg) argument.
#' @param pipe_environment environment to evaluate in.
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return data.frame
#'
#' @seealso \code{\link{rquery_apply_to_data_frame}}
#'
#' @examples
#'
#' # WARNING: example tries to change rquery.rquery_db_executor option to RSQLite and back.
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   # set up example database and
#'   # db execution helper
#'   db <- DBI::dbConnect(RSQLite::SQLite(),
#'                        ":memory:")
#'   RSQLite::initExtension(db)
#'   old_o <- options(list("rquery.rquery_db_executor" = list(db = db)))
#'
#'   # operations pipeline/tree
#'   optree <- mk_td("d", "x") %.>%
#'     extend_nse(., y = x*x)
#'
#'   # wrapr dot pipe apply_right dispatch
#'   # causes this statment to apply optree
#'   # to d.
#'   data.frame(x = 1:3) %.>% optree %.>% print(.)
#'
#'   # remote example
#'   rq_copy_to(db, "d",
#'               data.frame(x = 7:8),
#'               overwrite = TRUE,
#'               temporary = TRUE)
#'
#'   # wrapr dot pipe apply_right dispatch
#'   # causes this statment to apply optree
#'   # to db.
#'   db %.>% optree %.>% print(.)
#'
#'   # clean up
#'   options(old_o)
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#'
apply_right.relop <- function(pipe_left_arg,
                              pipe_right_arg,
                              pipe_environment,
                              left_arg_name,
                              pipe_string,
                              right_arg_name) {
  if(!("relop" %in% class(pipe_right_arg))) {
    stop("rquery::apply_right.relop expect pipe_right_arg to be of class relop")
  }
  if(is.data.frame(pipe_left_arg) || is_named_list_of_data_frames(pipe_left_arg)) {
    return(rquery_apply_to_data_frame(pipe_left_arg,
                                      pipe_right_arg,
                                      env = pipe_environment))
  }
  if("relop" %in% class(pipe_left_arg)) {
    stop("rquery::apply_right.relop left argument can not be an already defined relop pipeline")
  }
  # assume pipe_left_arg is a DB connection, execute and bring back result
  execute(pipe_left_arg, pipe_right_arg, env = pipe_environment)
}
