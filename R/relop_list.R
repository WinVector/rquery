

#' List of \code{rquery::relop} operator trees taken in order.
#'
#' Used to collect a sequence of related operations.
#'
#' @export
setClass(
  "relop_list",
  slots = c(mutable_store = "environment",
            name_source = "function"))


#' Create a new \code{rquery::relop} operator tree collector list
#'
#' @param name_source a wrapr::mk_tmp_name_source()
#' @return a relop_list relop stage collector
#'
#' @export
#'
make_relop_list <- function(name_source = wrapr::mk_tmp_name_source("rqrol")) {
  force(name_source)
  mutable_store = new.env(parent = emptyenv())
  assign("stages", list(), envir = mutable_store)
  new("relop_list",
      mutable_store = mutable_store,
      name_source = name_source)
}

#' S4 print method
#'
#' @param object item to print
#'
#' @export
setMethod(
  f = "show",
  signature = "relop_list",
  definition = function(object) {
    lst <- object@mutable_store$stages
    names(lst) <- vapply(lst,
                         function(opi) {
                           opi$materialize_as
                         }, character(1))
    print(lst)
  })

#' Add a relop to the end of a relop_list.
#'
#' @param collector a rquery::relop_list
#' @param ops a rquery::relop
#' @return a rquery::relop_table_source representing ops's future materialization.
#'
#' @export
#'
add_relop <- function(collector, ops) {
  if(!(isS4(collector) && methods::is(collector, "relop_list"))) {
    stop("rquery::add_relop, expected collector to be of S4 class relop_list")
  }
  if(!("relop" %in% class(ops))) {
    stop("rquery::add_relop, expected ops to be of S3 class relop")
  }
  table_name <- collector@name_source()
  ops$materialize_as <- table_name
  table <- mk_td(table_name, column_names(ops)) # note this preculdes later thinning
  stages <- get("stages", envir = collector@mutable_store)
  assign("stages", c(stages, list(ops)), envir = collector@mutable_store)
  table
}


# using ANY as relops are not S4 classes.

#' Add a relop to the end of a relop_list (piped version).
#'
#' Add a relop to the end of a relop_list in pipe notation with collector on the right.
#'
#' @param pipe_left_arg relop operation tree
#' @param pipe_right_arg relop_list
#' @param pipe_environment environment to evaluate in.
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @importMethodsFrom wrapr ApplyTo apply_right_S4
#' @export
setMethod(
  "apply_right_S4",
  signature(pipe_left_arg = "ANY", pipe_right_arg = "relop_list"),
  function(pipe_left_arg,
           pipe_right_arg,
           pipe_environment,
           left_arg_name,
           pipe_string,
           right_arg_name) {
    ops <- pipe_left_arg
    collector <- pipe_right_arg
    if(!("relop" %in% class(ops))) {
      print(class(ops))
      stop("rquery pipe into relop_list expected left argument to be of class relop")
    }
    rquery::add_relop(collector, ops)
  })




#' Return the stages list.
#'
#' @param collector a rquery::relop_list
#' @return a list of rquery::relops
#'
#' @export
#'
get_relop_list_stages <- function(collector) {
  if(!(isS4(collector) && methods::is(collector, "relop_list"))) {
    stop("rquery::add_relop, expected collector to be of S4 class relop_list")
  }
  collector@mutable_store$stages
}

#' Materialize a stages list.
#'
#' @param db database connecton (rquery_db_info class or DBI connections preferred).
#' @param collector a rquery::relop_list
#' @param ... force later arguments to bind by name.
#' @param limit numeric if not NULL result limit (to use this, last statement must not have a limit).
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param overwrite logical if TRUE drop an previous table.
#' @param temporary logical if TRUE try to create a temporary table.
#'
#' @return a rquery::relop_table_source representing ops's materialization.
#'
#' @export
#'
materialize_relop_list_stages <- function(db,
                                          collector,
                                          ...,
                                          limit = NULL,
                                          source_limit = NULL,
                                          overwrite = TRUE,
                                          temporary = TRUE) {
  if(!(isS4(collector) && methods::is(collector, "relop_list"))) {
    stop("rquery::add_relop, expected collector to be of S4 class relop_list")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::get_relop_list_stages")
  res <- NULL
  nstg <- length(collector@mutable_store$stages)
  for(i in seq_len(nstg)) {
    stage <- collector@mutable_store$stages[[i]]
    lmt <- NULL
    if(i==nstg) {
      lmt <- limit
    }
    res <- materialize(db, stage, table_name = stage$materialize_as,
                       limit = lmt,
                       source_limit = source_limit,
                       temporary = temporary,
                       overwrite = overwrite)
  }
  res
}



#' Materialize a stages list in pipe notation with relop_list on the left.
#'
#' Materialize a stages list in pipe notation with relop_list on the left.
#'
#' @param pipe_left_arg relop_list
#' @param pipe_right_arg rquery_db_info
#' @param pipe_environment environment to evaluate in.
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @importMethodsFrom wrapr ApplyTo apply_right_S4
#' @export
setMethod(
  "apply_right_S4",
  signature(pipe_left_arg = "relop_list", pipe_right_arg = "rquery_db_info"),
  function(pipe_left_arg,
           pipe_right_arg,
           pipe_environment,
           left_arg_name,
           pipe_string,
           right_arg_name) {
    db <- pipe_right_arg
    collector <- pipe_left_arg
    rquery::materialize_relop_list_stages(db, collector)
  })


#' Materialize a stages list in pipe notation with relop_list on the right.
#'
#' Materialize a stages list in pipe notation with relop_list on the right.
#'
#' @param pipe_left_arg rquery_db_info
#' @param pipe_right_arg relop_list
#' @param pipe_environment environment to evaluate in.
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @importMethodsFrom wrapr ApplyTo apply_right_S4
#' @export
setMethod(
  "apply_right_S4",
  signature(pipe_left_arg = "rquery_db_info", pipe_right_arg = "relop_list"),
  function(pipe_left_arg,
           pipe_right_arg,
           pipe_environment,
           left_arg_name,
           pipe_string,
           right_arg_name) {
    db <- pipe_left_arg
    collector <- pipe_right_arg
    rquery::materialize_relop_list_stages(db, collector)
  })


