

#' List of \code{rquery::relop} operator trees taken in order.
#'
#' Used to collect a sequence of related operations.
#' For how to use please see here \url{https://github.com/WinVector/rquery/blob/master/extras/query_growth/query_growth.md}.
#'
#' @seealso \code{\link{add_relop}}, \code{\link{get_relop_list_stages}}, \code{\link{materialize_relop_list_stages}}
#'
#' @export
#'
setClass(
  "relop_list",
  slots = c(mutable_store = "environment",
            name_source = "function"))


#' Create a new \code{rquery::relop} operator tree collector list
#'
#' For how to use please see here \url{https://github.com/WinVector/rquery/blob/master/extras/query_growth/query_growth.md}.
#'
#' @param name_source a wrapr::mk_tmp_name_source()
#' @return a relop_list relop stage collector
#'
#' @seealso \code{\link{add_relop}}, \code{\link{get_relop_list_stages}}, \code{\link{materialize_relop_list_stages}}
#'
#' @examples
#'
#' collector <- make_relop_list()
#' ops <- mk_td("d", c("x", "y")) %.>%
#'   extend(., z %:=% x + y) %.>%
#'   collector
#' get_relop_list_stages(collector)
#'
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
#' For how to use please see here \url{https://github.com/WinVector/rquery/blob/master/extras/query_growth/query_growth.md}.
#'
#' @param collector a rquery::relop_list
#' @param ops a rquery::relop
#' @return a rquery::relop_table_source representing ops's future materialization.
#'
#' @seealso \code{\link{make_relop_list}}, \code{\link{get_relop_list_stages}}, \code{\link{materialize_relop_list_stages}}
#'
#' @examples
#'
#' collector <- make_relop_list()
#' ops <- mk_td("d", c("x", "y")) %.>%
#'   extend(., z %:=% x + y) %.>%
#'   collector
#' get_relop_list_stages(collector)
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
  stages <- get("stages", envir = collector@mutable_store)
  # check for circularity (likely due to re-insertion of already existing stages)
  # (something we will not allow)
  for(si in stages) {
    if(length(intersect(tables_used(si), table_name))!=0) {
      stop("rquery::add_relop name chosen for materialization was already used in an earlier table")
    }
    if(length(intersect(si$materialize_as, table_name))!=0) {
      stop("rquery::add_relop name chosen for materialization was already materialization in an earlier table")
    }
  }
  ops$materialize_as <- table_name
  table <- mk_td(table_name, column_names(ops)) # note this preculdes later thinning
  assign("stages", c(stages, list(ops)), envir = collector@mutable_store)
  table
}


# using ANY, as relops are not S4 classes.

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


# work out column use moving right to left
relop_list_stages_columns_used <- function(stages) {
  n <- length(stages)
  new_stages <- list()
  cu <- list()
  for(i in rev(seq_len(n))) {
    si <- stages[[i]]
    mat_as <- si$materialize_as
    if(!is.null(mat_as)) {
      consuming <- cu[[mat_as]]
      if(!is.null(consuming)) {
        si$materialize_as <- NULL
        si <- select_columns(si, consuming)
        si$materialize_as <- mat_as
      }
    }
    cui <- columns_used(si)
    cu <- merge_columns_used(cu, cui)
    new_stages[[i]] <- si
  }
  new_stages
}


# TODO: update incoming table defs to match what is used?

#' Return the stages list.
#'
#' Stages can be narrowed to what is actually used.
#' For how to use please see here \url{https://github.com/WinVector/rquery/blob/master/extras/query_growth/query_growth.md}.
#'
#' @param collector a rquery::relop_list
#' @param ... force later arguments to bind by name
#' @param narrow logical, if TRUE add select_columns() to narrow stages.
#' @return a list of rquery::relops
#'
#' @seealso \code{\link{make_relop_list}}, \code{\link{add_relop}}, \code{\link{materialize_relop_list_stages}}
#'
#' @examples
#'
#' collector <- make_relop_list()
#' ops <- mk_td("d", c("x", "y")) %.>%
#'   extend(., z %:=% x + y) %.>%
#'   collector
#' get_relop_list_stages(collector)
#'
#' @export
#'
get_relop_list_stages <- function(collector,
                                  ...,
                                  narrow = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::get_relop_list_stages")
  if(!(isS4(collector) && methods::is(collector, "relop_list"))) {
    stop("rquery::get_relop_list_stages, expected collector to be of S4 class relop_list")
  }
  stages <- collector@mutable_store$stages
  if(narrow) {
    stages <- relop_list_stages_columns_used(stages)
  }
  stages
}




#' Materialize a stages list on a database.
#'
#' For how to use please see here \url{https://github.com/WinVector/rquery/blob/master/extras/query_growth/query_growth.md}.
#'
#' @param db database connecton (rquery_db_info class preferred, or DBI connections).
#' @param collector a rquery::relop_list
#' @param ... force later arguments to bind by name.
#' @param narrow logical, if TRUE add select_columns() to narrow stages.
#' @param limit numeric if not NULL result limit (to use this, last statement must not have a limit).
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param overwrite logical if TRUE drop an previous table.
#' @param temporary logical if TRUE try to create a temporary table.
#'
#' @return a rquery::relop_table_source representing ops's materialization.
#'
#' @seealso \code{\link{make_relop_list}}, \code{\link{add_relop}}, \code{\link{get_relop_list_stages}}
#'
#' @examples
#'
#' if(requireNamespace("DBI", quietly = TRUE) &&
#'    requireNamespace("RSQLite", quietly = TRUE) ) {
#'
#'   rsqlite_connection <- DBI::dbConnect(RSQLite::SQLite(),
#'                                        ":memory:")
#'   db <- rquery_db_info(
#'     connection = rsqlite_connection,
#'     is_dbi = TRUE
#'     )
#'
#'   collector <- make_relop_list()
#'   d <- rq_copy_to(db, "d", data.frame(x = 1, y = 2),
#'                   temporary = TRUE, overwrite = TRUE)
#'   ops1 <- d %.>%
#'     extend(., z %:=% x + y) %.>%
#'     collector
#'   ops2 <- ops1 %.>%
#'     extend(., z2 %:=% x / y) %.>%
#'     collector
#'   get_relop_list_stages(collector)
#'   res <- materialize_relop_list_stages(db, collector)
#'   res
#'   execute(db, res)
#'
#'   DBI::dbDisconnect(rsqlite_connection)
#' }
#'
#' @export
#'
materialize_relop_list_stages <- function(db,
                                          collector,
                                          ...,
                                          narrow = TRUE,
                                          limit = NULL,
                                          source_limit = NULL,
                                          overwrite = TRUE,
                                          temporary = TRUE) {
  if(!(isS4(collector) && methods::is(collector, "relop_list"))) {
    stop("rquery::materialize_relop_list_local, expected collector to be of S4 class relop_list")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::materialize_relop_list_stages")
  res <- NULL
  stages <- get_relop_list_stages(collector, narrow = narrow)
  nstg <- length(stages)
  for(stage in stages) {
    rq_remove_table(db, stage$materialize_as)
  }
  for(i in seq_len(nstg)) {
    stage <- stages[[i]]
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


