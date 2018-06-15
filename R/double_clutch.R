







double_apply_impl <- function(d, ds, ops, env) {
  if(!is.data.frame(d)) {
    stop("rquery::`%>>%` left hand side argument must be a data.frame")
  }

  # # # TODO: switch to wrapr::pipe_impl
  # b <- wrapr::pipe_impl(pipe_left_arg = ds,
  #                       pipe_right_arg = ops,
  #                       pipe_environment = env,
  #                       pipe_string = "%.>%")
  tmpenv <- new.env(parent = env)
  assign('.', d, envir = tmpenv)
  b <- eval(ops, envir = tmpenv, enclos = env)

  if(!("relop" %in% class(b))) {
    stop("rquery::`%>>%` right hand side argument must must evaluate to a relop tree")
  }
  tabs <- tables_used(b)
  if(length(tabs)!=1) {
    stop("rquery::`%>>%` right hand side argument must use exactly one table")
  }
  lst <- list(x = d)
  names(lst) <- tabs
  rquery_apply_to_data_frame(lst,
                             b,
                             env = env)
}

#' Double apply pipe to local data frame.
#'
#' Defined as roughly : \code{rquery::rquery_apply_to_data_frame(d \%.>\% ops)}.
#' Useful for systems like \code{rquery} where first appearance of an object
#' captures a description and second appearance applies the operation.
#'
#' Note: to make this usefull one must parenthesize the pipeline that are
#' piping into to.  This is to work around left to right associativity.
#' Please see the example for how this is done.
#'
#' @examples
#'
#' # WARNING: example tries to change rquery.rquery_db_executor option to RSQLite and back.
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   old_o <- options(list("rquery.rquery_db_executor" = list(db = my_db)))
#'
#'   data.frame(AUC = c(0.6, 0.45), R2 = c(0.2, 0.01)) %>>% (
#'     extend_se(., c("v" %:=% "AUC + R2", "x" %:=% "pmax(AUC,v)")) %.>%
#'     select_rows_nse(., x > 0.5) ) %.>%
#'     print(.)
#'
#'   options(old_o)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @param d data.frame.
#' @param ops free-form op tree.
#' @return \code{rquery::rquery_apply_to_data_frame(d \%.>\% ops)}
#'
#' @examples
#'
#'
#'
#' @name doubleapply
#'
#' @export
`%>>%` <- function(d, ops) {
  ds <- substitute(d)
  ops <- substitute(ops)
  env <- parent.frame()
  double_apply_impl(d, ds, ops, env)
}
