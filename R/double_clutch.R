
double_apply_impl <- function(d, ds, ops, env) {
  if(!is.data.frame(d)) {
    stop("rquery::`%>>$` d must be a data.frame")
  }
  b <- wrapr:::pipe_impl(pipe_left_arg = ds,
                         pipe_right_arg = ops,
                         pipe_environment = env,
                         pipe_string = "%.>%")
  if(!("relop" %in% class(b))) {
    stop("rquery::`%>>$` ops must must evaluate to a relop tree")
  }
  tabs <- tables_used(b)
  if(length(tabs)!=1) {
    stop("rquery::`%>>$` ops must use only one table")
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
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   winvector_temp_db_handle <- list(db = my_db)
#'
#'   data.frame(AUC = c(0.6, 0.45), R2 = c(0.2, 0.01)) %>>% (
#'     extend_se(., c("v" := "AUC + R2", "x" := "pmax(AUC,v)")) %.>%
#'     select_rows_nse(., x > 0.5) ) %.>%
#'     print(.)
#'
#'   rm(list = "winvector_temp_db_handle")
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
