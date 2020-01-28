
#' Make a list of assignments, applying many functions to many columns.
#'
#' Make a list of assignments, applying each function to each column named.
#' Intended to be used as an argument in \code{extend_se()} or \code{project_se()}.
#'
#' @param columns character, vector of column names to take values from.
#' @param funs character, names of functions to apply.
#' @param ... not used, forced later parameters to bind by name
#' @param sep character, naming separator
#' @param prefix logical, if TRUE place function names prior, else after in results.
#'
#' @examples
#'
#' assignments <- make_assignments(c('x', 'y'), c('mean', med = 'median'))
#' print(assignments)
#' ops <- mk_td('d', c('x', 'y')) %.>% project_se(., assignments)
#' cat(format(ops))
#'
#' @export
#'
make_assignments <- function(columns, funs,
                             ...,
                             sep = '_',
                             prefix = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), 'rquery::make_assignments')
  if(!is.character(funs)) {
    stop("funs is supposed to be character")
  }
  if(length(funs) < 1) {
    stop("funs must not be empty")
  }
  fn_names <- names(funs)
  if(length(fn_names) < 1) {
    fn_names <- character(length(fn_names))
    fn_names[seq_len(length(fn_names))] <- ""
  }
  fn_names[nchar(fn_names) <= 0] <- funs[nchar(fn_names) <= 0]
  names(funs) <- fn_names
  if(length(unique(fn_names)) != length(funs)) {
    stop("funs must have unique names")
  }
  if(!is.character(columns)) {
    stop("columns must be character")
  }
  if(length(columns) < 1) {
    stop("columns must not be empty")
  }

  combos <- expand.grid(column = columns, fn_name = fn_names)
  if(prefix) {
    combos$result_name <- paste0(combos$fn_name, sep, combos$column)
  } else {
    combos$result_name <- paste0(combos$column, sep, combos$fn_name)
  }
  combos$fn_impl <- funs[combos$fn_name]
  res <- paste0(combos$fn_impl, "(", combos$column, ")")
  names(res) <- combos$result_name
  res
}
