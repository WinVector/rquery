

# Common ops.
# "extend",
# "project",
# "natural_join",
# "select_rows",
# "drop_columns",
# "select_columns",
# "rename_columns",
# "order_rows",
# "convert_records", # exported form cdata as: build_pivot_control.wrapped_relop, blocks_to_rowrecs.wrapped_relop, unpivot_to_blocks.wrapped_relop

#' @export
#' @keywords internal
#'
extend.wrapped_relop <- function(source,
                                 ...,
                                 partitionby = NULL,
                                 orderby = NULL,
                                 reverse = NULL,
                                 display_form = NULL,
                                 env = parent.frame()) {
  force(env)
  underlying = extend(source$underlying,
                      ...,
                      partitionby = partitionby,
                      orderby = orderby,
                      reverse = reverse,
                      display_form = display_form,
                      env = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}


#' @export
#' @keywords internal
#'
extend_se.wrapped_relop <- function(source, assignments,
                                    ...,
                                    partitionby = NULL,
                                    orderby = NULL,
                                    reverse = NULL,
                                    display_form = NULL,
                                    env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rqdatatable::extend_se.wrapped_relop")
  underlying = extend_se(source, assignments,
                         partitionby = partitionby,
                         orderby = orderby,
                         reverse = reverse,
                         display_form = display_form,
                         env = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}



#' @export
#' @keywords internal
#'
project.wrapped_relop <- function(source,
                                  ...,
                                  groupby = c(),
                                  env = parent.frame()) {
  force(env)
  underlying = project(source$underlying,
                       ...,
                       groupby = groupby,
                       env = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}


#' @export
#' @keywords internal
#'
project_se.wrapped_relop <- function(source,
                                     assignments,
                                     ...,
                                     groupby=c(),
                                     env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rqdatatable::project_se.wrapped_relop")
  underlying = project_se(source, assignments,
                          groupby = groupby,
                          env = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}


#' @export
#' @keywords internal
#'
natural_join.wrapped_relop <- function(a, b,
                                       ...,
                                       by,
                                       jointype = 'INNER',
                                       env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rqdatatable::natural_join.wrapped_relop")
  data_map <- source$data_map
  if(!('wrapped_relop' %in% class(b))) {
    stop("rqdatatable::natural_join.wrapped_relop expected b to be a wrapped_relop")
  }
  for(k in names(b$data_map)) {
    data_map[[k]] <- b$data_map[[k]]
  }
  underlying = natural_join(a, b$underlying,
                            by = by,
                            jointype = jointype,
                            env = env)
  res <- list(underlying = underlying,
              data_map = data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}


lapply_bquote_to_langauge_list <- function(ll, env) {
  force(env)
  lapply(ll,
         function(li) {
           do.call(bquote, list(expr = li, where = env), envir = env)
         })
}


#' @export
#' @keywords internal
#'
select_rows.wrapped_relop <- function(source, expr,
                                      env = parent.frame()) {
  force(env)
  exprq <- substitute(expr)
  # # value oriented solution1: parse and pass on, a bit brutal
  # exprq <- lapply_bquote_to_langauge_list(list(exprq), env)[[1]]
  # exprs <- paste(format(exprq), collapse = "\n")
  # underlying = select_rows_se(source$underlying, exprs,
  #                             env = env)
  # # value oriented solution2: substitute and apss on
  # exprs <- format(exprq)
  # underlying = select_rows_se(source$underlying, exprs,
  #                             env = env)
  # # substitute/do.call based solution
  underlying = do.call(
    select_rows,
    list(source$underlying,
         exprq,
         env = env),
    envir = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}



#' @export
#' @keywords internal
#'
select_rows_se.wrapped_relop <- function(source, expr,
                                         env = parent.frame()) {
  force(env)
  underlying = select_rows_se(source$underlying, expr,
                              env = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}


#' @export
#' @keywords internal
#'
drop_columns.wrapped_relop <- function(source, drops,
                                       ...,
                                       strict = TRUE,
                                       env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rqdatatable:drop_columns.wrapped_relo")
  underlying = drop_columns(source$underlying, drops,
                            strict = strict,
                            env = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}


#' @export
#' @keywords internal
#'
select_columns.wrapped_relop <- function(source, columns, env = parent.frame()) {
  force(env)
  underlying = select_columns(source$underlying, columns,
                              env = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}


#' @export
#' @keywords internal
#'
rename_columns.wrapped_relop <- function(source, cmap,
                                         env = parent.frame()) {
  force(env)
  underlying = rename_columns(source$underlying, cmap,
                              env = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}


#' @export
#' @keywords internal
#'
order_rows.wrapped_relop <- function(source,
                                     cols = NULL,
                                     ...,
                                     reverse = NULL,
                                     limit = NULL,
                                     env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rqdatatable::order_rows.wrapped_relop")
  underlying = order_rows(source$underlying,
                          cols = cols,
                          reverse = reverse,
                          limit = limit,
                          env = env)
  res <- list(underlying = underlying,
              data_map = source$data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}






#' Wrap a data frame for later execution.
#'
#' Create a table description that includes the actual data.  Prevents wastefull table copies in
#' immediate pipelines.  Used with \code{ex()}.
#'
#' @param d data.frame
#' @param ... not used, force later argument to be referred by name
#' @param table_name character, name of table
#' @param env environment to work in.
#' @return a table description, with data attached
#'
#' @examples
#'
#' if(requireNamespace('rqdatatable')) {
#'  d <- data.frame(x = 1:3, y = 4:6)
#'  d %.>%
#'    wrap(.) %.>%
#'    extend(., z := x + y) %.>%
#'    ex(.)
#' }
#'
#' @export
#'
wrap <- function(d,
                 ...,
                 table_name = NULL,
                 env = parent.frame()) {
  UseMethod("wrap", d)
}



#' @export
#'
wrap.data.frame <- function(d,
                            ...,
                            table_name = NULL,
                            env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rqdatatable::wrap")
  force(env)
  if(!('data.frame' %in% class(d))) {
    stop("rqdatatable::wrap expected d to be a data.frame")
  }
  if(length(table_name)<=0) {
    table_name <- as.character(substitute(d))
    if(length(table_name)!=1) {
      table_name <- 'd'
    }
  }
  underlying <- local_td(d, name = table_name, env = env)
  data_map = list(d)
  names(data_map) = table_name
  res <- list(underlying = underlying,
              data_map = data_map)
  class(res) <- 'wrapped_relop'
  return(res)
}


#' Execute a wrapped execution pipeline.
#'
#' Execute a ops-dag using `code{wrap()}` data as values.
#'
#' @param ops rquery pipeline with tables formed by `wrap()`.
#' @param ... not used, force later argument to be referred by name
#' @param env environment to work in.
#' @return data.frame result
#'
#' @examples
#'
#' if(requireNamespace('rqdatatable')) {
#'  d <- data.frame(x = 1:3, y = 4:6)
#'  d %.>%
#'    wrap(.) %.>%
#'    extend(., z := x + y) %.>%
#'    ex(.)
#' }
#'
#' @export
#'
ex <- function(ops,
               ...,
               env = parent.frame()) {
  UseMethod("ex", ops)
}


#' @export
#'
ex.wrapped_relop <- function(ops,
               ...,
               env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rqdatatable::ex")
  if(!('wrapped_relop' %in% class(ops))) {
    stop("rqdatatable::ex expected ops to be of class wrapped_relop")
  }
  force(env)
  rquery_apply_to_data_frame(ops$data_map, ops$underlying, env = env)
}



#' @export
format.wrapped_relop <- function(x, ...) {
  paste0("[",
         format(x$underlying),
         "](\n ",
         paste(names(x$data_map), collapse = ', '),
         ")")
}


#' @export
print.wrapped_relop <- function (x, ...) {
  cat(format(x, ...))
}
