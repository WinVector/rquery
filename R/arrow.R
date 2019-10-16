
#' Data arrow
#'
#' A categorical arrow mapping a table to a table.
#'
#' @param pipeline pipeline with one source table
#' @param ... not used, force later argument to be referred to by name.
#' @param free_table_key name of table to consider free (input) to the pipeline
#' @return relop_arrow wrapping of pipeline
#'
#' @export
#'
arrow <- function(pipeline, ..., free_table_key=NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::arrow")
  if(!('relop' %in% class(pipeline))) {
    stop("expected pipeline to be of class relop")
  }
  cused <- columns_used(pipeline)
  if(length(free_table_key)>0) {
    if( (!is.character(free_table_key)) || (length(free_table_key)!=1)) {
      stop("free_table_key must be a scalar string (or NULL)")
    }
    if(!(free_table_key %in% names(cused))) {
      stop("free_table_key must be the name of an incoming table")
    }
  } else {
    if(length(cused)!=1) {
      stop("expected pipeline to use one table")
    }
    free_table_key = names(cused)[[1]]
  }
  r <- list(
    free_table_key = free_table_key,
    incoming_columns = cused[[free_table_key]],
    outgoing_columns = colnames(pipeline),
    pipeline = pipeline)
  class(r) <- 'relop_arrow'
  return(r)
}

#' @export
format.relop_arrow <- function(x, ...) {
  args <- list(...)
  verbose <- FALSE
  if('verbose' %in% names(args)) {
    verbose <- args[['verbose']]
  }
  if(!verbose) {
  str <- paste0(
    '[\n ', sQuote(x$free_table_key), ":\n ",
    wrapr::map_to_char(x$incoming_columns),
    '\n   ->\n ',
    wrapr::map_to_char(x$outgoing_columns),
    '\n]')
  } else {
    str <- paste0("arrow(\n ", format(x$pipeline),  ",\n ",
                  "free_table_key = ", sQuote(x$free_table_key),
                  ")\n")
  }
  return(str)
}

#' @export
print.relop_arrow <- function(x, ...) {
  cat(format(x, ...))
}

setOldClass('relop_arrow')


compose_arrows <- function(a, b, ..., strict=TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::compose_arrows")
  if(! ('relop_arrow' %in% class(b))) {
    stop("expected b to be a relop_arrow")
  }
  if('data.frame' %in% class(a)) {
    missing <- setdiff(b$incoming_columns, colnames(a))
    if(length(missing)>0) {
      stop(paste("missing required columns:", wrapr::map_to_char(missing)))
    }
    excess <- setdiff(colnames(a), b$incoming_columns)
    if(length(excess)>0) {
      if(strict) {
        stop(paste0("unexpected columns: ", paste(excess, collapse = ', ')))
      }
      a <- a[, b$incoming_columns, drop = FALSE]
    }
    res <- a %.>% b$pipeline
    return(res)
  }
  if(! ('relop_arrow' %in% class(a))) {
    stop("expected a to be a relop_arrow or data.frame")
  }
  missing <- setdiff(b$incoming_columns, a$outgoing_columns)
  if(length(missing)>0) {
    stop(paste("missing required columns:", wrapr::map_to_char(missing)))
  }
  excess <- setdiff(a$outgoing_columns, b$incoming_columns)
  if(length(excess)>0) {
    if(strict) {
      stop(paste0("unexpected columns: ", paste(excess, collapse = ', ')))
    }
    return(arrow(a$pipeline %.>% select_columns(., columns=b$incoming_columns) %.>% b$pipeline))
  }
  composite <- replace_all_table_sources(b$pipeline, a$pipeline, table_key = b$free_table_key)
  return(arrow(composite))
}


#' S4 dispatch method for apply_right.
#'
#' compose a data.frame and a relop_arrow class
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg pipe_right_arg argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @export
setMethod(
  "apply_right_S4",
  signature = c("data.frame", "relop_arrow"),
  definition = function(pipe_left_arg,
                        pipe_right_arg,
                        pipe_environment,
                        left_arg_name,
                        pipe_string,
                        right_arg_name) {
    return(compose_arrows(pipe_left_arg, pipe_right_arg, strict=TRUE))
  }
)


#' S4 dispatch method for apply_right.
#'
#' compose two relop_arrow classes
#'
#' @param pipe_left_arg left argument
#' @param pipe_right_arg pipe_right_arg argument
#' @param pipe_environment environment to evaluate in
#' @param left_arg_name name, if not NULL name of left argument.
#' @param pipe_string character, name of pipe operator.
#' @param right_arg_name name, if not NULL name of right argument.
#' @return result
#'
#' @export
setMethod(
  "apply_right_S4",
  signature = c("relop_arrow", "relop_arrow"),
  definition = function(pipe_left_arg,
                        pipe_right_arg,
                        pipe_environment,
                        left_arg_name,
                        pipe_string,
                        right_arg_name) {
    return(compose_arrows(pipe_left_arg, pipe_right_arg, strict=TRUE))
  }
)


