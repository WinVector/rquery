
# execute a pipeline with data.table sources

#' Execute a pipeline with \code{data.table} sources.
#'
#' \code{data.table}s are looked for in the \code{tables} argument and in the execution environment.
#'
#' @param optree relop operations tree.
#' @param ... not used, force later arguments to bind by name.
#' @param tables named list map from table names used in nodes to data.tables and data.frames.
#' @param env environment to work in.
#' @return resulting data.table (input tables can somtimes be mutated as is practice with data.table).
#'
#' @examples
#'
#' if(requireNamespace("data.table", quietly = TRUE)) {
#'   a <- data.table::data.table(x = c(1, 2) , y = c(20, 30), z = c(300, 400))
#'   optree <- local_td(a) %.>%
#'      select_columns(., c("x", "y")) %.>%
#'      select_rows_nse(., x<2 & y<30)
#'   cat(format(optree))
#'   print(ex_data_table(optree))
#' }
#'
#' @export
#'
ex_data_table <- function(optree,
                          ...,
                          tables = list(),
                          env = parent.frame()) {
  UseMethod("ex_data_table", optree)
}



ex_data_table.default <- function(optree,
                                  ...,
                                  tables = list(),
                                  env = parent.frame()) {
  stop(paste("rquery::ex_data_table() does not have an implementation for class: ",
             paste(class(optree), collapse = ", "),
             "yet"))
}

#' @export
ex_data_table.relop_table_source <- function(optree,
                                             ...,
                                             tables = list(),
                                             env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_table_source")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_table_source() requires the data.table package be installed")
  }
  name <- optree$table_name
  res <- NULL
  if(name %in% tables) {
    res <- tables[[name]]
  } else {
    res <- get(name, envir = env)
  }
  if(is.null(res)) {
    stop(paste("rquery::ex_data_table.relop_table_source, could not find: ",
               name))
  }
  if(!is.data.frame(res)) {
    stop(paste("rquery::ex_data_table.relop_table_source ",
               name,
               " is not a data.frame (class: ",
               paste(class(res), collapse = ", "),
               ")"))
  }
  if(!data.table::is.data.table(res)) {
    res <- data.table::as.data.table(res)
  } else {
    res <- copy(res) # try to break reference semantics
  }
  res
}



#' @export
ex_data_table.relop_select_columns <- function(optree,
                                               ...,
                                               tables = list(),
                                               env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_select_columns")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_select_columns() requires the data.table package be installed")
  }
  cols <- optree$columns
  x <- ex_data_table(optree$source[[1]])
  x[, cols, with=FALSE]
}


#' @export
ex_data_table.relop_drop_columns <- function(optree,
                                             ...,
                                             tables = list(),
                                             env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_drop_columns")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_drop_columns() requires the data.table package be installed")
  }
  cols <- optree$columns
  x <- ex_data_table(optree$source[[1]])
  x[, cols, with=FALSE]
}


#' @export
ex_data_table.relop_select_rows <- function(optree,
                                            ...,
                                            tables = list(),
                                            env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_select_rows")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_select_rows() requires the data.table package be installed")
  }
  x <- ex_data_table(optree$source[[1]])
  tmpnam <- ".rquery_ex_select_rows_tmp"
  src <- vapply(seq_len(length(optree$parsed)),
                function(i) {
                  paste0(tmpnam, "[ ", optree$parsed[[1]]$presentation, " , ]")
                }, character(1))
  src <- paste(src, collapse = " & ")
  expr <- parse(text = src)
  tmpenv <- new.env(parent = env)
  assign(tmpnam, x, envir = tmpenv)
  eval(expr, envir = tmpenv, enclos = env)
}


#' @export
ex_data_table.relop_sql <- function(optree,
                                    ...,
                                    tables = list(),
                                    env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_sql")
  stop("rquery::ex_data_table.relop_sql sql nodes can not used on data.table arguments")
}



#' @export
ex_data_table.relop_non_sql <- function(optree,
                                        ...,
                                        tables = list(),
                                        env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_non_sql")
  stop("rquery::ex_data_table.relop_non_sql direct non-sql (function) nodes can not used on data.table arguments")
}


build_order_clause <- function(orderby, rev_orderby) {
  oterms <- character(0)
  if(length(orderby)>0) {
    oterms <- c(oterms, orderby)
  }
  if(length(rev_orderby)>0) {
    oterms <- c(oterms, paste0("-", orderev_orderbyrby))
  }
  if(length(oterms)<=0) {
    return(NULL)
  }
  paste("order(",
        paste(oterms, collapse = ", "),
        ")")
}




#' @export
ex_data_table.relop_extend <- function(optree,
                                       ...,
                                       tables = list(),
                                       env = parent.frame()) {
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_extend() requires the data.table package be installed")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_extend")
  oclause <- build_order_clause(optree$orderby, optree$rev_orderby)
  if(length(oclause)<=0) {
    oclause = ""
  }
  n <- length(optree$parsed)
  if(n<0) {
    stop("rquery::ex_data_table.relop_extend() must have at least one assignment")
  }
  x <- ex_data_table(optree$source[[1]])
  byi <- ""
  if(length(optree$partitionby)>0) {
    pterms <- paste0("\"", optree$partitionby, "\"")
    byi <- paste0(" , by = c(", paste(pterms, collapse = ", "), ")")
  }
  tmpnam <- ".rquery_ex_extend_tmp"
  tmpenv <- new.env(parent = env)
  assign(tmpnam, x, envir = tmpenv)
  enames <-
    vapply(seq_len(n),
           function(i) {
             paste0("\"", optree$parsed[[i]]$symbols_produced, "\"")
           }, character(1))
  enames <- paste0("c(", paste(enames, collapse = ", "), ")")
  eexprs <-
    vapply(seq_len(n),
           function(i) {
             gsub("^[^:]*:=[[:space:]]*", "", as.character(optree$parsed[[i]]$presentation))
           }, character(1))
  eexprs <- paste0("list(", paste(eexprs, collapse = ", "), ")")
  src <- paste0(tmpnam, "[ ",
                oclause,
                " , ", paste(enames, ":=", eexprs),
                byi,
                " ]")
  expr <- parse(text = src)
  eval(expr, envir = tmpenv, enclos = env)
}


#' @export
ex_data_table.relop_orderby <- function(optree,
                                        ...,
                                        tables = list(),
                                        env = parent.frame()) {
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_orderby() requires the data.table package be installed")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_orderby")
  x <- ex_data_table(optree$source[[1]])
  oclause <- build_order_clause(optree$orderby, optree$orderev_orderbyrby)
  if(length(oclause)<=0) {
    return(x)
  }
  tmpnam <- ".rquery_ex_orderby_tmp"
  tmpenv <- new.env(parent = env)
  assign(tmpnam, x, envir = tmpenv)
  src <- paste0(tmpnam, "[ ",
                oclause,
                " ]")
  expr <- parse(text = src)
  eval(expr, envir = tmpenv, enclos = env)
}





#  TODO: implement the following







#' @export
ex_data_table.relop_natural_join <- function(optree,
                                             ...,
                                             tables = list(),
                                             env = parent.frame()) {
  stop("rquery::ex_data_table.relop_natural_join not implemented yet") # TODO: implement
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_natural_join")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_natural_join() requires the data.table package be installed")
  }
}



#' @export
ex_data_table.relop_null_replace <- function(optree,
                                             ...,
                                             tables = list(),
                                             env = parent.frame()) {
  stop("rquery::ex_data_table.relop_null_replace not implemented yet") # TODO: implement
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_null_replace")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_null_replace() requires the data.table package be installed")
  }
}

#' @export
ex_data_table.relop_project <- function(optree,
                                        ...,
                                        tables = list(),
                                        env = parent.frame()) {
  stop("rquery::ex_data_table.relop_project not implemented yet") # TODO: implement
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_project")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_project() requires the data.table package be installed")
  }
}

#' @export
ex_data_table.relop_rename_columns <- function(optree,
                                               ...,
                                               tables = list(),
                                               env = parent.frame()) {
  stop("rquery::ex_data_table.relop_rename_columns not implemented yet") # TODO: implement
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_rename_columns")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_rename_columns() requires the data.table package be installed")
  }
}

#' @export
ex_data_table.relop_theta_join <- function(optree,
                                           ...,
                                           tables = list(),
                                           env = parent.frame()) {
  stop("rquery::ex_data_table.relop_theta_join not implemented yet") # TODO: implement
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_theta_join")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_theta_join() requires the data.table package be installed")
  }
}

#' @export
ex_data_table.relop_unionall <- function(optree,
                                         ...,
                                         tables = list(),
                                         env = parent.frame()) {
  stop("rquery::ex_data_table.relop_unionall not implemented yet") # TODO: implement
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::ex_data_table.relop_unionall")
  if(!requireNamespace("data.table", quietly = TRUE)) {
    stop("rquery::ex_data_table.relop_unionall() requires the data.table package be installed")
  }
}

