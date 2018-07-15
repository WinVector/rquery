
# Convert table transforms into nodes (that do not have toSQL() method).

#' Wrap a non-SQL node.
#'
#' Note: non-SQL nodes are allowed to delete/overwrite both
#' both the incoming and outgoing tables, so do not point them
#' to non-temporary structures.  Also they tend to land all columns
#' (losing narrowing optimization),
#' so can be expensive and should be used sparingly.
#'
#' @param source source to work from (data.frame or relop node)
#' @param ... force later arguments to bind by name
#' @param f_db database implementation signature: f_db(db, incoming_table_name, outgoing_table_name) (db being a database handle)
#' @param f_df data.frame implementation signature: f_df(data.frame) (NULL defaults to taking from database).
#' @param incoming_table_name character, name of incoming table
#' @param outgoing_table_name character, name of produced table
#' @param columns_produced character, names of additional columns produced
#' @param display_form character, how to print node
#' @param pass_using logical, if TRUE (or if f_db is NULL) pass using column calculations through (else assume using all columns).
#' @param orig_columns logical if TRUE select all original columns.
#' @param temporary logical, if TRUE mark tables temporary.
#' @param env environment to look to.
#' @return sql node.
#'
#' @seealso \code{\link{rsummary_node}}, \code{\link{quantile_node}}, \code{\link{materialize_node}}
#'
#' @export
#'
non_sql_node <- function(source,
                         ...,
                         f_db,
                         f_df = NULL,
                         incoming_table_name,
                         outgoing_table_name,
                         columns_produced,
                         display_form,
                         pass_using = FALSE,
                         orig_columns = TRUE,
                         temporary = TRUE,
                         env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node")
  UseMethod("non_sql_node", source)
}

#' @export
non_sql_node.relop <- function(source,
                               ...,
                               f_db,
                               f_df = NULL,
                               incoming_table_name,
                               outgoing_table_name,
                               columns_produced,
                               display_form,
                               pass_using = FALSE,
                               orig_columns = TRUE,
                               temporary = TRUE,
                               env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node.relop")
  if(is.null(f_db)) {
    if(incoming_table_name!=outgoing_table_name) {
      stop("non_sql_node.relop: must have incoming_table_name==outgoing_table_name when f_db is NULL")
    }
  } else {
    if(incoming_table_name==outgoing_table_name) {
      stop("non_sql_node.relop: must have incoming_table_name!=outgoing_table_name when f_db is not NULL")
    }
  }
  r <- list(source = list(source),
            table_name = outgoing_table_name,
            f_db = f_db,
            f_df = f_df,
            pass_using = pass_using || is.null(f_db),
            incoming_table_name = incoming_table_name,
            outgoing_table_name = outgoing_table_name,
            columns_produced = columns_produced,
            display_form = display_form,
            orig_columns = orig_columns,
            overwrite = TRUE,
            temporary = temporary)
  r <- relop_decorate("relop_non_sql", r)
  r
}

#' @export
non_sql_node.data.frame <- function(source,
                                    ...,
                                    f_db,
                                    f_df = NULL,
                                    incoming_table_name,
                                    outgoing_table_name,
                                    columns_produced,
                                    display_form,
                                    pass_using = FALSE,
                                    orig_columns = TRUE,
                                    temporary = TRUE,
                                    env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node.data.frame")
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- non_sql_node(source,
                        f_db = f_db,
                        f_df = f_df,
                        incoming_table_name = incoming_table_name,
                        outgoing_table_name = outgoing_table_name,
                        columns_produced = columns_produced,
                        display_form = display_form,
                        pass_using = pass_using,
                        orig_columns = orig_columns,
                        temporary = temporary)
  rquery_apply_to_data_frame(source, enode, env = env)
}




#' @export
column_names.relop_non_sql <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "column_names.relop_non_sql")
  nms <- x$columns_produced
  if(x$orig_columns) {
    nms <- c(nms, column_names(x$source[[1]]))
  }
  nms
}



#' @export
format_node.relop_non_sql <- function(node) {
  paste0("non_sql_node(., ", node$display_form, ")\n")
}



#' @export
columns_used.relop_non_sql <- function (x, ...,
                                        using = NULL) {
  usingQ <- NULL
  if(x$pass_using) {
    usingQ <- using
  }
  return(columns_used(x$source[[1]],
                      using = usingQ))
}


#' @export
to_sql.relop_non_sql <- function (x,
                                  db,
                                  ...,
                                  limit = NULL,
                                  source_limit = NULL,
                                  indent_level = 0,
                                  tnum = mk_tmp_name_source('tsql'),
                                  append_cr = TRUE,
                                  using = NULL) {
  usingQ <- NULL
  if(x$pass_using) {
    usingQ <- using
  }
  subsql <- to_sql(x$source[[1]],
                    db = db,
                    source_limit = source_limit,
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = append_cr,
                    using = usingQ)
  nsubsql <- length(subsql)
  # non-SQL nodes must always be surrounded by SQL on both sides
  step1 <- materialize_sql_statement(db,
                                     subsql[[nsubsql]],
                                     x$incoming_table_name,
                                     temporary = x$temporary)
  nsql_step <- list(display_form = x$display_form,
                    incoming_table_name = x$incoming_table_name,
                    outgoing_table_name = x$outgoing_table_name,
                    temporary = x$temporary,
                    f = x$f_db)
  class(nsql_step) <- "rquery_non_sql_step"
  step2 <- list(nsql_step)
  qlimit = limit
  if(!getDBOption(db, "use_pass_limit", TRUE)) {
    qlimit = NULL
  }
  step3 <- list(to_sql(mk_td(x$outgoing_table_name, column_names(x)),
                       db = db,
                       limit = qlimit,
                       source_limit = source_limit,
                       indent_level = indent_level + 1,
                       tnum = tnum,
                       append_cr = append_cr,
                       using = usingQ))
  c(subsql[-length(subsql)], step1, step2, step3)
}



#' @export
format.rquery_non_sql_step <- function(x, ...) {
  paste("non SQL step: ", x$display_form)
}

#' @export
print.rquery_non_sql_step <- function(x, ...) {
  print(format(x))
}


#' Cache results to a named table inside a pipeline.
#'
#'
#' @param source source to work from (data.frame or relop node)
#' @param table_name character, name of caching table
#' @param ... force later arguments to bind by name
#' @param temporary logical, if TRUE mark tables temporary.
#' @return sql node.
#'
#' @seealso \code{\link{rsummary_node}}, \code{\link{non_sql_node}}
#'
#' @export
#'
materialize_node <- function(source,
                             table_name,
                             ...,
                             temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "materialize_node")
  non_sql_node(source = source,
               f_db = NULL,
               f_df = function(x) { x },
               incoming_table_name = table_name,
               outgoing_table_name = table_name,
               columns_produced = NULL,
               display_form = paste0("materialize_node(", table_name, ")"),
               orig_columns = TRUE,
               temporary = temporary)
}

