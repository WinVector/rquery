
# Convert table transforms into nodes (that do not have toSQL() method).

#' Wrap a non-SQL node.
#'
#' Note: non-SQL nodes are allowed to delete/overwrite both
#' both the incoming and outgoing tables, so do not point them
#' to non-temporary structures.  Also they tend to land all columns
#' (losing narrowing optimization),
#' so can be expensive and should be used sparingly.  Finally their
#' result can only be used once in a pipeline (else they will try to clobber their own result).
#'
#' @param source source to work from (data.frame or relop node)
#' @param ... force later arguments to bind by name
#' @param f_db database implementation signature: f_db(db, incoming_table_name, outgoing_table_name, nd, ...) (db being a database handle)
#' @param f_df data.frame implementation signature: f_df(data.frame, nd) (NULL defaults to taking from database).
#' @param f_dt data.table implementation signature: f_dt(data.table, nd) (NULL defaults f_df).
#' @param incoming_table_name character, name of incoming table
#' @param incoming_qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @param outgoing_table_name character, name of produced table
#' @param outgoing_qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @param columns_produced character, names of additional columns produced
#' @param display_form character, how to print node
#' @param orig_columns logical if TRUE select all original columns.
#' @param temporary logical, if TRUE mark tables temporary.
#' @param check_result_details logical, if TRUE enforce result type and columns.
#' @param env environment to look to.
#' @return non-sql node.
#'
#' @seealso \code{\link{rsummary_node}}, \code{\link{quantile_node}}
#'
#' @export
#'
non_sql_node <- function(source,
                         ...,
                         f_db = NULL,
                         f_df = NULL,
                         f_dt = NULL,
                         incoming_table_name,
                         incoming_qualifiers = NULL,
                         outgoing_table_name,
                         outgoing_qualifiers = NULL,
                         columns_produced,
                         display_form = 'non_sql_node',
                         orig_columns = TRUE,
                         temporary = TRUE,
                         check_result_details = TRUE,
                         env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node")
  force(env)
  UseMethod("non_sql_node", source)
}

#' @export
non_sql_node.relop <- function(source,
                               ...,
                               f_db = NULL,
                               f_df = NULL,
                               f_dt = NULL,
                               incoming_table_name,
                               incoming_qualifiers = NULL,
                               outgoing_table_name,
                               outgoing_qualifiers = NULL,
                               columns_produced,
                               display_form = 'non_sql_node',
                               orig_columns = TRUE,
                               temporary = TRUE,
                               check_result_details = TRUE,
                               env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node.relop")
  force(env)
  if(is.null(f_db)) {
    if(incoming_table_name!=outgoing_table_name) {
      stop("non_sql_node.relop: must have incoming_table_name==outgoing_table_name when f_db is NULL")
    }
  } else {
    if(incoming_table_name==outgoing_table_name) {
      stop("non_sql_node.relop: must have incoming_table_name!=outgoing_table_name when f_db is not NULL")
    }
  }
  src_cols <- column_names(source)
  r <- list(source = list(source),
            table_name = outgoing_table_name,
            f_db = f_db,
            f_df = f_df,
            f_dt = f_dt,
            incoming_table_name = incoming_table_name,
            incoming_qualifiers = incoming_qualifiers,
            outgoing_table_name = outgoing_table_name,
            outgoing_qualifiers = outgoing_qualifiers,
            columns_produced = columns_produced,
            display_form = display_form,
            orig_columns = orig_columns,
            overwrite = TRUE,
            src_cols = src_cols,
            temporary = temporary,
            check_result_details = check_result_details,
            allow_narrowing = (length(columns_produced)==0) && orig_columns)
  r <- relop_decorate("relop_non_sql", r)
  r
}

#' @export
non_sql_node.data.frame <- function(source,
                                    ...,
                                    f_db = NULL,
                                    f_df = NULL,
                                    f_dt = NULL,
                                    incoming_table_name,
                                    incoming_qualifiers = NULL,
                                    outgoing_table_name,
                                    outgoing_qualifiers = NULL,
                                    columns_produced,
                                    display_form = 'non_sql_node',
                                    orig_columns = TRUE,
                                    temporary = TRUE,
                                    check_result_details = TRUE,
                                    env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node.data.frame")
  force(env)
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- non_sql_node(dnode,
                        f_db = f_db,
                        f_df = f_df,
                        f_dt = f_dt,
                        incoming_table_name = incoming_table_name,
                        incoming_qualifiers = incoming_qualifiers,
                        outgoing_table_name = outgoing_table_name,
                        outgoing_qualifiers = outgoing_qualifiers,
                        columns_produced = columns_produced,
                        display_form = display_form,
                        orig_columns = orig_columns,
                        temporary = temporary,
                        check_result_details = check_result_details,
                        env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}




#' @export
column_names.relop_non_sql <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "column_names.relop_non_sql")
  nms <- x$columns_produced
  if(x$orig_columns) {
    nms <- c(nms, setdiff(column_names(x$source[[1]]), nms))
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
  if(!x$allow_narrowing) {
    using <- NULL
  }
  columns_used(x$source[[1]],
               using = using)
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
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::to_sql.relop_non_sql")
  dispatch_to_sql_method(
    method_name = "to_sql.relop_non_sql",
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
}



to_sql_relop_non_sql <- function(
  x,
  db,
  ...,
  limit = NULL,
  source_limit = NULL,
  indent_level = 0,
  tnum = mk_tmp_name_source('tsql'),
  append_cr = TRUE,
  using = NULL) {
  if(!x$allow_narrowing) {
    using <- NULL
  }
  subsql <- to_sql(x$source[[1]],
                   db = db,
                   source_limit = source_limit,
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = append_cr,
                   using = using)
  nsubsql <- length(subsql)
  # non-SQL nodes must always be surrounded by SQL on both sides
  step1 <- materialize_sql_statement(db,
                                     subsql[[nsubsql]],
                                     x$incoming_table_name,
                                     qualifiers = x$incoming_qualifiers,
                                     temporary = x$temporary)
  nsql_step <- list(display_form = x$display_form,
                    incoming_table_name = x$incoming_table_name,
                    incoming_qualifiers = x$incoming_qualifiers,
                    outgoing_table_name = x$outgoing_table_name,
                    outgoing_qualifiers = x$outgoing_qualifiers,
                    temporary = x$temporary,
                    node = x,
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
                       using = using))
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


