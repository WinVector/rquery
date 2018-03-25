
# Convert table transforms into nodes (that do not have toSQL() method).

#' Wrap a non-SQL node.
#'
#' Note: non-SQL nodes are allowed to delete/overwrite both
#' both the incoming and outgoing tables, so do not point them
#' to non-temporary structures.
#'
#' @param source source to work from (data.frame or relop node)
#' @param f implementation signature: f(db, incoming_table_name, outgoing_table_name)
#' @param ... force later arguments to bind by name
#' @param incoming_table_name character, name of incoming table
#' @param columns_used character, incoming columns used
#' @param outgoing_table_name character, name of produced table
#' @param columns_produced character, names of columns produced
#' @param display_form chacter, how to print node
#' @param orig_columns logical if TRUE select all original columns.
#' @param temporary logical, if TRUE mark tables temporary.
#' @return sql node.
#'
#' @seealso \code{\link{rsummary_node}}
#'
#' @export
#'
non_sql_node <- function(source,
                         f,
                         ...,
                         incoming_table_name,
                         columns_used,
                         outgoing_table_name,
                         columns_produced,
                         display_form,
                         orig_columns = TRUE,
                         temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node")
  UseMethod("non_sql_node", source)
}

#' @export
non_sql_node.relop <- function(source,
                               f,
                               ...,
                               incoming_table_name,
                               columns_used,
                               outgoing_table_name,
                               columns_produced,
                               display_form,
                               orig_columns = TRUE,
                               temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node.relop")
  if(is.null(f)) {
    if(incoming_table_name!=outgoing_table_name) {
      stop("non_sql_node.relop: must have incoming_table_name==outgoing_table_name when f is NULL")
    }
  } else {
    if(incoming_table_name==outgoing_table_name) {
      stop("non_sql_node.relop: must have incoming_table_name!=outgoing_table_name when f is not NULL")
    }
  }
  r <- list(source = list(source),
            table_name = outgoing_table_name,
            f = f,
            incoming_table_name = incoming_table_name,
            columns_used = columns_used,
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
                                    f,
                                    ...,
                                    incoming_table_name,
                                    columns_used,
                                    outgoing_table_name,
                                    columns_produced,
                                    display_form,
                                    orig_columns = TRUE,
                                    temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node.data.frame")
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- non_sql_node(source,
                        f,
                        incoming_table_name = incoming_table_name,
                        columns_used = columns_used,
                        outgoing_table_name = outgoing_table_name,
                        columns_produced = columns_produced,
                        display_form = display_form,
                        orig_columns = orig_columns,
                        temporary = temporary)
  return(enode)
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
format.relop_non_sql <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "format.relop_non_sql")
  paste0(trimws(format(x$source[[1]]), which = "right"),
         " %.>%\n ",
         "non_sql_node(., ", x$display_form, ")\n")
}



#' @export
columns_used.relop_non_sql <- function (x, ...,
                                        using = NULL,
                                        contract = FALSE) {
  # assume using all columns
  # TODO: improve
  return(columns_used(x$source[[1]],
                      using = NULL,
                      contract = contract))
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
  subsql <- to_sql(x$source[[1]],
                    db = db,
                    source_limit = source_limit,
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = append_cr,
                    using = NULL)
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
                    f = x$f)
  class(nsql_step) <- "rquery_non_sql_step"
  step2 <- list(nsql_step)
  qlimit = limit
  if(!getDBOption(db, "use_pass_limit", TRUE)) {
    qlimit = NULL
  }
  step3 <- list(to_sql(table_source(x$outgoing_table_name, column_names(x)),
                       db = db,
                       limit = qlimit,
                       source_limit = source_limit,
                       indent_level = indent_level + 1,
                       tnum = tnum,
                       append_cr = append_cr,
                       using = NULL))
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

