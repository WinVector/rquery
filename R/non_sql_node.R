
# convert table transforms into nodes (that do not have toSQL() method).

#' Wrap a non-SQL node.
#'
#' @param source source to work from.
#' @param f implementation signature: f(db, incoming_table_name, outgoing_table_name)
#' @param incoming_table_name
#' @param columns_used
#' @param outgoing_table_name
#' @param columns_produced
#' @param ... force later arguments to bind by name
#' @param orig_columns logical if TRUE select all original columns.
#' @return sql node.
#'
#'
#' @noRd
#'
non_sql_node <- function(source,
                         f,
                         incoming_table_name,
                         columns_used,
                         outgoing_table_name,
                         columns_produced,
                         ...,
                         orig_columns = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node")
  UseMethod("non_sql_node", source)
}

#' @noRd
non_sql_node.relop <- function(source,
                               f,
                               incoming_table_name,
                               columns_used,
                               outgoing_table_name,
                               columns_produced,
                               ...,
                               orig_columns = TRUE) {
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
            orig_columns = orig_columns)
  r <- relop_decorate("relop_non_sql", r)
  r
}

#' @noRd
non_sql_node.data.frame <- function(source,
                                    f,
                                    incoming_table_name,
                                    columns_used,
                                    outgoing_table_name,
                                    columns_produced,
                                    ...,
                                    orig_columns = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "non_sql_node.data.frame")
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- non_sql_node(source,
                        f,
                        incoming_table_name = incoming_table_name,
                        columns_used = columns_used,
                        outgoing_table_name = outgoing_table_name,
                        columns_produced =  columns_produced,
                        orig_columns = orig_columns)
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
  if(!is.null(x$display_form)) {
    str <- paste0(trimws(format(x$source[[1]]), which = "right"),
                  " %.>%\n ",
                  "non_sql_node(.,\n",
                  "          ", x$display_form,
                  ")\n")
    return(str)
  }
  assignments <- paste(names(x$exprs), ":=", as.character(x$exprs))
  modsstr <- ""
  indent_sep <- "\n             "
  if(!is.null(x$mods)) {
    modsstr <- paste(";\n          ", x$mods)
  }
  paste0(trimws(format(x$source[[1]]), which = "right"),
         " %.>%\n ",
         "non_sql_node(.,\n",
         "          ", paste(assignments, collapse = indent_sep),
         modsstr,
         ",", indent_sep, "*=", x$orig_columns,
         ")\n")
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
  # TODO: temp control to here
  step1 <- list(paste0("CREATE TABLE ",
                       quote_identifier(db, x$incoming_table_name),
                       " AS ",
                       subsql[[nsubsql]]))
  step2 <- NULL
  if(!is.null(x$f)) {
    step2 <- list(text = paste0(quote_identifier(db, x$outgoing_table_name),
                                " <- ",
                                x$node_description),
                  f = x$f)
  }
  step3 <- list(to_sql(table_source(x$outgoing_table_name, column_names(x)),
                       db = db,
                       source_limit = source_limit,
                       indent_level = indent_level + 1,
                       tnum = tnum,
                       append_cr = append_cr,
                       using = NULL))
  c(subsql[-length(subsql)], step1, step2, step3)
}


