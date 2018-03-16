

#' Make a general SQL node.
#'
#' @param source source to work from.
#' @param exprs SQL expressions
#' @param ... force later arguments to bind by name
#' @param mods SQL modifiers (GROUP BY, ORDER BY, and so on)
#' @param orig_columns logical if TRUE select all original columns.
#' @return sql node.
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   # example database connection
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'   # load up example data
#'   d <- dbi_copy_to(
#'     my_db, 'd',
#'     data.frame(v1 = c(1, 2, NA, 3),
#'                v2 = c(NA, "b", NA, "c"),
#'                v3 = c(NA, NA, 7, 8),
#'                stringsAsFactors = FALSE))
#'
#'   # look at table
#'   execute(my_db, d)
#'
#'   # get list of columns
#'   vars <- column_names(d)
#'   print(vars)
#'
#'   # build a NA/NULLs per-row counting expression.
#'   # names are "quoted" by wrapping them with as.name().
#'   # constants can be quoted by an additional list wrapping.
#'   expr <- lapply(vars,
#'                  function(vi) {
#'                    list("+ (CASE WHEN (",
#'                         as.name(vi),
#'                         "IS NULL ) THEN 1.0 ELSE 0.0 END)")
#'                  })
#'   expr <- unlist(expr, recursive = FALSE)
#'   expr <- c(list(0.0), expr)
#'   cat(paste(unlist(expr), collapse = " "))
#'
#'   # instantiate the operator node
#'   op_tree <- d %.>%
#'     sql_node(., "num_missing" := list(expr))
#'   cat(format(op_tree))
#'
#'   # examine produced SQL
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'
#'   # execute
#'   execute(my_db, op_tree) %.>%
#'      print(.)
#'
#'   # whole process wrapped in convenience node
#'   op_tree2 <- d %.>%
#'     count_null_cols(., vars, "nnull")
#'   execute(my_db, op_tree2) %.>%
#'     print(.)
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
sql_node <- function(source, exprs,
                     ...,
                     mods = NULL,
                     orig_columns = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sql_node")
  UseMethod("sql_node", source)
}

#' @export
sql_node.relop <- function(source, exprs,
                           ...,
                           mods = NULL,
                           orig_columns = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sql_node.relop")
  names_used <- Filter(is.name, unlist(exprs,
                                       recursive = TRUE,
                                       use.names = FALSE))
  names_used <- sort(unique(as.character(names_used)))
  undef <- setdiff(names_used, column_names(source))
  if(length(undef)>0) {
    stop(paste("rquery::sql_node.relop undefined columns:",
               paste(undef, collapse = ", ")))
  }
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            exprs = exprs,
            mods = mods,
            orig_columns = orig_columns)
  r <- relop_decorate("relop_sql", r)
  r
}

#' @export
sql_node.data.frame <- function(source, exprs,
                                ...,
                                mods = NULL,
                                orig_columns = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sql_node.data.frame")
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- sql_node(dnode, exprs = exprs,
                    mods = mods,
                    orig_columns = orig_columns)
  return(enode)
}




#' @export
column_names.relop_sql <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "column_names.relop_sql")
  nms <- names(x$exprs)
  if(x$orig_columns) {
    nms <- c(nms, column_names(x$source[[1]]))
  }
  nms
}



#' @export
format.relop_sql <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "format.relop_sql")
  if(!is.null(x$display_form)) {
    str <- paste0(trimws(format(x$source[[1]]), which = "right"),
           " %.>%\n ",
           "sql_node(.,\n",
           "          ", x$display_form,
           ")\n")
    return(str)
  }
  exprtxt <- vapply(x$exprs,
                    function(ei) {
                      paste(as.character(ei), collapse = " ")
                    }, character(1))
  assignments <- paste(names(x$exprs), ":=", exprtxt)
  modsstr <- ""
  indent_sep <- "\n             "
  if(!is.null(x$mods)) {
    modsstr <- paste(";\n          ", x$mods)
  }
  paste0(trimws(format(x$source[[1]]), which = "right"),
         " %.>%\n ",
         "sql_node(.,\n",
         "          ", paste(assignments, collapse = indent_sep),
         modsstr,
         ",", indent_sep, "*=", x$orig_columns,
         ")\n")
}



#' @export
columns_used.relop_sql <- function (x, ...,
                                    using = NULL,
                                    contract = FALSE) {
  # assume using all columns
  return(columns_used(x$source[[1]],
                      using = NULL,
                      contract = contract))
}


# assemble SQL from list of strings (treated as is),
# names (treated as SQL column names), and
# lists (first value used, if character treated as SQL constant).
prep_sql_toks <- function(db, ei) {
  eiq <- vapply(ei,
                function(eij) {
                  if(is.list(eij)) {
                    eij <- eij[[1]]
                    if(is.character(eij)) {
                      return(quote_string(db, eij))
                    }
                  }
                  if(is.name(eij)) {
                    return(quote_identifier(db, as.character(eij)))
                  }
                  return(as.character(eij))
                }, character(1))
  paste(eiq, collapse = " ")
}

#' @export
to_sql.relop_sql <- function (x,
                              db,
                              ...,
                              source_limit = NULL,
                              indent_level = 0,
                              tnum = mk_tmp_name_source('tsql'),
                              append_cr = TRUE,
                              using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "to_sql.relop_sql")
  colsA <- vapply(names(x$exprs),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  sqlexprs <- vapply(x$exprs,
                     function(ei) {
                       prep_sql_toks(db, ei)
                     }, character(1))
  cols <- paste(sqlexprs, "AS", colsA)
  if(x$orig_columns) {
    cols <- c("*", cols)
  }
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = NULL)
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  star_str <- ""
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", paste(cols, collapse = paste0(",\n", prefix, " ")), "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(!is.null(x$mods)) {
    q <- paste(q, x$mods)
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}


