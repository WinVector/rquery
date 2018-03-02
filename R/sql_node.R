

#' Make a general SQL node.
#'
#' @param source source to rename from.
#' @param exprs SQL expressions
#' @param ... force later arguments to bind by name
#' @param mods SQL modifiers (GROUP BY, ORDER BY, and so on)
#' @param orig_columns logical if TRUE select all original columns.
#' @return sql node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2, z = 3))
#' eqn <- sql_node(d, "n" := "COUNT(1)")
#' cat(format(eqn))
#' sql <- to_sql(eqn, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
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
  assignments <- paste(names(x$exprs), ":=", as.character(x$exprs))
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
  cols <- paste(as.character(x$exprs), "AS", colsA)
  if(x$orig_columns) {
    cols <- c("*", cols)
  }
  subsql <- to_sql(x$source[[1]],
                   db = db,
                   source_limit = source_limit,
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE,
                   using = NULL)
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
  q
}


