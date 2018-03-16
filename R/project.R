


#' project data by grouping, and adding aggregate columns.
#'
#' @param source source to select from.
#' @param groupby grouping columns.
#' @param parsed new column assignment expressions.
#' @return project node.
#'
#' @noRd
#'
project_impl <- function(source, groupby, parsed) {
  have <- column_names(source)
  check_have_cols(have, groupby, "rquery::project groupby")
  assignments <- unpack_assignments(source, parsed)
  producing <- names(assignments)
  overlap <- intersect(have, producing)
  if(length(overlap)>0) {
    stop(paste("rquery:::project_impl produced columns must be disjoint from incoming table: ",
               paste(overlap, collapse = ", ")))
  }
  r <- list(source = list(source),
            table_name = NULL,
            parsed = parsed,
            groupby = groupby,
            columns = c(groupby, names(assignments)),
            assignments = assignments)
  r <- relop_decorate("relop_project", r)
  r
}

#' project data by grouping, and adding aggregate columns.
#'
#' @param source source to select from.
#' @param groupby grouping columns.
#' @param assignments new column assignment expressions.
#' @param env environment to look for values in.
#' @return project node.
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'   d <- dbi_copy_to(
#'     my_db, 'd',
#'     data.frame(group = c('a', 'a', 'b', 'b'),
#'                val = 1:4,
#'                stringsAsFactors = FALSE))
#'
#'   op_tree <- d %.>%
#'     project_se(., "group", "vmax" := "max(val)")
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   execute(my_db, op_tree) %.>%
#'      print(.)
#'
#'   op_tree <- d %.>%
#'     project_se(., NULL, "vmax" := "max(val)")
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   execute(my_db, op_tree) %.>%
#'      print(.)
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
project_se <- function(source, groupby, assignments,
                       env = parent.frame()) {
  UseMethod("project_se", source)
}

#' @export
project_se.relop <- function(source, groupby, assignments,
                             env = parent.frame()) {
  parsed <- parse_se(source, assignments, env = env)
  project_impl(source, groupby, parsed)
}

#' @export
project_se.data.frame <- function(source, groupby, assignments,
                                  env = parent.frame()) {
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- project_se(dnode, groupby, assignments,
                      env = env)
  return(enode)
}




#' project data by grouping, and adding aggregate columns.
#'
#' @param source source to select from.
#' @param groupby grouping columns.
#' @param ... new column assignment expressions.
#' @param env environment to look for values in.
#' @return project node.
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'   d <- dbi_copy_to(
#'     my_db, 'd',
#'     data.frame(group = c('a', 'a', 'b', 'b'),
#'                val = 1:4,
#'                stringsAsFactors = FALSE))
#'
#'   op_tree <- d %.>%
#'     project_nse(., "group", vmax := max(val))
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   execute(my_db, op_tree) %.>%
#'      print(.)
#'
#'   op_tree <- d %.>%
#'     project_nse(., NULL, vmax := max(val))
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   execute(my_db, op_tree) %.>%
#'     print(.)
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
project_nse <- function(source, groupby, ...,
                        env = parent.frame()) {
  UseMethod("project_nse", source)
}

#' @export
project_nse.relop <- function(source, groupby, ...,
                        env = parent.frame()) {
  exprs <-  eval(substitute(alist(...)))
  parsed <- parse_nse(source, exprs, env = env)
  project_impl(source, groupby, parsed)
}

#' @export
project_nse.data.frame <- function(source, groupby, ...,
                                   env = parent.frame()) {
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- project_nse(dnode, groupby, ...,
                       env = env)
  return(enode)
}


#' @export
column_names.relop_project <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  x$columns
}


#' @export
format.relop_project <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  origTerms <- vapply(x$parsed,
                      function(pi) {
                        paste(as.character(pi$presentation), collapse = ' ')
                      }, character(1))
  aterms <- paste(origTerms, collapse = ", ")
  paste0(trimws(format(x$source[[1]]), which="right"),
         " %.>%\n ",
         "project(., ",
         aterms,
         ",\n  g= ",
         paste(x$groupby, collapse = ", "),
         ")",
         "\n")
}

calc_used_relop_project <- function (x,
                                     using = NULL,
                                     contract = FALSE) {
  cols <- column_names(x)
  if(length(using)>0) {
    cols <- using
  }
  producing <- merge_fld(x$parsed, "symbols_produced")
  expressions <- x$parsed
  if(contract) {
    expressions <- x$parsed[producing %in% cols]
  }
  cols <- setdiff(cols, producing)
  consuming <- merge_fld(expressions, "symbols_used")
  subusing <- unique(c(cols, consuming, x$groupby, x$orderby))
  subusing
}

#' @export
columns_used.relop_project <- function (x, ...,
                                        using = NULL,
                                        contract = FALSE) {
  if(length(list(...))>0) {
    stop("rquery:columns_used: unexpected arguments")
  }
  cols <- calc_used_relop_project(x,
                                  using = using,
                                  contract = contract)
  columns_used(x$source[[1]],
               using = cols,
               contract = contract)
}


#' @export
to_sql.relop_project <- function (x,
                                  db,
                                  ...,
                                  source_limit = NULL,
                                  indent_level = 0,
                                  tnum = mk_tmp_name_source('tsql'),
                                  append_cr = TRUE,
                                  using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  # re-quote expr
  re_quoted <- redo_parse_quoting(x$parsed, db)
  re_assignments <- unpack_assignments(x$source[[1]], re_quoted)
  # work on query
  using <- calc_used_relop_project(x,
                                   using = using)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = using)
  subsql <- subsql_list[[length(subsql_list)]]
  cols1 <- x$groupby
  cols <- NULL
  if(length(cols1)>0) {
    cols <- vapply(cols1,
                   function(ci) {
                     quote_identifier(db, ci)
                   }, character(1))
  }
  derived <- NULL
  if(length(re_assignments)>0) {
    derived <- vapply(names(re_assignments),
                      function(ni) {
                        ei <- re_assignments[[ni]]
                        paste(ei, "AS", quote_identifier(db, ni))
                      }, character(1))
  }
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT ",
              paste(c(cols, derived), collapse = ", "),
              " FROM (\n",
              subsql, "\n",
              prefix, " ) ", tab)
  if(length(cols)>0) {
    q <- paste0(q,
               "\n",
               prefix, "GROUP BY\n",
               prefix, " ", paste(cols, collapse = ", "))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}
