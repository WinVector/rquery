


#' project data by grouping, and adding aggregate columns.
#'
#' @param source source to select from.
#' @param ... force later arugments to bind by name.
#' @param groupby grouping columns.
#' @param parsed new column assignment expressions.
#' @return project node.
#'
#' @noRd
#'
project_impl <- function(source, ...,
                         groupby, parsed) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::project_impl")
  if(length(groupby)!=length(unique(groupby))) {
    stop("rquery:::project_impl duplicatge groupby columns")
  }
  have <- column_names(source)
  required_cols <- sort(unique(c(
    merge_fld(parsed, "symbols_used"),
    merge_fld(parsed, "free_symbols"),
    groupby
  )))
  check_have_cols(have, required_cols, "rquery::project")
  parts <- partition_assignments(parsed)
  if(length(parts)>1) {
    stop(paste("rquery:::project_impl can not use produced column names during a project"))
  }
  assignments <- unpack_assignments(source, parsed)
  # producing <- names(assignments)
  # overlap <- intersect(have, producing)
  # if(length(overlap)>0) {
  #   stop(paste("rquery:::project_impl produced columns must be disjoint from incoming table: ",
  #              paste(overlap, collapse = ", ")))
  # }
  r <- list(source = list(source),
            table_name = NULL,
            parsed = parsed,
            groupby = groupby,
            columns = c(groupby, names(assignments)),
            required_cols = required_cols,
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
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'   d <- rq_copy_to(
#'     my_db, 'd',
#'     data.frame(group = c('a', 'a', 'b', 'b'),
#'                val = 1:4,
#'                stringsAsFactors = FALSE))
#'
#'   op_tree <- d %.>%
#'     project_se(., groupby = "group", "vmax" %:=% "max(val)")
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   execute(my_db, op_tree) %.>%
#'      print(.)
#'
#'   op_tree <- d %.>%
#'     project_se(., groupby = NULL, "vmax" %:=% "max(val)")
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
  force(env)
  UseMethod("project_se", source)
}

#' @export
#' @rdname project_se
aggregate_se <- project_se

#' @export
project_se.relop <- function(source, groupby, assignments,
                             env = parent.frame()) {
  force(env)
  parsed <- parse_se(source, assignments, env = env)
  project_impl(source, groupby = groupby, parsed = parsed)
}

#' @export
project_se.data.frame <- function(source, groupby, assignments,
                                  env = parent.frame()) {
  force(env)
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- project_se(dnode, groupby, assignments,
                      env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}




#' project data by grouping, and adding aggregate columns.
#'
#' project() uses bquote() .()-style escaping.
#'
#' @param source source to select from.
#' @param groupby grouping columns.
#' @param ... new column assignment expressions.
#' @param env environment to look for values in.
#' @return project node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'   d <- rq_copy_to(
#'     my_db, 'd',
#'     data.frame(group = c('a', 'a', 'b', 'b'),
#'                val = 1:4,
#'                stringsAsFactors = FALSE))
#'
#'   op_tree <- d %.>%
#'     project(., groupby = "group", vmax %:=% max(val))
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   execute(my_db, op_tree) %.>%
#'      print(.)
#'
#'   op_tree <- d %.>%
#'     project(., groupby = NULL, vmax %:=% max(val))
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
project <- function(source, groupby, ...,
                        env = parent.frame()) {
  force(env)
  UseMethod("project", source)
}

#' @rdname project
#' @export
#'
project_nse <- project


#' @export
#' @rdname project
aggregate_nse <- project

#' @export
project.relop <- function(source, groupby, ...,
                              env = parent.frame()) {
  force(env)
  # Recommend way to caputre ... unevalauted from
  # http://adv-r.had.co.nz/Computing-on-the-language.html#substitute "Capturing unevaluated ..."
  exprs <-  eval(substitute(alist(...)))
  exprs <- lapply_bquote_to_langauge_list(exprs, env)
  parsed <- parse_nse(source, exprs, env = env)
  project_impl(source, groupby = groupby, parsed = parsed)
}

#' @export
project.data.frame <- function(source, groupby, ...,
                                   env = parent.frame()) {
  force(env)
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- project(dnode, groupby, ...,
                       env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}


#' @export
column_names.relop_project <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  x$columns
}


#' @export
format_node.relop_project <- function(node) {
  origTerms <- vapply(node$parsed,
                      function(pi) {
                        paste(as.character(pi$presentation), collapse = ' ')
                      }, character(1))
  aterms <- paste(origTerms, collapse = ", ")
  paste0("project(., ",
         aterms,
         ",\n  g= ",
         paste(node$groupby, collapse = ", "),
         ")",
         "\n")
}

calc_used_relop_project <- function (x,
                                     using = NULL) {
  expressions <- x$parsed
  if(length(using)>0) {
    want_expr <- vapply(x$parsed,
                        function(pi) {
                          length(intersect(pi$symbols_produced, using))>0
                        }, logical(1))
    expressions <- x$parsed[want_expr]
  }
  consuming <- merge_fld(expressions, "symbols_used")
  subusing <- unique(c(consuming, x$groupby, x$orderby))
  subusing
}

#' @export
columns_used.relop_project <- function (x, ...,
                                        using = NULL) {
  if(length(list(...))>0) {
    stop("rquery:columns_used: unexpected arguments")
  }
  cols <- calc_used_relop_project(x,
                                  using = using)
  columns_used(x$source[[1]],
               using = cols)
}


#' @export
to_sql.relop_project <- function (x,
                                  db,
                                  ...,
                                  limit = NULL,
                                  source_limit = NULL,
                                  indent_level = 0,
                                  tnum = mk_tmp_name_source('tsql'),
                                  append_cr = TRUE,
                                  using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  # re-quote expr
  parsed <- x$parsed
  if(length(using)>0) {
    want_expr <- vapply(x$parsed,
                        function(pi) {
                          length(intersect(pi$symbols_produced, using))>0
                        }, logical(1))
    parsed <- x$parsed[want_expr]
  }
  re_quoted <- redo_parse_quoting(parsed, db)
  re_assignments <- unpack_assignments(x$source[[1]], re_quoted)
  # work on query
  using_incoming <- calc_used_relop_project(x,
                                   using = using)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = using_incoming)
  subsql <- subsql_list[[length(subsql_list)]]
  grouping_cols <- x$groupby
  if(length(x$groupby)>0) {
    grouping_cols <- vapply(x$groupby,
                            function(ci) {
                              quote_identifier(db, ci)
                            }, character(1))
  }
  extra_cols <- x$groupby
  if(length(using)>0) {
    extra_cols <- intersect(x$groupby, using)
  }
  if(length(extra_cols)>0) {
    extra_cols <- vapply(extra_cols,
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
              paste(c(extra_cols, derived), collapse = ", "),
              " FROM (\n",
              subsql, "\n",
              prefix, " ) ", tab)
  if(length(grouping_cols)>0) {
    q <- paste0(q,
                "\n",
                prefix, "GROUP BY\n",
                prefix, " ", paste(grouping_cols, collapse = ", "))
  }
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}
