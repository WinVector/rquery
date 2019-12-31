
#' Make a order_expr node.
#'
#' @param source source to select from.
#' @param expr expression to order_expr in ascending order.
#' @param env environment to look for values in.
#' @return select columns node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'   optree <- order_expr_se(d, "AUC/R2")
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#' @export
#'
order_expr_se <- function(source, expr,
                          env = parent.frame()) {
  force(env)
  UseMethod("order_expr_se", source)
}

#' @export
order_expr_se.relop <- function(source, expr,
                                env = parent.frame()) {
  force(env)
  have <- column_names(source)
  parsed <- parse_se(source, expr, env = env,
                     check_names = FALSE)
  src_columns <- column_names(source)
  required_cols <- sort(unique(c(
    merge_fld(parsed, "symbols_used"),
    merge_fld(parsed, "free_symbols")
  )))
  check_have_cols(src_columns, required_cols, "rquery::order_expr_se.relop")
  assignments <- unpack_assignments(source, parsed,
                                    check_is_assignment = FALSE)
  parsed[[1]]$symbols_produced <- character(0)
  r <- list(source = list(source),
            table_name = NULL,
            parsed = parsed,
            expr = assignments,
            presentation = expr)
  r <- relop_decorate("relop_order_expr", r)
  r
}

#' @export
order_expr_se.data.frame <- function(source, expr,
                                     env = parent.frame()) {
  force(env)
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- order_expr_se(dnode, expr,
                         env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}



#' Make a order_expr node.
#'
#' order_expr() uses bquote() .()-style escaping.
#'
#' @param source source to select from.
#' @param expr expression to order_expr.
#' @param env environment to look to.
#' @return select columns node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2, z = 3))
#'   TARGETCOL = as.name("AUC")
#'   optree <- order_expr(d, .(TARGETCOL)/R2) %.>%
#'     select_columns(., "R2")
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
order_expr <- function(source, expr,
                       env = parent.frame()) {
  force(env)
  UseMethod("order_expr", source)
}

#' @rdname order_expr
#' @export
#'
order_expr_nse <- order_expr

#' @export
order_expr.relop <- function(source, expr,
                             env = parent.frame()) {
  force(env)
  exprq <- substitute(expr)
  exprq <- lapply_bquote_to_langauge_list(list(exprq), env)[[1]]
  have <- column_names(source)
  parsed <- parse_nse(source, list(exprq), env = env,
                      check_names = FALSE)
  src_columns <- column_names(source)
  required_cols <- sort(unique(c(
    merge_fld(parsed, "symbols_used"),
    merge_fld(parsed, "free_symbols")
  )))
  check_have_cols(src_columns, required_cols, "rquery::order_expr.relop")
  assignments <- unpack_assignments(source, parsed,
                                    check_is_assignment = FALSE)
  parsed[[1]]$symbols_produced <- character(0)
  r <- list(source = list(source),
            table_name = NULL,
            parsed = parsed,
            expr = assignments,
            presentation = parsed[[1]]$presentation)
  r <- relop_decorate("relop_order_expr", r)
  r
}

#' @export
order_expr.data.frame <- function(source, expr,
                                  env = parent.frame()) {
  force(env)
  exprq <- substitute(expr)
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- order_expr_se(dnode, rquery_deparse(exprq),
                         env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}




#' @export
format_node.relop_order_expr <- function(node) {
  paste0("order_expr(.,\n   ", node$presentation, ")",
         "\n")
}


calc_used_relop_order_expr <- function (x, ...,
                                        using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::calc_used_relop_order_expr")
  if(length(using)<=0) {
    using <- column_names(x)
  }
  consuming <- merge_fld(x$parsed, "symbols_used")
  using <- unique(c(using, consuming))
  missing <- setdiff(using, column_names(x$source[[1]]))
  if(length(missing)>0) {
    stop(paste("rquery::calc_used_relop_order_expr unknown columns",
               paste(missing, collapse = ", ")))
  }
  using
}

#' @export
columns_used.relop_order_expr <- function (x, ...,
                                           using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::columns_used.relop_order_expr")
  cols <- calc_used_relop_order_expr(x,
                                     using = using)
  return(columns_used(x$source[[1]],
                      using = cols))
}


#' @export
to_sql.relop_order_expr <- function (x,
                                     db,
                                     ...,
                                     limit = NULL,
                                     source_limit = NULL,
                                     indent_level = 0,
                                     tnum = mk_tmp_name_source('tsql'),
                                     append_cr = TRUE,
                                     using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::to_sql.relop_order_expr")
  dispatch_to_sql_method(
    method_name = "to_sql.relop_order_expr",
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
}

to_sql_relop_order_expr <- function(
  x,
  db,
  ...,
  limit = NULL,
  source_limit = NULL,
  indent_level = 0,
  tnum = mk_tmp_name_source('tsql'),
  append_cr = TRUE,
  using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::to_sql.relop_order_expr")
  # re-quote expr
  re_quoted <- redo_parse_quoting(x$parsed, db)
  re_expr <- unpack_assignments(x$source[[1]], re_quoted,
                                check_is_assignment = FALSE)
  # work on query
  cols <- calc_used_relop_order_expr(x,
                                     using = using)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = cols)
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT * FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab, "\n",
              prefix, "ORDER BY ",
              re_expr)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}

