
#' Make a drop columns node (not a relational operation).
#'
#' Note: must keep at least one column.
#'
#' @param source source to drop columns from.
#' @param drops list of distinct column names.
#' @param ... force later arguments to bind by name
#' @param strict logical, if TRUE do check columns to be dropped are actually present.
#' @param env environment to look to.
#' @return drop columns node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'   optree <- drop_columns(d, 'AUC')
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
drop_columns <- function(source, drops,
                         ...,
                         strict = TRUE,
                         env = parent.frame()) {
  force(env)
  UseMethod("drop_columns", source)
}

#' @export
drop_columns.relop <- function(source, drops,
                               ...,
                               strict = TRUE,
                               env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::drop_columns.relop")
  if(length(drops)<=0) {
    return(source)
  }
  have <- column_names(source)
  if(strict) {
    check_have_cols(have, drops, "rquery::drop_columns drops")
  }
  removing <- intersect(drops, have)
  if(length(removing)<=0) {
    return(source)
  }
  remain <- setdiff(have, drops)
  if(length(remain)<=0) {
    stop("rquery::drop_columns.relop must keep at least one column")
  }
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            strict = strict,
            drops = drops,
            columns = setdiff(column_names(source), drops))
  r <- relop_decorate("relop_drop_columns", r)
  r
}

#' @export
drop_columns.data.frame <- function(source, drops,
                                    ...,
                                    strict = TRUE,
                                    env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::drop_columns.data.frame")
  if(length(drops)<=0) {
    return(source)
  }
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- drop_columns(dnode, drops, strict = strict)
  rquery_apply_to_data_frame(source, enode, env = env)
}


#' @export
column_names.relop_drop_columns <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::column_names.relop_drop_columns")
  x$columns
}

#' @export
format_node.relop_drop_columns <- function(node) {
  paste0("drop_columns(.,\n   ",
         wrapr::map_to_char(node$drops),
         ")",
         "\n")
}


calc_using_relop_drop_columns <- function(x, ...,
                                          using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::calc_using_relop_drop_columns")
  cols <- x$columns
  if(length(using)>0) {
    missing <- setdiff(using, x$columns)
    if(length(missing)>0) {
      stop(paste("rquery:columns_used request for unknown columns",
                 paste(missing, collapse = ", ")))
    }
    cols <- intersect(cols, using)
  }
  cols
}

#' @export
columns_used.relop_drop_columns <- function (x, ...,
                                             using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::columns_used.relop_drop_columns")
  cols <- calc_using_relop_drop_columns(x,
                                        using = using)
  return(columns_used(x$source[[1]],
                      using = cols))
}

#' @export
to_sql.relop_drop_columns <- function (x,
                                       db,
                                       ...,
                                       limit = NULL,
                                       source_limit = NULL,
                                       indent_level = 0,
                                       tnum = mk_tmp_name_source('tsql'),
                                       append_cr = TRUE,
                                       using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::to_sql.relop_drop_columns")
  dispatch_to_sql_method(
    method_name = "to_sql.relop_drop_columns",
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
}


to_sql_relop_drop_columns <- function(
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
                          "rquery::to_sql_relop_drop_columns")
  using <- calc_using_relop_drop_columns(x,
                                         using = using)
  qlimit = limit
  if(!getDBOption(db, "use_pass_limit", TRUE)) {
    qlimit = NULL
  }
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        limit = qlimit,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = using)
  subsql <- subsql_list[[length(subsql_list)]]
  cols <- vapply(x$columns,
                 function(ci) {
                   quote_identifier(db, ci)
                 }, character(1))
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", paste(cols, collapse = paste0(",\n", prefix, " ")), "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}



