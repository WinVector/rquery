
#' Make a rename columns node (copies columns not renamed).
#'
#' @param source source to rename from.
#' @param cmap map written as new column names as keys and old column names as values.
#' @param env environment to look to.
#' @return rename columns node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2, z = 3))
#'   op_tree <- rename_columns(d, c('R2' %:=% 'AUC', 'AUC' %:=% 'R2'))
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
rename_columns <- function(source, cmap,
                           env = parent.frame()) {
  force(env)
  UseMethod("rename_columns", source)
}

#' @export
rename_columns.relop <- function(source, cmap,
                                 env = parent.frame()) {
  force(env)
  if(length(cmap)<=0) {
    return(source)
  }
  if(length(cmap)!=length(unique(as.character(cmap)))) {
    stop("rquery::rename_columns map values must be unique")
  }
  if(length(cmap)!=length(unique(names(cmap)))) {
    stop("rquery::rename_columns map keys must be unique")
  }
  have <- column_names(source)
  check_have_cols(have, as.character(cmap),
                  "rquery::rename_columns cmap")
  collisions <- intersect(names(cmap),
                          setdiff(have, as.character(cmap)))
  if(length(collisions)>0) {
    stop(paste("rquery::rename_columns rename collisions",
               paste(collisions, collapse = ", ")))
  }
  cmap <- cmap[names(cmap)!=cmap]
  if(length(cmap)<=0) {
    return(source)
  }
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            cmap = cmap)
  r <- relop_decorate("relop_rename_columns", r)
  r
}

#' @export
rename_columns.data.frame <- function(source, cmap,
                                      env = parent.frame()) {
  force(env)
  if(length(cmap)<=0) {
    stop("rquery::rename_columns must rename at least 1 column")
  }
  if(length(cmap)!=length(unique(as.character(cmap)))) {
    stop("rquery::rename_columns map values must be unique")
  }
  if(length(cmap)!=length(unique(names(cmap)))) {
    stop("rquery::rename_columns map keys must be unique")
  }
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- rename_columns(dnode, cmap, env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}


#' @export
column_names.relop_rename_columns <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::column_names.relop_rename_columns")
  sc <- column_names(x$source[[1]])
  rmap <- names(x$cmap)
  names(rmap) <- as.character(x$cmap)
  sc[sc %in% names(rmap)] <- rmap[sc[sc %in% names(rmap)]]
  sc
}



#' @export
format_node.relop_rename_columns <- function(node) {
  paste0("rename(.,\n",
         "  ", gsub("\n", "\n  ",
                    wrapr::map_to_char(node$cmap, sep = "\n  "),
                    fixed = TRUE), ")",
         "\n")
}


calc_used_relop_rename_columns <- function (x,
                                            ...,
                                            using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::calc_used_relop_rename_columns")
  cols <- column_names(x)
  if(length(using)>0) {
    missing <- setdiff(using, cols)
    if(length(missing)>0) {
      stop(paste("rquery::calc_used_relop_rename_columns unknown columns",
                 paste(missing, collapse = ", ")))
    }
    cols <- intersect(cols, using)
  }
  # map back prior to rename
  rmap <- x$cmap
  sc <- cols
  sc[sc %in% names(rmap)] <- rmap[sc[sc %in% names(rmap)]]
  names(sc) <- cols
  sc
}

#' @export
columns_used.relop_rename_columns <- function (x,
                                               ...,
                                               using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::columns_used.relop_rename_columns")
  qmap <- calc_used_relop_rename_columns(x, using=using)
  return(columns_used(x$source[[1]],
                      using = as.character(qmap)))
}


#' @export
to_sql.relop_rename_columns <- function (x,
                                         db,
                                         ...,
                                         limit = NULL,
                                         source_limit = NULL,
                                         indent_level = 0,
                                         tnum = mk_tmp_name_source('tsql'),
                                         append_cr = TRUE,
                                         using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::to_sql.relop_rename_columns")
  dispatch_to_sql_method(
    method_name = "to_sql.relop_rename_columns",
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
}

to_sql_relop_rename_columns <- function(
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
                          "rquery::to_sql.relop_rename_columns")
  qmap <- calc_used_relop_rename_columns(x, using=using)
  colsV <- vapply(as.character(qmap),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  colsA <- vapply(names(qmap),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  cols <- paste(colsV, "AS", colsA)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        limit = limit,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = as.character(qmap))
  subsql <- subsql_list[[length(subsql_list)]]
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



