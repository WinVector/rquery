
#' Make a select columns node (not a relational operation).
#'
#' @param source source to select from.
#' @param columns list of distinct column names.
#' @param env environment to look to.
#' @return select columns node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'   optree <- select_columns(d, 'AUC')
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
select_columns <- function(source, columns, env = parent.frame()) {
  force(env)
  UseMethod("select_columns", source)
}

#' @export
select_columns.relop <- function(source, columns, env = parent.frame()) {
  force(env)
  if(length(columns)<=0) {
    stop("rquery::select_columns must select at least 1 column")
  }
  have <- column_names(source)
  check_have_cols(have, columns, "rquery::select_columns columns")
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            columns = columns)
  r <- relop_decorate("relop_select_columns", r)
  r
}

#' @export
select_columns.data.frame <- function(source, columns, env = parent.frame()) {
  force(env)
  if(length(columns)<=0) {
    stop("rquery::select_columns must select at least 1 column")
  }
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- select_columns(dnode, columns, env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}




#' @export
column_names.relop_select_columns <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  x$columns
}

#' @export
format_node.relop_select_columns <- function(node) {
  paste0("select_columns(.,\n   ",
         paste(node$columns, collapse = ", "),
         ")",
         "\n")
}


calc_using_relop_select_columns <- function(x, ...,
                                            using = NULL) {
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
columns_used.relop_select_columns <- function (x, ...,
                                               using = NULL) {
  cols <- calc_using_relop_select_columns(x,
                                          using = using)
  return(columns_used(x$source[[1]],
                      using = cols))
}

#' @export
to_sql.relop_select_columns <- function (x,
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
  using <- calc_using_relop_select_columns(x,
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



