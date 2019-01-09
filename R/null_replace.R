

#' Create a null_replace node.
#'
#' Replace NA/NULL is specified columns with the given replacement value.
#'
#' @param src relop or data.frame data source.
#' @param cols character, columns to work on.
#' @param value scalar, value to write.
#' @param ... force later arguments to bind by name.
#' @param note_col character, if not NULL record number of columns altered per-row in this column.
#' @param env environment to look to.
#' @return null_replace node or data.frame.
#'
#' @seealso \code{\link{count_null_cols}}, \code{\link{mark_null_cols}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d1 <- rq_copy_to(my_db, 'd1',
#'                     data.frame(A = c(NA, 2, 3, NA), B = c(3, NA, 4, NA)))
#'   optree <- null_replace(d1, qc(A, B),
#'                          0.0, note_col = "alterations")
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
null_replace <- function(src,
                         cols,
                         value,
                         ...,
                         note_col = NULL,
                         env = parent.frame()) {
  force(env)
  UseMethod("null_replace", src)
}

#' @export
null_replace.relop <- function(src,
                               cols,
                               value,
                               ...,
                               note_col = NULL,
                               env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::null_replace.relop")
  if(length(sort(unique(cols)))!=length(cols)) {
    stop("rquery::null_replace.relop bad cols argument")
  }
  if(length(value)!=1) {
    stop("rquery::null_replace.relop value must be a scalar")
  }
  bads <- setdiff(cols, column_names(src))
  if(length(bads)>0) {
    stop(paste("rquery::null_replace.relop unknown columns",
               paste(bads, collapse = ", ")))
  }
  if(length(note_col)>0) {
    if(length(note_col)!=1) {
      stop("rquery::null_replace.relop note_col must be length 1 character")
    }
    if(length(intersect(note_col, column_names(src)>0))) {
      stop("rquery::null_replace.relop note_col must not intersect with existing columns")
    }
  }
  r <- list(source = list(src),
            table_name = NULL,
            cols = cols,
            value = value,
            note_col = note_col,
            parsed = NULL)
  r <- relop_decorate("relop_null_replace", r)
  r
}

#' @export
null_replace.data.frame <- function(src,
                                    cols,
                                    value,
                                    ...,
                                    note_col = NULL,
                                    env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::null_replace.data.frame")
  nmgen <- mk_tmp_name_source("rquery_tmp")
  tmp_namea <- nmgen()
  dnodea <- mk_td(tmp_namea, colnames(src))
  enode <- null_replace(dnodea,
                        cols = cols,
                        value = value,
                        note_col = note_col,
                        env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}


#' @export
format_node.relop_null_replace <- function(node) {
  cstr <- ifelse(is.null(node$note_col),
                         "",
                         paste("; ", node$note_col))
  paste0("null_replace(.; ",
         paste(node$cols, collapse = ",\n  "),
         ": ",
         node$value,
         cstr,
         ")",
         "\n")
}

#' @export
column_names.relop_null_replace <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::column_names.relop_null_replace")
  c(column_names(x$source[[1]], x$note_col))
}


#' @export
columns_used.relop_null_replace <- function (x, ...,
                                             using = NULL) {
  columns_used(x$source[[1]])
}


#' @export
to_sql.relop_null_replace <- function (x,
                                       db,
                                       ...,
                                       limit = NULL,
                                       source_limit = NULL,
                                       indent_level = 0,
                                       tnum = mk_tmp_name_source('tsql'),
                                       append_cr = TRUE,
                                       using = NULL) {
  if(length(list(...))>0) {
    stop("rquery::to_sql.relop_null_replace unexpected arguments")
  }
  dispatch_to_sql_method(
    method_name = "to_sql.relop_null_replace",
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
}
