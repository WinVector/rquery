
flatten_with_sep <- function(list_of_lists, sep_list) {
  nl <- length(list_of_lists)
  if(nl<1) {
    return(list())
  }
  if(nl==1) {
    return(list_of_lists[[1]])
  }
  r <- vector(2*nl-1, mode = "list")
  r[seq_len(length(r))] <- sep_list
  r[2*seq_len(nl)-1] <- list_of_lists
  r <- unlist(r, recursive = FALSE)
  r
}


#' Count NULLs per row for given column set.
#'
#' Build a query that counts the number of nulls in each row.
#'
#' @param source incoming rel_op tree or data.frame.
#' @param cols character, columns to track
#' @param count character, column to write count in.
#' @return rel_op node or data.frame (depending on input).
#'
#' @seealso \code{\link{null_replace}}, \code{\link{mark_null_cols}}
#'
#' @examples
#'
#' # WARNING: example tries to change rquery.rquery_db_executor option to RSQLite and back.
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   RSQLite::initExtension(my_db)
#'   old_o <- options(list("rquery.rquery_db_executor" = list(db = my_db)))
#'
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = c(0.6, 0.5, NA),
#'                               R2 = c(1.0, 0.9, NA)))
#'   op_tree <- d %.>% count_null_cols(., c("AUC", "R2"), "nnull")
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   # ad-hoc mode
#'   data.frame(AUC=c(1,NA,0.5), R2=c(NA,1,0)) %.>%
#'      op_tree %.>%
#'      print(.)
#'
#'   # cleanup
#'   options(old_o)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
count_null_cols <- function(source, cols, count) {
  nc <- length(cols)
  if(nc<1) {
    cols <- column_names(source)
  } else {
    bad_cols <- setdiff(cols, column_names(source))
    if(length(bad_cols)>0) {
      stop(paste("rquery::count_null_cols unknown columns:",
                 paste(bad_cols, collapse = ", ")))
    }
  }
  if(count %in% column_names(source)) {
    stop("rquery::count_null_cols count column can not be an existing column")
  }
  terms <- lapply(cols,
                  function(ci) {
                    list("( CASE WHEN (",
                         as.name(ci),
                         "IS NULL ) THEN 1 ELSE 0 END )")
                  })
  expr <- flatten_with_sep(terms, list("+"))
  nd <- sql_node(source, count %:=% list(expr),
                 orig_columns = TRUE)
  if("relop" %in% class(nd)) {
    nd$display_form <- paste0("count_null_cols(",
                              paste(cols, collapse = ", "),
                              ")")
  }
  nd
}


#' Indicate NULLs per row for given column set.
#'
#' Build a query that creates NULL indicators for nulls in selected columns.
#'
#' @param source incoming rel_op tree or data.frame.
#' @param cols named character, values are columns to track, names are where to land indicators.
#' @return rel_op node or data.frame (depending on input).
#'
#' @seealso \code{\link{null_replace}}, \code{\link{count_null_cols}}
#'
#' @examples
#'
#' # WARNING: example tries to change rquery.rquery_db_executor option to RSQLite and back.
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   RSQLite::initExtension(my_db)
#'   old_o <- options(list("rquery.rquery_db_executor" = list(db = my_db)))
#'
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = c(0.6, 0.5, NA),
#'                               R2 = c(1.0, 0.9, NA)))
#'   op_tree <- d %.>% mark_null_cols(., qc(AUC_NULL, R2_NULL) %:=%
#'                                      qc(AUC, R2))
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   # ad-hoc mode
#'   data.frame(AUC=c(1,NA,0.5), R2=c(NA,1,0)) %.>%
#'      op_tree %.>%
#'      print(.)
#'
#'   # cleanup
#'   options(old_o)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
mark_null_cols <- function(source, cols) {
  if((length(cols)<=0) || (length(names(cols))<=0)) {
    stop("rquery::mark_null_cols cost must be a named column list")
  }
  if(length(intersect(names(cols), column_names(source)))>0) {
    stop("mark_null_cols: new names can not intersect column names")
  }
  nc <- length(cols)
  if(nc<1) {
    cols <- column_names(source)
  } else {
    bad_cols <- setdiff(as.character(cols), column_names(source))
    if(length(bad_cols)>0) {
      stop(paste("rquery::mark_null_cols unknown columns:",
                 paste(bad_cols, collapse = ", ")))
    }
  }
  terms <- lapply(cols,
                  function(ci) {
                    list(as.name(ci),
                         "IS NULL")
                  })
  names(terms) <- names(cols)
  nd <- sql_node(source, terms,
           orig_columns = TRUE)
  if("relop" %in% class(nd)) {
    nd$display_form <- paste0("mark_null_cols(",
                              wrapr::map_to_char(cols),
                              ")")
  }
  nd
}

