

#' Count NULLs per row for given column set.
#'
#' Build a query that counts the number of nulls in each row.
#'
#' @param source incoming rel_op tree or data.frame.
#' @param cols character, columns to track
#' @param count character, column to write count in.
#' @return rel_op node or data.frame (depending on input).
#'
#' @seealso \code{\link{mark_null_cols}}, \code{\link{replace_null_cols}}
#'
#' @examples
#'
#'  my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'  winvector_temp_db_handle <- list(
#'    db = DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'  )
#'  RSQLite::initExtension(winvector_temp_db_handle$db)
#'
#'  d <- dbi_copy_to(my_db, 'd',
#'                   data.frame(AUC = c(0.6, 0.5, NA),
#'                              R2 = c(1.0, 0.9, NA)))
#'  op_tree <- d %.>% count_null_cols(., c("AUC", "R2"), "nnull")
#'  cat(format(op_tree))
#'  sql <- to_sql(op_tree, my_db)
#'  cat(sql)
#'  DBI::dbGetQuery(my_db, sql)
#'
#'  # ad-hoc mode
#'  data.frame(AUC=c(1,NA,0.5), R2=c(NA,1,0)) %.>% op_tree
#'
#'  # cleanup
#'  rm(list = "winvector_temp_db_handle")
#'  DBI::dbDisconnect(my_db)
#'
#' @export
#'
count_null_cols <- function(source, cols, count) {
  terms <- paste0("( CASE WHEN ( ",
                  cols,
                  " IS NULL ) THEN 1 ELSE 0 END )")
  expr <- paste0(paste(terms, collapse = " + "))
  sql_node(source, count := expr,
           orig_columns = TRUE)
}


#' Indicate NULLs per row for given column set.
#'
#' Build a query that creates NULL indicators for nulls in slected columns.
#'
#' @param source incoming rel_op tree or data.frame.
#' @param cols named character, values are columns to track, names are where to land indicators.
#' @return rel_op node or data.frame (depending on input).
#'
#' @seealso \code{\link{count_null_cols}}, \code{\link{replace_null_cols}}
#'
#' @examples
#'
#'  my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'  winvector_temp_db_handle <- list(
#'    db = DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'  )
#'  RSQLite::initExtension(winvector_temp_db_handle$db)
#'
#'  d <- dbi_copy_to(my_db, 'd',
#'                   data.frame(AUC = c(0.6, 0.5, NA),
#'                              R2 = c(1.0, 0.9, NA)))
#'  op_tree <- d %.>% mark_null_cols(., qc(AUC_NULL, R2_NULL) :=
#'                                      qc(AUC, R2))
#'  cat(format(op_tree))
#'  sql <- to_sql(op_tree, my_db)
#'  cat(sql)
#'  DBI::dbGetQuery(my_db, sql)
#'
#'  # ad-hoc mode
#'  data.frame(AUC=c(1,NA,0.5), R2=c(NA,1,0)) %.>% op_tree
#'
#'  # cleanup
#'  rm(list = "winvector_temp_db_handle")
#'  DBI::dbDisconnect(my_db)
#'
#' @export
#'
mark_null_cols <- function(source, cols) {
  if(length(intersect(names(cols), as.character(cols)))>0) {
    stop("mark_null_cols: names can not intersect values")
  }
  terms <- paste0(as.character(cols), " IS NULL")
  sql_node(source, names(cols) := terms,
           orig_columns = TRUE)
}

#' Replace NULLs per row for given column set.
#'
#' Build a query that replaces NULL values in selected columns.
#'
#' @param source incoming rel_op tree or data.frame.
#' @param cols character, columns to replace NULLs in.
#' @param val relpacement value.
#' @return rel_op node or data.frame (depending on input).
#'
#' @seealso \code{\link{count_null_cols}}, \code{\link{mark_null_cols}}
#'
#' @examples
#'
#'  my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'  winvector_temp_db_handle <- list(
#'    db = DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'  )
#'  RSQLite::initExtension(winvector_temp_db_handle$db)
#'
#'  d <- dbi_copy_to(my_db, 'd',
#'                   data.frame(AUC = c(NA, 0.5, NA),
#'                              R2 = c(1.0, 0.9, NA)))
#'  op_tree <- d %.>% replace_null_cols(., qc(AUC, R2),
#'                                         0.0)
#'  cat(format(op_tree))
#'  sql <- to_sql(op_tree, my_db)
#'  cat(sql)
#'  DBI::dbGetQuery(my_db, sql)
#'
#'  # ad-hoc mode
#'  data.frame(AUC=c(1,NA,0.5), R2=c(NA,1,0)) %.>% op_tree
#'
#'  # cleanup
#'  rm(list = "winvector_temp_db_handle")
#'  DBI::dbDisconnect(my_db)
#'
#' @export
#'
replace_null_cols <- function(source, cols, val) {
  if(length(intersect(names(cols), as.character(cols)))>0) {
    stop("replace_null_cols: names can not intersect values")
  }
  source_cols <- column_names(source)
  others <- setdiff(source_cols, as.character(cols))
  names(others) <- others
  terms <- paste0("CASE WHEN  ",
                  as.character(cols),
                  " IS NULL THEN ",
                  val,
                  " ELSE ",
                  as.character(cols),
                  " END")
  names(terms) <- cols
  sql_node(source, c(terms, others),
           orig_columns = FALSE)
}

