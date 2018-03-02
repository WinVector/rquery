

#' Count NULLs per row for given column set.
#'
#' Build a query that counts the number of nulls in each row.
#'
#' @param source incoming rel_op tree or data.frame.
#' @param cols character, columns to track
#' @param count character, column to write count in.
#' @return rel_op node or data.frame (depending on input).
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
#'
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
  sql_node(source, count := expr)
}
