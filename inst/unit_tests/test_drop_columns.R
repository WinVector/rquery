

test_relop_drop_columns <- function() {
  dL <- data.frame(x = 1, y = 2, z = 3)
  rquery_pipeline <- local_td(dL) %.>%
    drop_columns(., "y")
  cols <- column_names(rquery_pipeline)
  RUnit::checkEquals(c("x", "z"), sort(column_names(rquery_pipeline)))

  invisible(NULL)
}


test_relop_drop_columns_not_strict <- function() {
  rquery_pipeline <- mk_td(table_name = 'd', columns = c('x', 'q')) %.>%
    drop_columns(., c('q', 'z'))
  if(requireNamespace('rqdatatable', quietly = TRUE)) {
    res <- data.frame(x=1, y=2, q=3, z=4) %.>% rquery_pipeline
    RUnit::checkEquals(c("x"), sort(colnames(res)))
    res <- data.frame(x=1, y=2) %.>% rquery_pipeline
    RUnit::checkEquals(c("x"), sort(colnames(res)))
  }

  invisible(NULL)
}
