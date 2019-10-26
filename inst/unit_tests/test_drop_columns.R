

test_relop_drop_columns <- function() {

  dL <- data.frame(x = 1, y = 2, z = 3)
  rquery_pipeline <- local_td(dL) %.>%
    drop_columns(., "y")
  cols <- column_names(rquery_pipeline)
  RUnit::checkEquals(c("x", "z"), sort(column_names(rquery_pipeline)))

  invisible(NULL)
}
