
test_col_name_map <- function() {
  cmap <- rquery:::build_col_name_map(c("key", "id", "info", "key_group"),
                              c("key", "data", "key_group"),
                              c("_a", "_b"))
  expect_equal(sort(names(cmap)), c("a", "b"))

  invisible(NULL)
}

test_col_name_map()
