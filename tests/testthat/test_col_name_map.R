
library("rquery")
context("build_col_name_map")



test_that("test_col_name_map: Works As Expected", {
  cmap <- rquery:::build_col_name_map(c("key", "id", "info", "key_group"),
                              c("key", "data", "key_group"),
                              c("_a", "_b"))
  testthat::expect_equal(sort(names(cmap)), c("a", "b"))
})
