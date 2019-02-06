
library("rquery")

context("self_join")



test_that("test_self_join: Works As Expected", {
  dA <- mk_td("dA", c("x", "y"))
  dB <- mk_td("dB", c("x", "y"))
  testthat::expect_equal(c("x", "y"), column_names(dA))
  testthat::expect_equal(c("x", "y"), colnames(dA))
  d1 <- natural_join(dA, dA, by = "x", jointype = "LEFT")
  testthat::expect_equal(2, length(column_names(d1)))
  d2 <- natural_join(dA, dB, by = "x", jointype = "LEFT")
  testthat::expect_equal(3, length(column_names(d2)))
})
