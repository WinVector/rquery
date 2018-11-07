library("rquery")

context("capture")

test_that("test_q: Works As Expected", {
  d <- table_source("d", c("AUC", "R2"))
  v <- "NEWVAR"
  optree <- extend_nse(d, v %:=% R2 + 1)
  str <- format(optree)
  testthat::expect_equal(0, length(grep("NEWVAR", str, fixed = TRUE)))
})
