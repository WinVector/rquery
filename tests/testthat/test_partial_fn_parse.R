library("rquery")
context("test_partial_fn_parse")



test_that("test_partial_fn_parse.R", {
  d <- data.frame(x = c(1, NA), g = c(1, 1))

  testthat::expect_error(
    d %.>%
      project_nse(.,
                  x = mean(x, na.rm = TRUE),
                  groupby = c())
  )

  mean2 <- function(x) (mean(x, na.rm = TRUE))

  local_td(d) %.>%
    project_nse(.,
                x = mean2(x),
                groupby = c())
})
