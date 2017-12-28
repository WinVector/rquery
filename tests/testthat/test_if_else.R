library("rquery")
context("if_else")



test_that("test_extend_partition: Works As Expected", {

  d <- table_source("d",
                    c("a_1", "a_2", "a_3"  ))
  dQ <- d %.>%
    extend_se(., "a_4" := "ifelse(! a_1, a_2, a_3)")
  txt <- format(dQ)
  expect_true(length(grep("!", txt, fixed = TRUE))>0)
})
