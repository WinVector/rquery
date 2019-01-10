library("rquery")

context("mod")

test_that("test_mod: Works As Expected", {
  my_db <- rquery_default_db_info()

  td <- mk_td("data", "x")
  ops <- extend(td, xm = x %% 2)
  sql <- to_sql(ops, my_db)
  #cat(sql)
  testthat::expect_equal(1, grep("MOD", sql, fixed = TRUE))
})
