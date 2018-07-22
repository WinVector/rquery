library("rquery")

context("select narrowing")

test_that("test_select_narrowing.R: Works As Expected", {
  db <- rquery_db_info(identifier_quote_char = "`", string_quote_char = '"')
  x <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  op1 <- local_td(x) %.>% extend_nse(., e %:=% a + 1)
  # cat(to_sql(op1, db))
  testthat::expect_equal(sort(column_names(op1)), c("a", "b", "c", "e"))
  op2 <- op1 %.>% select_columns(., "e")
  # cat(to_sql(op2, db))
  testthat::expect_equal(sort(column_names(op2)), c("e"))
})
