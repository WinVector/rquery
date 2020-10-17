
test_NULL_NA_parse <- function() {
  td <- mk_td("dr", c("purchase_date", "product"))

  db <- rquery::rquery_default_db_info()

  ops <- td %.>%
    extend(., partitionby = "product", orderby = "purchase_date",
           z = LAG(purchase_date, 1, NULL))
  ops_s <- format(ops)
  sql_s <- to_sql(ops, db)
  expect_equal(length(grep("NULL", ops_s, fixed = TRUE)), 1)
  expect_equal(length(grep("NULL", sql_s, fixed = TRUE)), 1)
  expect_equal(length(grep("NA", ops_s, fixed = TRUE)), 0)
  expect_equal(length(grep("NA", sql_s, fixed = TRUE)), 0)

  ops <- td %.>%
    extend(., partitionby = "product", orderby = "purchase_date",
           z = LAG(purchase_date, 1, NA))
  ops_s <- format(ops)
  sql_s <- to_sql(ops, db)
  expect_equal(length(grep("NULL", ops_s, fixed = TRUE)), 0)
  expect_equal(length(grep("NULL", sql_s, fixed = TRUE)), 0)
  expect_equal(length(grep("NA", ops_s, fixed = TRUE)), 1)
  expect_equal(length(grep("NA", sql_s, fixed = TRUE)), 1)

  invisible(NULL)
}
