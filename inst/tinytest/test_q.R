
test_q <- function() {
  my_db <- rquery_default_db_info()

  d <- table_source("d", c("AUC", "R2"))
  v <- "NEWVAR"
  AUC <- "NEWVAR"
  R2 <- "NEWVAR"
  optree <- extend(d, v %:=% R2 + 1)
  str <- format(optree)
  expect_equal(0, length(grep("NEWVAR", str, fixed = TRUE)))
  sql <- to_sql(optree, my_db)
  expect_equal(0, length(grep("NEWVAR", sql, fixed = TRUE)))

  d <- table_source("d", c("AUC", "R2", "z"))
  TARGETCOL = as.name("AUC")
  VALUE = 0.5
  optree <- select_rows(d, .(TARGETCOL) >= VALUE)
  str <- format(optree)
  expect_true(length(grep("TARGETCOL", str, fixed = TRUE))==0, "saw TARGETCOL")
  expect_true(length(grep("VALUE", str, fixed = TRUE))==0, "saw VALUE")
  sql <- to_sql(optree, my_db)
  expect_true(length(grep("TARGETCOL", sql, fixed = TRUE))==0, "saw TARGETCOL")
  expect_true(length(grep("VALUE", sql, fixed = TRUE))==0, "saw VALUE")

  invisible(NULL)
}

test_q()

