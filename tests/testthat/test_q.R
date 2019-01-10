library("rquery")

context("capture")

test_that("test_q: Works As Expected", {
  my_db <- rquery_default_db_info()

  d <- table_source("d", c("AUC", "R2"))
  v <- "NEWVAR"
  AUC <- "NEWVAR"
  R2 <- "NEWVAR"
  optree <- extend(d, v %:=% R2 + 1)
  str <- format(optree)
  testthat::expect_equal(0, length(grep("NEWVAR", str, fixed = TRUE)))
  sql <- to_sql(optree, my_db)
  testthat::expect_equal(0, length(grep("NEWVAR", sql, fixed = TRUE)))

  # should throw
  testthat::expect_error({
    d <- table_source("d", c("AUC", "R2", "z"))
    TARGETCOL = as.name("AUC")
    VALUE = 0.5
    optree <- select_rows(d, TARGETCOL >= VALUE)
  }, ".*")
  # str <- format(optree)
  # testthat::expect(length(grep("TARGETCOL", str, fixed = TRUE))>0, "saw TARGETCOL")
  # sql <- to_sql(optree, my_db)
  # cat(sql)
  # testthat::expect(length(grep("TARGETCOL", sql, fixed = TRUE))>0)

  d <- table_source("d", c("AUC", "R2", "z"))
  TARGETCOL = as.name("AUC")
  VALUE = 0.5
  optree <- select_rows(d, .(TARGETCOL) >= VALUE)
  str <- format(optree)
  testthat::expect(length(grep("TARGETCOL", str, fixed = TRUE))==0, "saw TARGETCOL")
  testthat::expect(length(grep("VALUE", str, fixed = TRUE))==0, "saw VALUE")
  sql <- to_sql(optree, my_db)
  testthat::expect(length(grep("TARGETCOL", sql, fixed = TRUE))==0, "saw TARGETCOL")
  testthat::expect(length(grep("VALUE", sql, fixed = TRUE))==0, "saw VALUE")

  # should throw
  testthat::expect_error({
    d <- table_source("d", c("AUC", "R2", "z"))
    TARGETCOL = as.name("AUC")
    optree <- extend(d, AUC = TARGETCOL)
  }, ".*")
  # str <- format(optree)
  # testthat::expect(length(grep("TARGETCOL", str, fixed = TRUE))>0, "saw TARGETCOL")
  # sql <- to_sql(optree, my_db)
  # cat(sql)
  # testthat::expect(length(grep("TARGETCOL", sql, fixed = TRUE))>0)
})
