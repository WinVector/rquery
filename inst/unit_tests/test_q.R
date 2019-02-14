
test_q <- function() {
  my_db <- rquery_default_db_info()

  d <- table_source("d", c("AUC", "R2"))
  v <- "NEWVAR"
  AUC <- "NEWVAR"
  R2 <- "NEWVAR"
  optree <- extend(d, v %:=% R2 + 1)
  str <- format(optree)
  RUnit::checkEquals(0, length(grep("NEWVAR", str, fixed = TRUE)))
  sql <- to_sql(optree, my_db)
  RUnit::checkEquals(0, length(grep("NEWVAR", sql, fixed = TRUE)))

  # should throw
  RUnit::checkException({
    d <- table_source("d", c("AUC", "R2", "z"))
    TARGETCOL = as.name("AUC")
    VALUE = 0.5
    optree <- select_rows(d, TARGETCOL >= VALUE)
  })
  # str <- format(optree)
  # RUnit::checkTrue(length(grep("TARGETCOL", str, fixed = TRUE))>0, "saw TARGETCOL")
  # sql <- to_sql(optree, my_db)
  # cat(sql)
  # RUnit::checkTrue(length(grep("TARGETCOL", sql, fixed = TRUE))>0)

  d <- table_source("d", c("AUC", "R2", "z"))
  TARGETCOL = as.name("AUC")
  VALUE = 0.5
  optree <- select_rows(d, .(TARGETCOL) >= VALUE)
  str <- format(optree)
  RUnit::checkTrue(length(grep("TARGETCOL", str, fixed = TRUE))==0, "saw TARGETCOL")
  RUnit::checkTrue(length(grep("VALUE", str, fixed = TRUE))==0, "saw VALUE")
  sql <- to_sql(optree, my_db)
  RUnit::checkTrue(length(grep("TARGETCOL", sql, fixed = TRUE))==0, "saw TARGETCOL")
  RUnit::checkTrue(length(grep("VALUE", sql, fixed = TRUE))==0, "saw VALUE")

  # should throw
  RUnit::checkException({
    d <- table_source("d", c("AUC", "R2", "z"))
    TARGETCOL = as.name("AUC")
    optree <- extend(d, AUC = TARGETCOL)
  })
  # str <- format(optree)
  # RUnit::checkTrue(length(grep("TARGETCOL", str, fixed = TRUE))>0, "saw TARGETCOL")
  # sql <- to_sql(optree, my_db)
  # cat(sql)
  # RUnit::checkTrue(length(grep("TARGETCOL", sql, fixed = TRUE))>0)

  invisible(NULL)
}
