
test_partial_fn_parse <- function() {
  d <- data.frame(x = c(1, NA), g = c(1, 1))

  expect_error(
    d %.>%
      project(.,
                  x = mean(x, na.rm = TRUE),
                  groupby = c())
  )

  mean2 <- function(x) (mean(x, na.rm = TRUE))

  ops <- local_td(d) %.>%
    project(.,
                x = mean2(x),
                groupby = c())
  sql <- to_sql(ops, rquery_default_db_info())
  # cat(format(sql))

  # variation on
  # http://www.win-vector.com/blog/2018/08/collecting-expressions-in-r/
  d <- data.frame(AUC = 0.6, R2 = 0.2)
  exprs <- list()
  exprs <- c(exprs, "v" %:=% "AUC + R2")
  exprs <- c(exprs, "x" %:=% "pmax(AUC,v)")
  ops <- extend_se(local_td(d), exprs)
  # cat(format(ops))
  sql <- to_sql(ops, rquery_default_db_info())
  # cat(format(sql))

  invisible(NULL)
}
