
test_extend_shrink <- function() {
  d <- data.frame(x = c(1, 2, 3))

  ops1 <- local_td(d) %.>%
    extend(., y = 1) %.>%
    extend(., z = 2)

  expect_true("relop_table_source" %in% class(ops1$source[[1]]))

  ops2 <- local_td(d) %.>%
    extend(., y = 1) %.>%
    extend(., y = 2)

  expect_true("relop_table_source" %in% class(ops2$source[[1]]))

  ops3 <- local_td(d) %.>%
    extend(., y = 1) %.>%
    extend(., y = y + 1)

  expect_true("relop_extend" %in% class(ops3$source[[1]]))

  invisible(NULL)
}
