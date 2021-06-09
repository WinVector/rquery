
test_self_join <- function() {
  dA <- mk_td("dA", c("x", "y"))
  dB <- mk_td("dB", c("x", "y"))
  expect_equal(c("x", "y"), column_names(dA))
  expect_equal(c("x", "y"), colnames(dA))
  d1 <- natural_join(dA, dA, by = "x", jointype = "LEFT")
  expect_equal(2, length(column_names(d1)))
  d2 <- natural_join(dA, dB, by = "x", jointype = "LEFT")
  expect_equal(2, length(column_names(d2)))

  invisible(NULL)
}

test_self_join()
