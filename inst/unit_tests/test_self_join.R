
test_self_join <- function() {
  dA <- mk_td("dA", c("x", "y"))
  dB <- mk_td("dB", c("x", "y"))
  RUnit::checkEquals(c("x", "y"), column_names(dA))
  RUnit::checkEquals(c("x", "y"), colnames(dA))
  d1 <- natural_join(dA, dA, by = "x", jointype = "LEFT")
  RUnit::checkEquals(2, length(column_names(d1)))
  d2 <- natural_join(dA, dB, by = "x", jointype = "LEFT")
  RUnit::checkEquals(2, length(column_names(d2)))

  invisible(NULL)
}

