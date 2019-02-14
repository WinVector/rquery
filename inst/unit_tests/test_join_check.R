
test_join_check <- function() {

  d1 <- mk_td("d", "x")
  d2 <- mk_td("d", "x")
  ops <- natural_join(d1, d2, by = "x")

  d1 <- mk_td("d", "x")
  d2 <- mk_td("d", c("x", "y"))
  RUnit::checkException({
    # expect an exception here
    ops <- natural_join(d1, d2, by = "x")
  })

  # check same for theta_join

  d1 <- mk_td("d", "x")
  d2 <- mk_td("d", "x")
  ops <- theta_join(d1, d2, x == x)

  d1 <- mk_td("d", "x")
  d2 <- mk_td("d", c("x", "y"))
  RUnit::checkException({
    # expect an exception here
    ops <- theta_join(d1, d2, x == x)
  })

  invisible(NULL)
}
