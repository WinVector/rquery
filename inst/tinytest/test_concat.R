
test_concat <- function() {

  d <- data.frame(x = 1)
  p1 <- local_td(d) %.>% extend(., x = x + 1)
  p2 <- local_td(d) %.>% extend(., x = x + 2)
  ops <- p1 %.>% p2


  d1 <- data.frame(x = 1)
  d2 <- data.frame(y = 1)
  p1 <- local_td(d1) %.>% extend(., x = x + 1)
  p2 <- local_td(d2) %.>% extend(., y = y + 2)
  expect_error({
    # expect an exception here
    ops <- p1 %.>% p2
  })

  invisible(NULL)
}
