library("rquery")
context("Parsing")



test_that("test_parse: Works As Expected", {
  zs <- 'q'
  zn <- 5

  do_parse <- function(text, env= parent.frame()) {
    ei <- parse(text = text)[[1]]
    p <- parse_for_SQL(ei,
                  contextname = "table",
                  colnames = c("c1", "c2"),
                  env = env)
    ftoks <- vapply(p$parsed,
                     function(pi) {
                       format(pi)
                       }, character(1))
    p$check <- trimws(paste(ftoks, collapse = " "), which = "both")
    p
  }

  ex1 <- do_parse("1")
  expect_equal("1", ex1$check)

  ex2 <- do_parse("c1")
  expect_equal("'table'.'c1'", ex2$check)

  ex3 <- do_parse("x1")
  expect_equal("x1", ex3$check)

  ex4 <- do_parse("'c1'")
  expect_equal('"c1"', ex4$check)

  ex5 <- do_parse("zs")
  expect_equal('"q"', ex5$check)

  ex6 <- do_parse("zn")
  expect_equal("5", ex6$check)

  ex7 <- do_parse("1+1")
  expect_equal("1 + 1", ex7$check)
})
