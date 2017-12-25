library("rquery")
context("Parsing")



test_that("test_parse: Works As Expected", {
  zs <- 'q'
  zn <- 5

  do_parse <- function(text, env= parent.frame()) {
    ei <- parse(text = text)[[1]]
    p <- parse_for_SQL(ei,
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
  expect_equal("'c1'", ex2$check)
  expect_equal("c1", ex2$symbols_used)

  ex3 <- do_parse("x1")
  expect_equal("x1", ex3$check)
  expect_equal(character(0), ex3$symbols_used)

  ex4 <- do_parse("'c1'")
  expect_equal('"c1"', ex4$check)

  ex5 <- do_parse("zs")
  expect_equal('"q"', ex5$check)

  ex6 <- do_parse("zn")
  expect_equal("5", ex6$check)

  ex7 <- do_parse("1+1")
  expect_equal("1 + 1", ex7$check)

  ex8 <- do_parse("(1+1)*3")
  expect_equal("( 1 + 1 ) * 3", ex8$check)

  ex9 <- do_parse("!TRUE")
  expect_equal("( NOT ( TRUE ) )", ex9$check)

  ex10 <- do_parse("1==2")
  expect_equal("1 = 2", ex10$check)

  ex11 <- do_parse("ifelse(TRUE,1,2)")
  expect_equal("( CASE WHEN ( TRUE ) THEN ( 1 ) ELSE ( 2 ) END )", ex11$check)

  ex12 <- do_parse("sin(x)")
  expect_equal("sin ( x )", ex12$check)

  ex13 <- do_parse("x := 1+1")
  expect_equal("1 + 1", ex13$check)
  expect_equal("x", ex13$symbols_produced)
})
