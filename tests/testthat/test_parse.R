library("rquery")
context("Parsing")



test_that("test_parse: Works As Expected", {
  zs <- 'q'
  zn <- 5

  do_parse <- function(text, env= parent.frame()) {
    ei <- parse(text = text)[[1]]
    p <- tokenize_for_SQL(ei,
                  colnames = c("c1", "c2"),
                  env = env)
    p$check <- pre_sql_to_query(p$parsed_toks, rquery::rquery_default_db_info())
    p
  }

  ex1 <- do_parse("1")
  testthat::expect_equal("1", ex1$check)

  ex2 <- do_parse("c1")
  testthat::expect_equal('"c1"', ex2$check)
  testthat::expect_equal("c1", ex2$symbols_used)

  ex3 <- do_parse("x1")
  testthat::expect_equal("x1", ex3$check)
  testthat::expect_equal(character(0), ex3$symbols_used)

  ex4 <- do_parse('"c1"')
  testthat::expect_equal("'c1'", ex4$check)

  ex5 <- do_parse("zs")
  testthat::expect_equal("'q'", ex5$check)

  ex6 <- do_parse("zn")
  testthat::expect_equal("5", ex6$check)

  ex7 <- do_parse("1+2")
  testthat::expect_equal("1 + 2", ex7$check)

  ex8 <- do_parse("(1+2)*3")
  testthat::expect_equal("( 1 + 2 ) * 3", ex8$check)

  ex9 <- do_parse("!TRUE")
  testthat::expect_equal("( NOT ( TRUE ) )", ex9$check)

  ex10 <- do_parse("1==2")
  testthat::expect_equal("1 = 2", ex10$check)

  ex11 <- do_parse("ifelse(TRUE,1,2)")
  testthat::expect_equal("( CASE WHEN ( TRUE ) THEN ( 1 ) WHEN NOT ( TRUE ) THEN ( 2 ) ELSE NULL END )", ex11$check)

  ex12 <- do_parse("sin(x)")
  testthat::expect_equal("sin ( x )", ex12$check)

  ex13 <- do_parse("x := 1+1")
  testthat::expect_equal("1 + 1", ex13$check)
  testthat::expect_equal("x", ex13$symbols_produced)

  ex13b <- do_parse("x := 1+1")
  testthat::expect_equal("1 + 1", ex13b$check)
  testthat::expect_equal("x", ex13b$symbols_produced)

  ex14 <- do_parse("rank := rank")
  testthat::expect_equal("rank", ex14$check)
  testthat::expect_equal("rank", ex14$symbols_produced)

  ex15 <- do_parse("rank := rank()")
  testthat::expect_equal("rank ( )", ex15$check)
  testthat::expect_equal("rank", ex15$symbols_produced)

  ex17 <- do_parse("x" %:=% "exp(3 * 5)")
  testthat::expect_equal("exp ( 3 * 5 )", ex17$check)

  ex18 <- do_parse("y := y + 1")
  testthat::expect_equal("y", ex18$symbols_produced)
  testthat::expect_equal(character(0), ex18$symbols_used)
  testthat::expect_equal("y", ex18$free_symbols)
})
