library("rquery")
context("Parsing")



test_that("test_parse: Works As Expected", {
  do_parse <- function(text, env= parent.frame()) {
    ei <- paste(parse(text = text), collapse = "\n")
    parse_for_SQL(ei,
                  contextname = "table",
                  colnames = c("c1", "c2"),
                  env = env)
  }

  ex1 <- do_parse("1")
  expect_equal(list("1"), ex1$parsed)

  ex2 <- do_parse("c1")

  ex3 <- do_parse("1+1")
})
