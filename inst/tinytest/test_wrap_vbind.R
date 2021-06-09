

test_arrow_vbind <- function() {
  d <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(2, 2, 2, 3, 7, 10),
    g = c('a', 'a', 'a', 'b', 'b' ,'b'),
    stringsAsFactors = FALSE
  )

  scale <- 5

  ops <- d %.>%
    wrap %.>%       # wrap data in a description
    extend(.,       # add a new column
           ratio := scale * y / x) %.>%
    extend(.,       # rank the rows by group and order
           simple_rank := row_number(),
           partitionby = 'g',
           orderby = 'ratio',
           reverse = 'ratio') %.>%
    extend(.,       # mark the rows we want
           choice := simple_rank == 1)

  str <- format(ops)
  expect_true(length(grep('scale', str, fixed = TRUE))==0)
  expect_true(length(grep('t', str, fixed = TRUE))==1)
  if(requireNamespace('rqdatatable', quietly = TRUE)) {
    res <- ex(ops)
    expect_true(is.data.frame(res))
  }

  invisible(NULL)
}

test_arrow_vbind()

