
# https://github.com/WinVector/pyvtreat/blob/master/Examples/StratifiedCrossPlan/StratifiedCrossPlan.ipynb

test_project_empty <- function() {

  prepared_stratified <- data.frame(
    'y' = c(1, 0, 0, 1, 0, 0),
    'g' = c(0, 0, 0, 1, 1, 1),
    'x' = c(1, 2, 3, 4, 5, 6)
  )

  ops <- local_td(prepared_stratified) %.>%
    project(.,
            sum %:=% sum(y),
            mean %:=% mean(y),
            size %:=% n(),
            groupby='g')

  opsb <- local_td(prepared_stratified) %.>%
    project(.,
            groupby='g',
            sum %:=% sum(y),
            mean %:=% mean(y),
            size %:=% n())

  expect_equal(format(ops), format(opsb))

  # used to throw
  ops2 <- local_td(prepared_stratified) %.>%
    project(.,
            sum %:=% sum(y),
            mean %:=% mean(y),
            size %:=% n())

  ops3 <- local_td(prepared_stratified) %.>%
    project(.,
            groupby=c(),
            sum %:=% sum(y),
            mean %:=% mean(y),
            size %:=% n())

  expect_equal(format(ops2), format(ops3))

  invisible(NULL)
}

test_project_empty()
