
test_calc_warnings <- function() {

  d <- data.frame(
    x = 1:3,
    y = 4:6
  )

  # # not warning here any more, as if_else uses this construction
  # saw_w <- NULL
  # tryCatch({
  #   ops <- local_td(d) %.>%
  #     extend(.,
  #            x := 1,
  #            x := 2)
  # },
  # warning = function(w) { saw_w <<- w }
  # )
  # expect_true(!is.null(saw_w))

  expect_error({
    ops <- local_td(d) %.>%
      project(.,
              x := 1,
              x := 2)
  })

  expect_error({
    ops <- local_td(d) %.>%
      project(.,
              x := 1,
              y := x)
  })

  invisible(NULL)
}

test_calc_warnings()

