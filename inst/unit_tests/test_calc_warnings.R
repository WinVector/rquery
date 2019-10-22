
test_calc_warnings <- function() {

  d <- data.frame(
    x = 1:3,
    y = 4:6
  )

  saw_w <- NULL
  tryCatch({
    ops <- local_td(d) %.>%
      extend(.,
             x := 1,
             x := 2)
  },
  warning = function(w) { saw_w <<- w }
  )
  RUnit::checkTrue(!is.null(saw_w))

  RUnit::checkException({
    ops <- local_td(d) %.>%
      project(.,
              x := 1,
              x := 2)
  }, silent = TRUE)

  RUnit::checkException({
    ops <- local_td(d) %.>%
      project(.,
              x := 1,
              y := x)
  }, silent = TRUE)

  invisible(NULL)
}
