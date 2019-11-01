

test_immediate_substitution <- function() {
  if(requireNamespace('rqdatatable', quietly = TRUE)) {
    library("rqdatatable")
    d <- data.frame(
      x = c(1, 1, 2),
      y = c(5, 4, 3),
      z = c(6, 7, 8)
    )

    condition_variable <- as.name('x')
    new_value_variable <- as.name('y')
    old_value_variable <- as.name('z')

    res <- d %.>%
      select_rows(.,
                  .(condition_variable) == 1) %.>%
      extend(.,
             .(new_value_variable) := .(old_value_variable) + 1) %.>%
      order_rows(.,
                 c('x', 'y', 'z'))

    expect <- wrapr::build_frame(
      "x"  , "y", "z" |
        1  , 7  , 6   |
        1  , 8  , 7   )

    RUnit::checkTrue(wrapr::check_equiv_frames(res, expect))
  }
  invisible(NULL)
}
