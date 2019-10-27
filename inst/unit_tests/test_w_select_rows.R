
test_w_select_rows <- function() {

  d <- data.frame(x = 1:3, y = 4:6)

  ops <- wrap(d) %.>%
    select_rows(., x>2)
  if(requireNamespace('rqdatatable', quietly = TRUE)) {
     res <- ex(ops)

     expect <- wrapr::build_frame(
       "x"  , "y" |
       3L , 6L  )

     RUnit::checkTrue(wrapr::check_equiv_frames(res, expect))
  }

  invisible(NULL)
}
