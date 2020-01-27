
test_multi_arg_fn <- function() {
  # at the very least hard to pass named arguments to SQL
  # so we are blocking it for now.
  RUnit::checkException(
    ops <- mk_td('d', 'str') %.>%
      extend(., strs = paste(str, collapse = ', ')),
    silent = TRUE)

  invisible(NULL)
}
