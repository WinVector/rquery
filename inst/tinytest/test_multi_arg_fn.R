
test_multi_arg_fn <- function() {
  # at the very least hard to pass named arguments to SQL
  # so we are blocking it for now.
  expect_error(
    ops <- mk_td('d', 'str') %.>%
      extend(., strs = paste(str, collapse = ', ')))

  invisible(NULL)
}

test_multi_arg_fn()
