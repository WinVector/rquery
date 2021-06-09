
test_multi_arg_fn <- function() {
  ops <- mk_td('d', c('a', 'b')) %.>%
    select_columns(., c('b', 'a'))
  expect_true(!("relop_table_source" %in% class(ops)))
  expect_true("relop_select_columns" %in% class(ops))

  ops <- mk_td('d', c('a', 'b')) %.>%
    select_columns(., c('a', 'b'))
  expect_true("relop_table_source" %in% class(ops))
  expect_true(!("relop_select_columns" %in% class(ops)))

  invisible(NULL)
}

test_multi_arg_fn()
