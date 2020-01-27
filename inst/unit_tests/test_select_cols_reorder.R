
test_multi_arg_fn <- function() {
  ops <- mk_td('d', c('a', 'b')) %.>%
    select_columns(., c('b', 'a'))
  RUnit::checkTrue(!("relop_table_source" %in% class(ops)))
  RUnit::checkTrue("relop_select_columns" %in% class(ops))

  ops <- mk_td('d', c('a', 'b')) %.>%
    select_columns(., c('a', 'b'))
  RUnit::checkTrue("relop_table_source" %in% class(ops))
  RUnit::checkTrue(!("relop_select_columns" %in% class(ops)))

  invisible(NULL)
}
