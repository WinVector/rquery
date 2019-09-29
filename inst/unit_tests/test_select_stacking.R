test_select_stacking <- function() {

  ops1 <- table_source('d', c('a', 'b', 'c')) %.>% select_columns(., c('a', 'b')) %.>% select_columns(., c('b'))
  ops1_str <- format(ops1)
  RUnit::checkEquals(length(gregexpr('select_columns', ops1_str, fixed = TRUE)[[1]]), 1)

  ops2 <- table_source('d', c('a', 'b', 'c')) %.>% select_columns(., c('a', 'b')) %.>% select_columns(., c('b', 'a'))
  ops2_str <- format(ops2)
  RUnit::checkEquals(length(gregexpr('select_columns', ops2_str, fixed = TRUE)[[1]]), 1)

  invisible(NULL)
}
