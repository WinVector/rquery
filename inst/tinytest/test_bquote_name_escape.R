

test_bquote_name_escape <- function() {
  a = 'a_name'
  b = 'b_name'
  # wrong form b is character not name
  ops <- mk_td('d', c('a_name', 'b_name')) %.>% extend(., .(a) := .(b))
  str <- format(ops)
  expect_equal(length(grep(':= "b_name"', str, fixed = TRUE)), 1)
  expect_equal(length(grep(':= b_name', str, fixed = TRUE)), 0)
  # correct form b is a name
  ops <- mk_td('d', c('a_name', 'b_name')) %.>% extend(., .(-a) := .(-b))
  str <- format(ops)
  expect_equal(length(grep(':= "b_name"', str, fixed = TRUE)), 0)
  expect_equal(length(grep(':= b_name', str, fixed = TRUE)), 1)
  # math
  ops <- mk_td('d', c('a_name', 'b_name')) %.>% extend(., .(-a) := .(-3-5))
  str <- format(ops)
  expect_equal(length(grep('-8', str, fixed = TRUE)), 1)
  invisible(NULL)
}

test_bquote_name_escape()
