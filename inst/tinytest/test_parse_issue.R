

test_parse_issue <- function() {
  badex <- wrapr::build_frame(
    'name'                      , 'expr'                                                              |
    'Var228_lev_x.F2FyR07IdsN7I', 'ifelse(is.na(Var228), 0, ifelse(Var228 == "F2FyR07IdsN7I", 1, 0))' )
  expr <- badex$expr
  names(expr) <- badex$name
  dat <- mk_td("tab", paste0("Var228"))
  op <- extend_se(dat, expr)
  str <- format(op) # notice some lines have more than 1 := !
  parts <- strsplit(str, ":=", fixed = TRUE)
  expect_equal(length(parts[[1]]), 2)

  invisible(NULL)
}

test_parse_issue()
