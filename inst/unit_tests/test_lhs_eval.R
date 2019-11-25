

test_lhs_eval <- function() {
  d <- data.frame(x = 1)
  ops <- local_td(d)
  for(i in 1:3) {
    ops <- extend(ops, paste0("v_", i) %:=% (i+1))
  }

  invisible(NULL)
}
