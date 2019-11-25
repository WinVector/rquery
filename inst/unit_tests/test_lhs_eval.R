

test_lhs_eval_ad_hoc <- function() {
  # without the .() notation (not quiet what we want from the user)
  d <- data.frame(x = 1)
  ops <- local_td(d)
  for(i in 1:3) {
    ops <- extend(ops, paste0("v_", i) %:=% (i+1))
  }

  invisible(NULL)
}

test_lhs_eval_b <- function() {
  # with the .() notation (the strict form)
  d <- data.frame(x = 1)
  ops <- local_td(d)
  for(i in 1:3) {
    ops <- extend(ops, .(paste0("v_", i)) %:=% (i+1))
  }

  invisible(NULL)
}

test_lhs_eval_b2 <- function() {
  # with the .() notation (the strict form)
  d <- data.frame(x = 1)
  ops <- local_td(d)
  for(i in 1:3) {
    v_i <- paste0("v_", i)
    ops <- extend(ops, .(v_i) %:=% (i+1))
  }

  invisible(NULL)
}


test_lhs_eval_b3 <- function() {
  # with the .() notation (the strict form)
  d <- data.frame(x = 1)
  var <- 'y'
  # should be var
  ops1 <- local_td(d) %.>%
    extend(., var := 1)
  RUnit::checkTrue(length(grep('y', format(ops1)))==0)
  RUnit::checkTrue(length(grep('var', format(ops1)))==1)
  # should be y
  ops2 <- local_td(d) %.>%
    extend(., .(var) := 1)
  RUnit::checkTrue(length(grep('var', format(ops2)))==0)
  RUnit::checkTrue(length(grep('y', format(ops2)))==1)
  invisible(NULL)
}


