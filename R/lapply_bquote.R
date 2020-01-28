

# convert unary -char (length1) to a name
minus_fn <- function(e1, e2) {
  if(is.name(e1)) {
    return(e1)
  }
  if(is.character(e1) && (length(e1)==1)) {
    return(as.name(e1))
  }
  if(missing(e2)) {
    base::`-`(e1)
  } else {
    base::`-`(e1, e2)
  }
}


# bquote items, and also apply -char -> name conversion
lapply_bquote_to_langauge_list <- function(ll, env) {
  force(env)
  env2 <- new.env(parent = env)
  assign('-', minus_fn, envir = env2)
  lapply(ll,
         function(li) {
           do.call(bquote, list(expr = li, where = env2), envir = env2)
         })
}

