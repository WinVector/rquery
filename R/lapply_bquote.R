



# bquote items, and also apply -char -> name conversion
lapply_bquote_to_langauge_list <- function(ll, env) {
  force(env)
  orig_minus <- get('-', mode = 'function', envir = env)
  minus_fn_to_name <- function(e1, e2) {
    if(!missing(e2)) {
      return(orig_minus(e1, e2))
    }
    if(is.name(e1)) {
      return(e1)
    }
    if(is.character(e1) && (length(e1)==1)) {
      return(as.name(e1))
    }
    orig_minus(e1)
  }
  env2 <- new.env(parent = env)
  assign('-', minus_fn_to_name, envir = env2)
  lapply(ll,
         function(li) {
           do.call(bquote, list(expr = li, where = env2), envir = env2)
         })
}

