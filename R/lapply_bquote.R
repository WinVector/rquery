

lapply_bquote_to_langauge_list <- function(ll, env) {
  force(env)
  lapply(ll,
         function(li) {
           do.call(bquote, list(expr = li, where = env), envir = env)
         })
}
