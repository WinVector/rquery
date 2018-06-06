

.onLoad <- function(libname, pkgname) {
  opts <- options()
  our_defaults <- list(
    rquery.verbose = FALSE,
    rquery.rquery_db_executor = NULL
  )
  defs <- setdiff(names(our_defaults),
                  names(opts))
  if(length(defs)>=1) {
    options(our_defaults[defs])
  }
  invisible()
}

