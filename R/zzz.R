

.onLoad <- function(libname, pkgname) {
  opts <- options()
  our_defaults <- list(
    rquery.verbose = FALSE,
    rquery.use_DBI_dbExistsTable = TRUE, # this solution is ugly, so don't turn it on unless we need it.
    rquery.use_DBI_dbListFields = FALSE,
    rquery.use_DBI_dbRemoveTable = FALSE
  )
  defs <- setdiff(names(our_defaults),
                  names(opts))
  if(length(defs)>=1) {
    options(our_defaults[defs])
  }
  invisible()
}

