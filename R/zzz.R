

#' An example \code{rquery_db_info} object useful for formatting \code{SQL} without a database connection.
#'
#' @export
#'
rquery_default_db_info <- rquery_db_info(identifier_quote_char = '"',
                                         string_quote_char = "'",
                                         is_dbi = FALSE,
                                         db_methods = rquery_default_methods())


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

