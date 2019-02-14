
# package to check
pkg = "rquery"

# This file is distributed without license requirements, feel free to alter/copy.
if(requireNamespace("RUnit", quietly = TRUE) &&
   requireNamespace("wrapr", quietly = TRUE)) {
  # library("RUnit") # uncomment this if you want RUnit attached during testing
  library(pkg, character.only = TRUE)
  run_rquery_tests(verbose = TRUE, require_RUnit_attached = FALSE)
}
