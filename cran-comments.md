

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.2.1.tar.gz 
 

### Windows ( win-builder.r-project.org uses --as-cran )

    rhub::check_for_cran()
 

    devtools::build_win()
    


## Reverse dependencies

    devtools::revdep_check()
    Checked cdata      : 0 errors | 0 warnings | 0 notes
    Checked rqdatatable: 0 errors | 0 warnings | 0 notes
    Checked vtreat     : 0 errors | 0 warnings | 0 notes
    
Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
