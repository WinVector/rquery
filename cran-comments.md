

## Test environments

    * OSX (local machine using --as-cran from the command line)
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

 
    * Windows ( win-builder.r-project.org uses --as-cran )
    * devtools::build_win()


## R CMD check results

    R CMD check --as-cran rquery_0.6.1.tar.gz 
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘0.6.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Reverse dependencies

    devtools::revdep_check()
    Checking 2 packages: rqdatatable, vtreat
    Checked rqdatatable: 0 errors | 0 warnings | 0 notes
    Checked vtreat     : 0 errors | 0 warnings | 0 notes

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
