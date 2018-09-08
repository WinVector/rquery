

## Test environments

    * OSX (local machine using --as-cran from the command line)
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

    * Windows ( win-builder.r-project.org uses --as-cran )

## R CMD check results

    R CMD check --as-cran rquery_1.0.0.tar.gz
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.0.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Reverse dependencies

    devtools::revdep_check()
    Checking 3 packages: cdata, rqdatatable, vtreat
    Checked cdata      : 0 errors | 0 warnings | 0 notes
    Checked rqdatatable: 0 errors | 0 warnings | 0 notes
    Checked vtreat     : 0 errors | 0 warnings | 0 notes
    

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
