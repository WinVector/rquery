

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.5.tar.gz 
    * using R version 3.6.2 (2019-12-12)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.5’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    
    Number of updates in past 6 months: 7
    
    Found the following (possibly) invalid URLs:
      URL: http://r-datatable.com
        From: README.md
        Status: 500
        Message: Internal Server Error
      URL: http://r-datatable.com/
        From: README.md
        Status: 500
        Message: Internal Server Error
    * checking package namespace information ... OK
    * checking package dependencies ... OK
    * checking if this is a source package ... OK
    Status: 1 NOTE
    URLs are previously good, and possibly suffering a temporary outage.
    
### Windows

    rhub::check_for_cran()

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
