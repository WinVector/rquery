

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.3.5.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.3.5’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::build_win()
    * using R Under development (unstable) (2019-06-23 r76735)
    * using platform: x86_64-w64-mingw32 (64-bit)
    Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md
    ## cdata_1.1.0 started at 2019-06-25 09:14:15 success at 2019-06-25 09:14:50 (1/0/0) 
    ## rqdatatable_1.1.8 started at 2019-06-25 09:14:50 success at 2019-06-25 09:15:22 (2/0/0)
    ## Test of rquery had 2 successes, 0 failures, and 0 skipped packages. 

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
