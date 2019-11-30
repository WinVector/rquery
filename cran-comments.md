

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.0.tar.gz
    * using log directory ‘/Users/johnmount/Documents/work/rquery.Rcheck’
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    rhub::check_for_cran()
     920#> * using R Under development (unstable) (2019-11-08 r77393)
     921#> * using platform: x86_64-w64-mingw32 (64-bit)
     922#> * using session charset: ISO8859-1
     923#> * using option '--as-cran'
     924#> * checking for file 'rquery/DESCRIPTION' ... OK
     925#> * checking extension type ... Package
     926#> * this is package 'rquery' version '1.4.0'
     927#> * package encoding: UTF-8
     928#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     929#> Maintainer: 'John Mount '
     985#> Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
