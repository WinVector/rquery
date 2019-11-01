

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.0.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Days since last update: 5
    Number of updates in past 6 months: 7
    Status: 1 NOTE

### Windows

    rhub::check_for_cran()
     857#> * using R Under development (unstable) (2019-10-19 r77318)
     858#> * using platform: x86_64-w64-mingw32 (64-bit)
     859#> * using session charset: ISO8859-1
     860#> * using option '--as-cran'
     861#> * checking for file 'rquery/DESCRIPTION' ... OK
     862#> * checking extension type ... Package
     863#> * this is package 'rquery' version '1.4.0'
     864#> * package encoding: UTF-8
     865#> * checking CRAN incoming feasibility ... NOTE
     866#> Maintainer: 'John Mount '
     867#> Days since last update: 5
     868#> Number of updates in past 6 months: 7
     924#> Status: 1 NOTE

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
