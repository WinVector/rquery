

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.3.8.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.3.8’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    rhub::check_for_cran()
    831#> * using R Under development (unstable) (2019-08-30 r77101)
    832#> * using platform: x86_64-w64-mingw32 (64-bit)
    833#> * using session charset: ISO8859-1
    834#> * using option '--as-cran'
    835#> * checking for file 'rquery/DESCRIPTION' ... OK
    836#> * checking extension type ... Package
    837#> * this is package 'rquery' version '1.3.8'
    838#> * package encoding: UTF-8
    839#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    840#> Maintainer: 'John Mount '
    896#> Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
