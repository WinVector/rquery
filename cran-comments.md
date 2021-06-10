

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.7.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.7’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    ...
    Status: OK


### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2021-06-07 r80458)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'rquery/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'rquery' version '1.4.7'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK


## Linux

    rhub::check_for_cran()
    #> Fedora 33 - x86_64 - Updates 29 kB/s | 11 kB 00:00
    3803#> * using R Under development (unstable) (2021-06-09 r80471)
    3809#> * this is package ‘rquery’ version ‘1.4.7’
    3811#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    3812#> Maintainer: ‘John Mount ’
    3869#> Status: OK
    
    rhub::check_for_cran()

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
