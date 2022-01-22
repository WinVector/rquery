

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.8.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.8’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2022-01-21 r81547 ucrt)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: UTF-8
    * checking for file 'rquery/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'rquery' version '1.4.8'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK

    rhub::check_for_cran()
    748#> * using R Under development (unstable) (2021-12-17 r81389 ucrt)
    749#> * using platform: x86_64-w64-mingw32 (64-bit)
    750#> * using session charset: UTF-8
    751#> * using option '--as-cran'
    752#> * checking for file 'rquery/DESCRIPTION' ... OK
    753#> * checking extension type ... Package
    754#> * this is package 'rquery' version '1.4.8'
    755#> * package encoding: UTF-8
    756#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    757#> Maintainer: 'John Mount '
    811#> * checking for detritus in the temp directory ... NOTE
    812#> Found the following files/directories:
    813#> 'lastMiKTeXException'
    814#> * DONE
    815#> Status: 1 NOTE
    lastMiKTeXException not in package, likely an effect of testing platform.

## Linux

    rhub::check_for_cran()
    4624#> * using R version 4.1.2 (2021-11-01)
    4625#> * using platform: x86_64-pc-linux-gnu (64-bit)
    4626#> * using session charset: UTF-8
    4627#> * using option ‘--as-cran’
    4628#> * checking for file ‘rquery/DESCRIPTION’ ... OK
    4629#> * checking extension type ... Package
    4630#> * this is package ‘rquery’ version ‘1.4.8’
    4631#> * package encoding: UTF-8
    4632#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    4633#> Maintainer: ‘John Mount ’
    4690#> Status: OK
    
    rhub::check_for_cran()
    4563#> * using R Under development (unstable) (2022-01-21 r81547)
    4564#> * using platform: x86_64-pc-linux-gnu (64-bit)
    4565#> * using session charset: UTF-8
    4566#> * using option ‘--as-cran’
    4567#> * checking for file ‘rquery/DESCRIPTION’ ... OK
    4568#> * checking extension type ... Package
    4569#> * this is package ‘rquery’ version ‘1.4.8’
    4570#> * package encoding: UTF-8
    4571#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    4572#> Maintainer: ‘John Mount ’
    4629#> Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
