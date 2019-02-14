

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.3.1.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.3.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    rhub::check_for_cran()
    792#> * using R Under development (unstable) (2019-01-26 r76018)
    793#> * using platform: x86_64-w64-mingw32 (64-bit)
    794#> * using session charset: ISO8859-1
    795#> * using option '--as-cran'
    796#> * checking for file 'rquery/DESCRIPTION' ... OK
    797#> * checking extension type ... Package
    798#> * this is package 'rquery' version '1.3.1'
    799#> * package encoding: UTF-8
    800#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    801#> Maintainer: 'John Mount '
    855#> Status: OK
    
    devtools::build_win()
  

## Reverse dependencies

    Checked 
    ## cdata_1.0.5 started at 2019-02-14 10:37:27 success at 2019-02-14 10:37:44 (1/0/0) 
    ## rqdatatable_1.1.2 started at 2019-02-14 10:37:44 success at 2019-02-14 10:38:09 (2/0/0)
    https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md


Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
