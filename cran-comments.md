

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.3.7.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.3.7’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    rhub::check_for_cran()
    822#> * using R Under development (unstable) (2019-07-04 r76780)
    823#> * using platform: x86_64-w64-mingw32 (64-bit)
    824#> * using session charset: ISO8859-1
    825#> * using option '--as-cran'
    826#> * checking for file 'rquery/DESCRIPTION' ... OK
    827#> * checking extension type ... Package
    828#> * this is package 'rquery' version '1.3.7'
    829#> * package encoding: UTF-8
    830#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    831#> Maintainer: 'John Mount '
    885#> * DONE
    886#> Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md
    ## cdata_1.1.1 started at 2019-07-29 09:21:48 success at 2019-07-29 09:22:22 (1/0/0) 
    ## rqdatatable_1.1.9 started at 2019-07-29 09:22:22 success at 2019-07-29 09:22:53 (2/0/0)
    ## Test of rquery had 2 successes, 0 failures, and 0 skipped packages. 

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
