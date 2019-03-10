

Update to clean up non-UTF8 source code warning on CRAN.

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.3.2.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.3.2’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::build_win()
    * using R version 3.5.2 (2018-12-20)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'rquery/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'rquery' version '1.3.2'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK
    
    devtools::build_win()
    * using R Under development (unstable) (2019-03-08 r76215)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'rquery/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'rquery' version '1.3.2'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

    rhub::check_for_cran()
    875#> * using R Under development (unstable) (2019-02-24 r76155)
    876#> * using platform: x86_64-w64-mingw32 (64-bit)
    877#> * using session charset: ISO8859-1
    878#> * using option '--as-cran'
    879#> * checking for file 'rquery/DESCRIPTION' ... OK
    880#> * checking extension type ... Package
    881#> * this is package 'rquery' version '1.3.2'
    882#> * package encoding: UTF-8
    883#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    884#> Maintainer: 'John Mount '
    938#> Status: OK
 
### Linux

    R CMD check --as-cran rquery_1.3.2.tar.gz 
    * using R version 3.5.2 (2018-12-20)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.3.2’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


## Reverse dependencies

    Checked 
    ## cdata_1.0.6 started at 2019-03-10 09:11:14 success at 2019-03-10 09:11:34 (1/0/0) 
    ## rqdatatable_1.1.4 started at 2019-03-10 09:11:34 success at 2019-03-10 09:11:59 (2/0/0)
    https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md


Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
