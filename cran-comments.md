

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.2.1.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.2.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ...
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

 

    devtools::build_win()
    * using R Under development (unstable) (2018-12-14 r75850)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'rquery/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'rquery' version '1.2.1'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK
    
    rhub::check_for_cran()
    743#> * using R Under development (unstable) (2018-11-18 r75627)
    744#> * using platform: x86_64-w64-mingw32 (64-bit)
    745#> * using session charset: ISO8859-1
    746#> * using option '--as-cran'
    747#> * checking for file 'rquery/DESCRIPTION' ... OK
    748#> * checking extension type ... Package
    749#> * this is package 'rquery' version '1.2.1'
    750#> * package encoding: UTF-8
    751#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    752#> Maintainer: 'John Mount '
    806#> Status: OK

### Linux (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.2.1.tar.gz 
    * using R version 3.5.1 (2018-07-02)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.2.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Reverse dependencies

    devtools::revdep_check()
    Checking 3 packages: cdata, rqdatatable, vtreat
    Checked cdata      : 0 errors | 0 warnings | 0 notes
    Checked rqdatatable: 0 errors | 0 warnings | 0 notes
    Checked vtreat     : 0 errors | 0 warnings | 0 notes
    
Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
