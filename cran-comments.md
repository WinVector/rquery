

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.3.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
    
### Windows

    rhub::check_for_cran()
     998#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     999#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    1000#> setting R_REMOTES_STANDALONE to true
    1001#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    1002#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    1003#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    1004#> * using log directory 'C:/Users/USERJluVHZIXfM/rquery.Rcheck'
    1005#> * using R Under development (unstable) (2020-01-22 r77697)
    1006#> * using platform: x86_64-w64-mingw32 (64-bit)
    1007#> * using session charset: ISO8859-1
    1008#> * using option '--as-cran'
    1009#> * checking for file 'rquery/DESCRIPTION' ... OK
    1010#> * checking extension type ... Package
    1011#> * this is package 'rquery' version '1.4.3'
    1012#> * package encoding: UTF-8
    1013#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1014#> Maintainer: 'John Mount '
    1070#> Status: OK    

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
