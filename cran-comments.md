

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.4.tar.gz 
    * using R version 3.6.2 (2019-12-12)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.4’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
    
### Windows

    rhub::check_for_cran()
     782#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     783#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     784#> setting R_REMOTES_STANDALONE to true
     785#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     786#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     787#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     788#> * using log directory 'C:/Users/USERBGEoQwFmlc/rquery.Rcheck'
     789#> * using R Under development (unstable) (2020-01-22 r77697)
     790#> * using platform: x86_64-w64-mingw32 (64-bit)
     791#> * using session charset: ISO8859-1
     792#> * using option '--as-cran'
     793#> * checking for file 'rquery/DESCRIPTION' ... OK
     794#> * checking extension type ... Package
     795#> * this is package 'rquery' version '1.4.4'
     796#> * package encoding: UTF-8
     797#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     798#> Maintainer: 'John Mount '
     854#> Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
