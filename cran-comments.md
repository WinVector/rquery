

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.1.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.1’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
    
### Windows

     rhub::check_for_cran()
     822#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     823#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     824#> setting R_REMOTES_STANDALONE to true
     825#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     826#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     827#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     828#> * using log directory 'C:/Users/USERPqidzGfXOb/rquery.Rcheck'
     829#> * using R Under development (unstable) (2019-11-08 r77393)
     830#> * using platform: x86_64-w64-mingw32 (64-bit)
     831#> * using session charset: ISO8859-1
     832#> * using option '--as-cran'
     833#> * checking for file 'rquery/DESCRIPTION' ... OK
     834#> * checking extension type ... Package
     835#> * this is package 'rquery' version '1.4.1'
     836#> * package encoding: UTF-8
     837#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     838#> Maintainer: 'John Mount '
     894#> Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
