

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.5.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.5’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows

    rhub::check_for_cran()
    887#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    888#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    889#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    890#> setting R_REMOTES_STANDALONE to true
    891#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    892#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    893#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    894#> * using log directory 'C:/Users/USERdNIehprwXY/rquery.Rcheck'
    895#> * using R Under development (unstable) (2020-07-05 r78784)
    896#> * using platform: x86_64-w64-mingw32 (64-bit)
    897#> * using session charset: ISO8859-1
    898#> * using option '--as-cran'
    899#> * checking for file 'rquery/DESCRIPTION' ... OK
    900#> * checking extension type ... Package
    901#> * this is package 'rquery' version '1.4.5'
    902#> * package encoding: UTF-8
    903#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    904#> Maintainer: 'John Mount '
    960#> Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
