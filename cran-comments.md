

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.6.tar.gz 
    * using log directory ‘/Users/johnmount/Documents/work/rquery.Rcheck’
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.6’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


### Windows

    devtools::check_win_devel()
    * using R Under development (unstable) (2020-10-15 r79342)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'rquery/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'rquery' version '1.4.6'
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    ...
    Status: OK


    rhub::check_for_cran()
    1234#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    1235#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    1236#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    1237#> setting R_REMOTES_STANDALONE to true
    1238#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    1239#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    1240#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    1241#> * using log directory 'C:/Users/USERpCWcKjIBFh/rquery.Rcheck'
    1242#> * using R Under development (unstable) (2020-10-09 r79317)
    1243#> * using platform: x86_64-w64-mingw32 (64-bit)
    1244#> * using session charset: ISO8859-1
    1245#> * using option '--as-cran'
    1246#> * checking for file 'rquery/DESCRIPTION' ... OK
    1247#> * checking extension type ... Package
    1248#> * this is package 'rquery' version '1.4.6'
    1249#> * package encoding: UTF-8
    1250#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1251#> Maintainer: 'John Mount '
    1307#> Status: OK


## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
