

Work around not reproducible vignette failure on r-release-windows-ix86+x86_64.

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.4.9.tar.gz 
    * using R version 4.1.2 (2021-11-01)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.4.9’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


### Windows

    devtools::check_win_devel()
    ...


    devtools::check_win_release()
    ...
    
    rhub::check_for_cran()
    470#> * using platform: x86_64-w64-mingw32 (64-bit)
    471#> * using session charset: UTF-8
    472#> * using option '--as-cran'
    473#> * checking for file 'rquery/DESCRIPTION' ... OK
    474#> * checking extension type ... Package
    475#> * this is package 'rquery' version '1.4.9'
    476#> * package encoding: UTF-8
    477#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    478#> Maintainer: 'John Mount '
    532#> * checking for detritus in the temp directory ... NOTE
    533#> Found the following files/directories:
    534#> 'lastMiKTeXException'
    535#> * DONE
    536#> Status: 1 NOTE


## Linux

    rhub::check_for_cran()

    
    rhub::check_for_cran()


## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
