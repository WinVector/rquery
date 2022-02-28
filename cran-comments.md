

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
    Your package rquery_1.4.9.tar.gz has been built (if working) and checked for Windows.
    Please check the log files and (if working) the binary package at:
    https://win-builder.r-project.org/5FU971Iw3l9A
    The files will be removed after roughly 72 hours.
    Installation time in seconds: 15
    Check time in seconds: 96
    Status: OK
    R Under development (unstable) (2022-02-26 r81819 ucrt)

    devtools::check_win_release()
    Your package rquery_1.4.9.tar.gz has been built (if working) and checked for Windows.
    Please check the log files and (if working) the binary package at:
    https://win-builder.r-project.org/gD5drZ6080Ye
    The files will be removed after roughly 72 hours.
    Installation time in seconds: 85
    Check time in seconds: 146
    Status: OK
    R version 4.1.2 (2021-11-01)
    
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
    2772#> * using R version 4.1.2 (2021-11-01)
    2773#> * using platform: x86_64-pc-linux-gnu (64-bit)
    2774#> * using session charset: UTF-8
    2775#> * using option ‘--as-cran’
    2776#> * checking for file ‘rquery/DESCRIPTION’ ... OK
    2777#> * checking extension type ... Package
    2778#> * this is package ‘rquery’ version ‘1.4.9’
    2779#> * package encoding: UTF-8
    2780#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    2781#> Maintainer: ‘John Mount ’
    2838#> Status: OK
    
    rhub::check_for_cran()
    2705#> * using R Under development (unstable) (2022-02-06 r81658)
    2706#> * using platform: x86_64-pc-linux-gnu (64-bit)
    2707#> * using session charset: UTF-8
    2708#> * using option ‘--as-cran’
    2709#> * checking for file ‘rquery/DESCRIPTION’ ... OK
    2710#> * checking extension type ... Package
    2711#> * this is package ‘rquery’ version ‘1.4.9’
    2712#> * package encoding: UTF-8
    2713#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    2714#> Maintainer: ‘John Mount ’
    2771#> Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
