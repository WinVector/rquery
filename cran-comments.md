

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.3.3.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘1.3.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK


### Windows

    devtools::build_win()
    
    * using log directory 'd:/RCompile/CRANguest/R-release/rquery.Rcheck'
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-w64-mingw32 (64-bit)
    Status: OK
    
    * using log directory 'd:/RCompile/CRANguest/R-devel/rquery.Rcheck'
    * using R Under development (unstable) (2019-05-30 r76623)
    * using platform: x86_64-w64-mingw32 (64-bit)
    Status: OK

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md
    ## cdata_1.1.0 started at 2019-06-01 08:06:59 success at 2019-06-01 08:07:27 (1/0/0) 
    ## rqdatatable_1.1.7 started at 2019-06-01 08:07:27 success at 2019-06-01 08:07:56 (2/0/0)
    ## Test of rquery had 2 successes, 0 failures, and 0 skipped packages. 

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
