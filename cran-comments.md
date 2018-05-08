


## Test environments

    * OSX (local machien using --as-cran from the command line)
    * using R version 3.4.4 (2018-03-15)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

    * Windows ( win-builder.r-project.org uses --as-cran )
    * using R Under development (unstable) (2018-05-05 r74699)
    * using platform: x86_64-w64-mingw32 (64-bit)
    
    * Linux (r-hub, I believe it does not set --as-cran)
    * using R Under development (unstable) (2018-05-05 r74699)
    * using platform: x86_64-pc-linux-gnu (64-bit)
   
## R CMD check results

    R CMD check --as-cran rquery_0.4.3.tar.gz 

    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘0.4.3’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ...
    Maintainer: ‘John Mount <jmount@win-vector.com>’

    Status: OK

## Reverse dependencies

New package, now reverse dependencies.

     devtools::revdep()
     character(0)
     
     
Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
