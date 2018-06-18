
I would like to request an early update for the rquery package, please.
Due to an aggressive development pace the package has been updated 7 times in the last 6 months and will
not be eligible for an update until July 24th.  However an early update will allow
me to submit a substantial new package to CRAN ( rqdatatable: https://github.com/WinVector/rqdatatable )
and allow a substantial update to the vtreat package on CRAN.
The main improvements are the ability to work with the data.table package and allowing vtreat to
work with databases and data.table.
Both of these updates are fully tested and I will not ask for any more early updates
on the rquery package in the near term (before the number of updates again falls to below 7 in the last 
6 months).

## Test environments

    * OSX (local machine using --as-cran from the command line)
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
   
    * Windows ( win-builder.r-project.org uses --as-cran )
    * devtools::build_win()
    * using R Under development (unstable) (2018-06-15 r74904)
    * using platform: x86_64-w64-mingw32 (64-bit)


## R CMD check results

    R CMD check --as-cran rquery_0.5.0.tar.gz 

    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘0.5.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’

    Number of updates in past 6 months: 7
    Status: 1 NOTE


## Reverse dependencies

New package, now reverse dependencies.

     devtools::revdep()
     character(0)
     

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.



