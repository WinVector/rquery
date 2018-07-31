
Update to prepare for S3/S4 method change in upcoming wrapr package update.  Appears to temporarily cause some notes which should clear when the wrapr package is updated.

Details: apply_right is defined in the wrapr package which is switching from S3 dispatch to S4 generic methods.  However, the rquery package is updating first with a NAMESPACE that anticipates the S4 change to be compatible with this change.  It is my belief these notes go away once I release wrapr the rquery notes go away.  wrapr is ready for release and will pushed to CRAN soon.


## Test environments

    * OSX (local machine using --as-cran from the command line)
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

    * Windows ( win-builder.r-project.org uses --as-cran )
    * devtools::build_win()


## R CMD check results

    * OSX (local machine using --as-cran from the command line)
    R CMD check --as-cran rquery_0.6.0.tar.gz 
    * using option ‘--as-cran’
    * checking for file ‘rquery/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘rquery’ version ‘0.6.0’
    * package encoding: UTF-8
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Status: OK
    
    * Windows ( win-builder.r-project.org uses --as-cran )
    * checking S3 generic/method consistency ... NOTE
     Found the following apparent S3 methods exported but not registered:
     apply_right.relop
    * checking Rd \usage sections ... NOTE
     S3 methods shown with full name in documentation object 'apply_right.relop':
     'apply_right.relop'
      The \usage entries for S3 methods should use the \method markup and not
      their full name.
    Status: 2 NOTEs
    Note: apply_right is defined in the wrapr package which is switching from S3 dispatch to S4 generic methods.  However, the rquery package is updating first with a NAMESPACE that anticipates the S4 change to be compatible with this change.  It is my belief these notes go away once I release wrapr the rquery notes go away.  wrapr is ready for release and will pushed to CRAN soon.
    

## Reverse dependencies

     devtools::revdep_check()
     Checking 2 packages: rqdatatable, vtreat
     Checked rqdatatable: 0 errors | 0 warnings | 0 notes
     Checked vtreat     : 0 errors | 0 warnings | 0 notes
     
Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
