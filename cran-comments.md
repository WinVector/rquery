

## Test environments

### OSX (local machine using --as-cran from the command line)

    R CMD check --as-cran rquery_1.3.7.tar.gz


### Windows

    devtools::check_win_devel()


## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md
    ## cdata_1.1.0 started at 2019-07-04 09:32:21 success at 2019-07-04 09:32:50 (1/0/0) 
    ## rqdatatable_1.1.8 started at 2019-07-04 09:32:50 success at 2019-07-04 09:33:22 (2/0/0)
    ## Test of rquery had 2 successes, 0 failures, and 0 skipped packages. 

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
