
## Test environments

    R CMD check --as-cran rquery_1.5.1.tar.gz 
    
        * using log directory ‘/Users/johnmount/Documents/work/rquery.Rcheck’
        * using R version 4.6.1 (2026-06-24)
        * using platform: x86_64-apple-darwin20
        * checking HTML version of manual ... NOTE
        Skipping checking HTML validation: 'tidy' doesn't look like recent enough HTML Tidy.
        Please obtain a recent version of HTML Tidy by downloading a binary
        release or compiling the source code from <https://www.html-tidy.org/>.
        * checking for non-standard things in the check directory ... OK
        * checking for detritus in the temp directory ... OK
        * DONE
        Status: 1 NOTE


    devtools::check_win_devel()
    skip

    rhub::check_for_cran()
    skip

## Reverse dependencies

    Checked https://github.com/WinVector/rquery/blob/master/extras/check_reverse_dependencies.md

Note: "Edgar F. Codd", "SQL", and "observable" are all spelled correctly.
