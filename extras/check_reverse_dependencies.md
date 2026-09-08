check_reverse_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "rquery"
date()
```

    ## [1] "Tue Sep  8 08:13:18 2026"

``` r
packageVersion(package)
```

    ## [1] '1.5.1'

``` r
parallelCluster <- NULL
ncores <- parallel::detectCores()
# prrd back to bombing out with database locked
#if(ncores > 1) {
#  parallelCluster <- parallel::makeCluster(ncores)
#}

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/rquery/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7f/sdjycp_d08n8wwytsbgwqgsw0000gn/T//RtmpAlJtv4"

``` r
options(repos = c(CRAN="https://cloud.r-project.org"))
jobsdfe <- enqueueJobs(package=package, directory=td)

mk_fn <- function(package, directory) {
  force(package)
  force(directory)
  function(i) {
    library("prrd")
    setwd(directory)
    Sys.sleep(1*i)
    dequeueJobs(package=package, directory=directory)
  }
}
f <- mk_fn(package=package, directory=td)

if(!is.null(parallelCluster)) {
  parallel::parLapply(parallelCluster, seq_len(ncores), f)
} else {
  f(0)
}
```

    ## ## Reverse depends check of rquery 1.5.1 
    ## cdata_1.2.1 started at 2026-09-08 08:13:19 success at 2026-09-08 08:13:42 (1/0/0) 
    ## rqdatatable_1.3.3 started at 2026-09-08 08:13:44 success at 2026-09-08 08:14:24 (2/0/0) 
    ## WVPlots_1.3.9 started at 2026-09-08 08:14:24 success at 2026-09-08 08:15:32 (3/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of rquery 1.5.1 had 3 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2026-09-08 08:13:19 to 2026-09-08 08:15:32 for 2.217 mins 
    ## Average of 44.333 secs relative to 43.988 secs using 1 runners
    ## 
    ## Failed packages:   
    ## 
    ## Skipped packages:   
    ## 
    ## None still working
    ## 
    ## None still scheduled

``` r
setwd(orig_dir)
if(!is.null(parallelCluster)) {
  parallel::stopCluster(parallelCluster)
}
```
