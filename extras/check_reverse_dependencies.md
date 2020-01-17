check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "rquery"
date()
```

    ## [1] "Fri Jan 17 08:17:25 2020"

``` r
packageVersion(package)
```

    ## [1] '1.4.2'

``` r
parallelCluster <- NULL
ncores <- 0
# # parallel doesn't work due to https://github.com/r-lib/liteq/issues/22
#ncores <- parallel::detectCores()
#parallelCluster <- parallel::makeCluster(ncores)

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/rquery/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//Rtmpd4528P"

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

    ## cdata_1.1.4 started at 2020-01-17 08:17:28 success at 2020-01-17 08:18:05 (1/0/0) 
    ## rqdatatable_1.2.5 started at 2020-01-17 08:18:05 success at 2020-01-17 08:18:30 (2/0/0) 
    ## WVPlots_1.2.2 started at 2020-01-17 08:18:31 success at 2020-01-17 08:19:57 (3/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of rquery had 3 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2020-01-17 08:17:28 to 2020-01-17 08:19:57 for 2.483 mins 
    ## Average of 49.667 secs relative to 49.623 secs using 1 runners
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
