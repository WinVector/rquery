check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "rquery"
date()
```

    ## [1] "Fri Nov  1 14:45:57 2019"

``` r
packageVersion(package)
```

    ## [1] '1.4.0'

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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//RtmpuvoYVk"

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

    ## cdata_1.1.3 started at 2019-11-01 14:46:00 success at 2019-11-01 14:46:32 (1/0/0) 
    ## rqdatatable_1.2.3 started at 2019-11-01 14:46:32 success at 2019-11-01 14:47:03 (2/0/0) 
    ## WVPlots_1.2.1 started at 2019-11-01 14:47:03 success at 2019-11-01 14:48:30 (3/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of rquery had 3 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-11-01 14:46:00 to 2019-11-01 14:48:30 for 2.5 mins 
    ## Average of 50 secs relative to 49.891 secs using 1 runners
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
