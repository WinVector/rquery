check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "rquery"
date()
```

    ## [1] "Sat Oct 26 20:27:20 2019"

``` r
packageVersion(package)
```

    ## [1] '1.3.9'

``` r
parallelCluster <- NULL
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

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//Rtmpw62hWb"

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

    ## cdata_1.1.2 started at 2019-10-26 20:27:23 success at 2019-10-26 20:28:03 (1/0/0) 
    ## rqdatatable_1.2.3 started at 2019-10-26 20:28:03 success at 2019-10-26 20:28:31 (2/0/0) 
    ## WVPlots_1.2.1 started at 2019-10-26 20:28:32 success at 2019-10-26 20:29:58 (3/0/0)

    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of rquery had 3 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2019-10-26 20:27:23 to 2019-10-26 20:29:58 for 2.583 mins 
    ## Average of 51.667 secs relative to 51.821 secs using 1 runners
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
