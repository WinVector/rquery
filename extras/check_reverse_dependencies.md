check\_reverse\_dependencies
================

``` r
library("prrd")
td <- tempdir()
package = "rquery"
date()
```

    ## [1] "Thu Feb 27 22:58:33 2020"

``` r
packageVersion(package)
```

    ## [1] '1.4.5'

``` r
parallelCluster <- NULL
ncores <- parallel::detectCores()
if(ncores > 1) {
  parallelCluster <- parallel::makeCluster(ncores)
}

orig_dir <- getwd()
print(orig_dir)
```

    ## [1] "/Users/johnmount/Documents/work/rquery/extras"

``` r
setwd(td)
print(td)
```

    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//Rtmp5G8xpC"

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

    ## [[1]]
    ##   id   title  status
    ## 1  3 WVPlots WORKING
    ## 
    ## [[2]]
    ##   id   title  status
    ## 1  1   cdata WORKING
    ## 2  3 WVPlots WORKING
    ## 
    ## [[3]]
    ## [1] id     title  status
    ## <0 rows> (or 0-length row.names)
    ## 
    ## [[4]]
    ##   id       title  status
    ## 1  1       cdata WORKING
    ## 2  2 rqdatatable WORKING
    ## 3  3     WVPlots WORKING

``` r
summariseQueue(package=package, directory=td)
```

    ## Test of rquery had 3 successes, 0 failures, and 0 skipped packages. 
    ## Ran from 2020-02-27 22:58:43 to 2020-02-27 23:00:48 for 2.083 mins 
    ## Average of 41.667 secs relative to 80.633 secs using 3 runners
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
