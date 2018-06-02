pipeline\_example
================

``` r
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("rqdatatable")
```

    ## Loading required package: rquery

    ## Loading required package: wrapr

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     coalesce

``` r
library("microbenchmark")

batting <- Lahman::Batting


# dplyr pipeline example from R for Data Science.
fn_dplyr <- function(batting) {
  # http://r4ds.had.co.nz/transform.html
  batting %>% 
    group_by(playerID) %>% 
    summarise(
      ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
      ab = sum(AB, na.rm = TRUE)
    ) %>%
    filter(ab > 100) 
}

system.time(
  batters <- fn_dplyr(batting)
)
```

    ##    user  system elapsed 
    ##   0.525   0.018   0.556

``` r
batters <- arrange(batters, playerID)

# translation of above example into an rquery pipeline
fn_rquery <- function(batting) {
  rq_pipeline <- local_td(batting) %.>%
    project_nse(., groupby = "playerID",
                ba = sum(H) / sum(AB),
                ab = sum(AB)) %>%
    select_rows_nse(., ab > 100) %>%
    orderby(., "playerID")
  ex_data_table(rq_pipeline)
}

system.time({
  batters2 <- fn_rquery(batting)
})
```

    ##    user  system elapsed 
    ##   0.118   0.004   0.078

``` r
assertthat::are_equal(as.data.frame(batters),
                      as.data.frame(batters2))
```

    ## [1] TRUE

``` r
microbenchmark(
  dplyr = nrow(fn_dplyr(batting)),
  rquery = nrow(fn_rquery(batting))
)
```

    ## Unit: milliseconds
    ##    expr       min        lq      mean    median        uq      max neval
    ##   dplyr 449.70200 482.96252 552.90256 544.00514 571.20214 902.9251   100
    ##  rquery  48.95125  66.52097  75.26825  76.43914  80.62677 160.0422   100
