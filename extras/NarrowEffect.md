NarrowEffect
================
Win-Vector LLC
12/18/2017

<!-- NarrowEffect.md is generated from NarrowEffect.Rmd. Please edit that file -->
For some time we have been teaching [`R`](https://journal.r-project.org) users "when working with wide tables on Spark or on databases: narrow to the columns you really want to work with early in your analysis."

This issue arises because ultra-wide tables (200 to 1000 columns) are quite common in big-data analytics projects. Often these are "denormalized marts" that are used to drive many different projects. For any one project only a small subset of the columns may be relevant in a calculation.

The idea behind the advice is: working with fewer columns make for quicker queries.

Some wonder is this really an issue or is it something one can ignore in the hope the downstream query optimizer fixes the problem. In this note we will show the effect is real.

Let's set up our experiment. The data is a larger version of the problem from ["Let’s Have Some Sympathy For The Part-time R User"](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/). We have expanded the number of subjects to 10^{5} and added 1000 irrelevant columns to the data store. We define a new function that uses `dplyr` and `Sparklyr` to compute the diagnoses. We can turn on and off if the table is first limited to columns of interest and if the results are brought back to `R`.

``` r
scale <- 0.237


dplyr_run <- function(narrow, collect) {
  dR <- dT
  if(narrow) {
    dR <- dR %>%
      select(subjectID, surveyCategory, assessmentTotal)
  }
  dR <- dR %>%
    group_by(subjectID) %>%
    mutate(probability =
             exp(assessmentTotal * scale)/
             sum(exp(assessmentTotal * scale))) %>%
    arrange(probability, surveyCategory) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    rename(diagnosis = surveyCategory) %>%
    select(subjectID, diagnosis, probability) %>%
    arrange(subjectID)
  if(collect) {
    dR <- collect(dR)
  } else {
    dR <- compute(dR)
  }
  dR
}


head(dplyr_run(narrow=FALSE, collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.840
    ## 2         2 positive re-framing       0.869
    ## 3         3 withdrawal behavior       0.500
    ## 4         4 withdrawal behavior       0.671
    ## 5         5 withdrawal behavior       0.500
    ## 6         6 positive re-framing       0.616

We can get timings for variations of the function:

``` r
library("microbenchmark")

timings <- microbenchmark(dplyr_run(narrow=FALSE, collect=TRUE), 
                          dplyr_run(narrow=TRUE, collect=TRUE),
                          dplyr_run(narrow=FALSE, collect=FALSE), 
                          dplyr_run(narrow=TRUE, collect=FALSE),
                          times = 10)
```

And then present the results:

``` r
print(timings)
```

    ## Unit: milliseconds
    ##                                        expr       min        lq     mean
    ##   dplyr_run(narrow = FALSE, collect = TRUE) 2765.8306 2838.8260 3102.163
    ##    dplyr_run(narrow = TRUE, collect = TRUE) 1695.0866 1730.7774 1905.877
    ##  dplyr_run(narrow = FALSE, collect = FALSE) 2224.3104 2259.4446 2455.062
    ##   dplyr_run(narrow = TRUE, collect = FALSE)  906.2803  939.9202 1223.131
    ##     median       uq      max neval
    ##  2884.8669 2943.277 4238.061    10
    ##  1790.9622 2107.483 2403.142    10
    ##  2295.3708 2317.765 3855.946    10
    ##   998.0271 1026.260 2363.670    10

``` r
tdf <- as.data.frame(timings)

# order the data
tdf <- tdf %>%
  group_by(., expr) %>%
  mutate(., mtime = median(time)) %>%
  ungroup(.)

tdf$expr <- reorder(tdf$expr, tdf$mtime)
WVPlots::ScatterBoxPlotH(tdf, "time", "expr",  
                         pt_alpha=0.2,
                         title="Execution times in NS")
```

![](NarrowEffect_files/figure-markdown_github/present-1.png)

Notice the times where we have not by-hand narrowed the table are indeed much slower.

The advice is confirmed: narrow to the columns of interest early in your analysis.

Narrowing to the exact columns used can be difficult: it can involve inspecting an arbitrarily long pipeline for column uses. That is part of why we are developing a new `R` query generator that does just that: [`rquery`](https://winvector.github.io/rquery/).

``` r
sparklyr::spark_disconnect(my_db)
```
