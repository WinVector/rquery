Plotting example
================
John Mount, Nina Zumel
January 10, 2018

As a follow-up to ["rquery: Fast Data Manipulation in R"](http://www.win-vector.com/blog/2018/01/rquery-fast-data-manipulation-in-r/) we [re-ran the experiment with a nice "base `R`" (or "pure `R`") implementation of the calculation added to the assessments](https://github.com/WinVector/rquery/blob/master/extras/QTimingFollowup/QTiming.md).

It turns out base `R` is much faster than any of the alternatives.

That should not come as a surprise, but we think there are some current gaps in R teaching that make it surprising to many. There is a bit of a "this package is in C/C++, so it is going to be fast" fallacy. Also it has probably been a while since somebody publicly investigated exactly how large the "this package is a little slower at runtime, but the notation allows faster development" trade off actually is (and one also should think hard on the stability and clarity of some newer notations).

``` r
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
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
library(ggplot2)
library(wrapr)
library(rquery)
```

    ## Loading required package: cdata

``` r
source("../plotbenchmark.R")

timings = readRDS("qtimings.RDS")
timings$expr <- as.character(timings$expr)

# colors
highlightcolor = "#33a02c"
backgroundcolor = "#a6cee3"
reruncolor = "#b2df8a"
greycolor = "darkgrey"
```

``` r
runs <- c("base R calculation",
          "data.table in memory", 
          "rquery in memory",
          "dplyr tbl in memory",
          "dplyr in memory no grouped filter",
          "dplyr from memory to db and back")
colormap = runs := c(highlightcolor,
                     highlightcolor,
                     highlightcolor,
                     highlightcolor,
                     highlightcolor,
                     backgroundcolor)

tr <- timings[timings$expr %in% runs, , drop=FALSE]
tr$expr <- factor(tr$expr, levels = rev(runs))
plotbenchmark(tr, colormap, 
              title = "In-Memory Runtimes by Implementation (smaller better)",
              subtitle = "Database enhanced dplyr shown for comparison")
```

![](plotexample_files/figure-markdown_github/unnamed-chunk-1-1.png)

Note: `"rquery in memory"` is `rquery` starting and finishing with in-memory `data.frame`s, the implementation uses a user chosen SQL engine (usually one of PostgreSQL, SparkSQL, or SQLite; in this case PostgreSQL).

``` r
summary <- tr %.>% 
  as.data.frame(.) %.>%
  project_nse(., groupby = "expr", 
              durationMS := avg(time)/1000000 ) %.>%
  orderby(., "durationMS") %>%
  execute(.)
baseTiming <- summary$durationMS[[which(summary$expr == 
                                          "base R calculation")]]
summary$relativeDuration <- summary$durationMS / baseTiming

knitr::kable(summary)
```

| expr                              |  durationMS|  relativeDuration|
|:----------------------------------|-----------:|-----------------:|
| base R calculation                |    111.3837|          1.000000|
| data.table in memory              |    247.3955|          2.221111|
| rquery in memory                  |    352.8966|          3.168298|
| dplyr from memory to db and back  |    609.6689|          5.473594|
| dplyr in memory no grouped filter |    832.8824|          7.477599|
| dplyr tbl in memory               |   1232.2004|         11.062667|
