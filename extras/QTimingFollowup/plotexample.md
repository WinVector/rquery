Plotting example
================
John Mount, Nina Zumel
January 10, 2018

As a follow-up to ["rquery: Fast Data Manipulation in R"](http://www.win-vector.com/blog/2018/01/rquery-fast-data-manipulation-in-r/) I re-ran the experiment with a nice "base R" implementation of the calculation added to the assessments. It turns out base R is much faster than any of the alternatives.

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

``` r
summary <- tr %.>% 
  as.data.frame(.) %.>%
  project_nse(., "expr", durationMS = avg(time)/1000000 ) %.>%
  orderby(., "durationMS") %>%
  execute(.)
baseTiming <- summary$durationMS[[which(summary$expr == 
                                          "base R calculation")]]
summary$relativeDuration <- summary$durationMS / baseTiming

knitr::kable(summary)
```

| expr                              |  durationMS|  relativeDuration|
|:----------------------------------|-----------:|-----------------:|
| base R calculation                |    111.3464|          1.000000|
| data.table in memory              |    246.1304|          2.210492|
| rquery in memory                  |    353.6315|          3.175958|
| dplyr from memory to db and back  |    607.0985|          5.452340|
| dplyr in memory no grouped filter |    834.1066|          7.491096|
| dplyr tbl in memory               |   1211.7760|         10.882937|
