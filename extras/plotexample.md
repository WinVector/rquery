Plotting example
================
Nina Zumel
January 8, 2018

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

source("plotbenchmark.R")

timings = readRDS("qtimings.RDS")
timings$expr <- as.character(timings$expr)

# colors
highlightcolor = "#33a02c"
backgroundcolor = "#a6cee3"
reruncolor = "#b2df8a"
greycolor = "darkgrey"
```

``` r
runs <- c("data.table in memory", 
          "rquery in memory",
          "dplyr tbl in memory",
          "dplyr in memory no grouped filter",
          "dplyr from memory to db and back")
colormap = runs := c(highlightcolor,
                     highlightcolor,
                     highlightcolor,
                     highlightcolor,
                     backgroundcolor)

tr <- timings[timings$expr %in% runs, , drop=FALSE]
tr$expr <- factor(tr$expr, levels = rev(runs))
plotbenchmark(tr, colormap, 
              title = "In-Memory Task Runtimes by Implementation",
              subtitle = "Database enhanced dplyr shown for comparison")
```

![](plotexample_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
runs <- c("data.table in memory", 
          "rquery in memory",
          "rquery database count",
          "rquery database land",
          "dplyr tbl in memory",
          "dplyr in memory no grouped filter",
          "dplyr from memory to db and back",
          "dplyr database count",
          "dplyr database land")
colormap = runs := c(greycolor,
                     greycolor,
                     highlightcolor,
                     highlightcolor,
                     greycolor,
                     greycolor,
                     greycolor,
                     highlightcolor,
                     highlightcolor)

tr <- timings[timings$expr %in% runs, , drop=FALSE]
tr$expr <- factor(tr$expr, levels = rev(runs))
plotbenchmark(tr, colormap, 
              title = "Pure Database Task Runtimes by Implementation",
              subtitle = "In-memory task runtimes included for comparison")
```

![](plotexample_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
followups <- c("rquery in memory",
               "rquery from db to memory", 
               "rquery database land", 
               "rquery database count", 
               "dplyr from memory to db and back",
               "dplyr from db to memory",
               "dplyr database land",
               "dplyr database count")
colormap = followups := c(greycolor,
                          backgroundcolor,
                          highlightcolor,
                          reruncolor,
                          greycolor,
                          backgroundcolor,
                          highlightcolor,
                          reruncolor)
tf <- timings[timings$expr %in% followups, , drop=FALSE]
tf$expr <- factor(tf$expr, levels = rev(followups))
plotbenchmark(tf, colormap, "Breakdown of Database Transport Costs")
```

![](plotexample_files/figure-markdown_github/unnamed-chunk-1-3.png)
