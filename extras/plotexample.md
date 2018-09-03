Plotting example
================
Nina Zumel

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

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
```

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     coalesce

``` r
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
          "rqdatatable",
          "dplyr tbl in memory",
          "dplyr in memory no grouped filter")
colormap = runs := c(highlightcolor,
                     highlightcolor,
                     backgroundcolor,
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
          "rqdatatable",
          "rquery database roundtrip",
          "rquery database land",
          "dplyr tbl in memory",
          "dplyr in memory no grouped filter",
          "dplyr from memory to db and back",
          "dplyr database land")
colormap = runs := c(greycolor,
                     greycolor,
                     highlightcolor,
                     highlightcolor,
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
