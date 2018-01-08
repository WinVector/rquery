Plotting example
================
Nina Zumel
January 8, 2018

``` r
timings = readRDS("qtimings.RDS")
timings$expr <- as.character(timings$expr)

# colors
highlightcolor = "#b2df8a"
backgroundcolor = "#a6cee3"

runs <- c("nrow(data.table_local())", 
          "nrow(rquery_local())",
          "nrow(dplyr_local())",
          "nrow(dplyr_round_trip())")
colormap = runs := c(highlightcolor,
                     highlightcolor,
                     highlightcolor,
                     backgroundcolor)

tr <- timings[timings$expr %in% runs, , drop=FALSE]
tr$expr <- factor(tr$expr, levels = rev(runs))
plotbenchmark(tr, colormap, "Comparison of Task Runtimes by Implementation")
```

![](plotexample_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
followups <- c("nrow(rquery_local())",
               "nrow(rquery_database_pull())", 
               "rquery_database_count()", 
               "nrow(dplyr_round_trip())",
               "nrow(dplyr_database_pull())",
               "dplyr_database_count()")     
colormap = followups := c(highlightcolor,
                          backgroundcolor,
                          backgroundcolor,
                          highlightcolor,
                          backgroundcolor,
                          backgroundcolor)
tf <- timings[timings$expr %in% followups, , drop=FALSE]
tf$expr <- factor(tf$expr, levels = rev(followups))
plotbenchmark(tf, colormap, "Breakdown of Transport Costs")
```

![](plotexample_files/figure-markdown_github/unnamed-chunk-1-2.png)
