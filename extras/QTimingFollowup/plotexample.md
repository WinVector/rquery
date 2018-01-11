Plotting example
================
John Mount, Nina Zumel
January 10, 2018

As a follow-up to ["rquery: Fast Data Manipulation in R"](http://www.win-vector.com/blog/2018/01/rquery-fast-data-manipulation-in-r/) we [re-ran the experiment with a nice "base `R`" (or "pure `R`") implementation of the calculation added to the assessments](https://github.com/WinVector/rquery/blob/master/extras/QTimingFollowup/QTiming.md).

It turns out base `R` can be much faster than any of the alternatives.

That should not come as a surprise, but we think there are some current gaps in R teaching that make it surprising to many. There is a bit of a "this package is in C/C++, so it is going to be fast" fallacy. Also it has probably been a while since somebody publicly investigated exactly how large the "this package is a little slower at runtime, but the notation allows faster development" trade off actually is.

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
runs <- c("base R row calculation",
          "base R tabular calculation",
          "base R sequential calculation",
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
                     highlightcolor,
                     highlightcolor,
                     backgroundcolor)

tr <- timings[timings$expr %in% runs, , drop=FALSE]
tr$expr <- factor(tr$expr, levels = rev(runs))
sizes <- sort(unique(tr$data_size))
for(sz in sizes) {
  trs <- tr[tr$data_size==sz, , drop=FALSE]
  print(plotbenchmark(trs, colormap, 
                title = paste0("Runtimes (smaller better), data rows: ", sz),
                subtitle = "Database enhanced dplyr shown for comparison") )
}
```

![](plotexample_files/figure-markdown_github/unnamed-chunk-1-1.png)![](plotexample_files/figure-markdown_github/unnamed-chunk-1-2.png)![](plotexample_files/figure-markdown_github/unnamed-chunk-1-3.png)![](plotexample_files/figure-markdown_github/unnamed-chunk-1-4.png)![](plotexample_files/figure-markdown_github/unnamed-chunk-1-5.png)![](plotexample_files/figure-markdown_github/unnamed-chunk-1-6.png)![](plotexample_files/figure-markdown_github/unnamed-chunk-1-7.png)

Note: `"rquery in memory"` is `rquery` starting and finishing with in-memory `data.frame`s, the implementation uses a user chosen SQL engine (usually one of PostgreSQL, SparkSQL, or SQLite; in this case PostgreSQL).

``` r
summary <- tr %.>% 
  as.data.frame(.) %.>%
  project_nse(., groupby = c("expr", "data_size"), 
              durationMS := avg(time)/1000000 ) %.>%
  orderby(., c("data_size", "durationMS")) %>%
  execute(.)
baseTiming <- min(summary$durationMS)
summary$relativeDuration <- summary$durationMS / baseTiming

knitr::kable(summary)
```

| expr                              |  data\_size|    durationMS|  relativeDuration|
|:----------------------------------|-----------:|-------------:|-----------------:|
| base R row calculation            |       4e+00|  8.142369e-01|      1.000000e+00|
| base R sequential calculation     |       4e+00|  1.681830e+00|      2.065529e+00|
| base R tabular calculation        |       4e+00|  3.500383e+00|      4.298973e+00|
| data.table in memory              |       4e+00|  4.542401e+00|      5.578721e+00|
| dplyr tbl in memory               |       4e+00|  2.274884e+01|      2.793885e+01|
| dplyr in memory no grouped filter |       4e+00|  2.308071e+01|      2.834643e+01|
| rquery in memory                  |       4e+00|  4.032271e+01|      4.952208e+01|
| dplyr from memory to db and back  |       4e+00|  1.610944e+02|      1.978470e+02|
| base R sequential calculation     |       4e+01|  1.464613e+00|      1.798755e+00|
| base R row calculation            |       4e+01|  1.478195e+00|      1.815436e+00|
| base R tabular calculation        |       4e+01|  3.291921e+00|      4.042953e+00|
| data.table in memory              |       4e+01|  3.898316e+00|      4.787692e+00|
| dplyr in memory no grouped filter |       4e+01|  2.200082e+01|      2.702017e+01|
| dplyr tbl in memory               |       4e+01|  2.207029e+01|      2.710549e+01|
| rquery in memory                  |       4e+01|  4.603565e+01|      5.653840e+01|
| dplyr from memory to db and back  |       4e+01|  1.500021e+02|      1.842242e+02|
| base R sequential calculation     |       4e+02|  1.594000e+00|      1.957661e+00|
| base R tabular calculation        |       4e+02|  5.641278e+00|      6.928301e+00|
| data.table in memory              |       4e+02|  6.150488e+00|      7.553684e+00|
| base R row calculation            |       4e+02|  1.178774e+01|      1.447704e+01|
| dplyr in memory no grouped filter |       4e+02|  2.582843e+01|      3.172102e+01|
| dplyr tbl in memory               |       4e+02|  2.939436e+01|      3.610050e+01|
| rquery in memory                  |       4e+02|  4.047464e+01|      4.970868e+01|
| dplyr from memory to db and back  |       4e+02|  1.327213e+02|      1.630008e+02|
| base R sequential calculation     |       4e+03|  8.151204e+00|      1.001085e+01|
| data.table in memory              |       4e+03|  2.668934e+01|      3.277835e+01|
| base R tabular calculation        |       4e+03|  3.717665e+01|      4.565827e+01|
| rquery in memory                  |       4e+03|  6.318726e+01|      7.760305e+01|
| dplyr in memory no grouped filter |       4e+03|  9.006206e+01|      1.106092e+02|
| dplyr tbl in memory               |       4e+03|  1.225142e+02|      1.504651e+02|
| dplyr from memory to db and back  |       4e+03|  1.708256e+02|      2.097984e+02|
| base R row calculation            |       4e+03|  1.789041e+02|      2.197199e+02|
| base R sequential calculation     |       4e+04|  1.114282e+02|      1.368498e+02|
| data.table in memory              |       4e+04|  2.514926e+02|      3.088691e+02|
| rquery in memory                  |       4e+04|  3.566062e+02|      4.379637e+02|
| base R tabular calculation        |       4e+04|  5.738032e+02|      7.047128e+02|
| dplyr from memory to db and back  |       4e+04|  6.011190e+02|      7.382606e+02|
| dplyr in memory no grouped filter |       4e+04|  8.327460e+02|      1.022732e+03|
| dplyr tbl in memory               |       4e+04|  1.241371e+03|      1.524582e+03|
| base R row calculation            |       4e+04|  1.196411e+04|      1.469365e+04|
| base R sequential calculation     |       4e+05|  1.539166e+03|      1.890317e+03|
| data.table in memory              |       4e+05|  2.584407e+03|      3.174024e+03|
| rquery in memory                  |       4e+05|  4.359878e+03|      5.354557e+03|
| dplyr from memory to db and back  |       4e+05|  4.815202e+03|      5.913761e+03|
| base R tabular calculation        |       4e+05|  7.082258e+03|      8.698031e+03|
| dplyr in memory no grouped filter |       4e+05|  1.023800e+04|      1.257373e+04|
| dplyr tbl in memory               |       4e+05|  1.504158e+04|      1.847322e+04|
| base R sequential calculation     |       4e+06|  2.163105e+04|      2.656604e+04|
| data.table in memory              |       4e+06|  3.020697e+04|      3.709851e+04|
| rquery in memory                  |       4e+06|  5.274460e+04|      6.477795e+04|
| dplyr from memory to db and back  |       4e+06|  6.378353e+04|      7.833534e+04|
| base R tabular calculation        |       4e+06|  8.922455e+04|      1.095806e+05|

``` r
summary$expr <- reorder(summary$expr, -summary$durationMS)
ggplot(data=summary, aes(x=data_size, y=durationMS, color=expr)) +
  geom_point() + geom_line()
```

![](plotexample_files/figure-markdown_github/unnamed-chunk-2-1.png)
