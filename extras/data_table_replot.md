data.table backend for rquery replot
================
John Mount, Win-Vector LLC
06/01/2018

We can work an example similar to the [`rquery`](https://winvector.github.io/rquery/) [example](https://winvector.github.io/rquery/index.html) using a [`data.table`](http://r-datatable.com/) back-end ([`rqdatatable`](https://github.com/WinVector/rqdatatable)).

``` r
library("ggplot2")
# https://github.com/WinVector/rqdatatable
library("rqdatatable") # devtools::install.packages("WinVector/rqdatatable")
```

    ## Loading required package: rquery

    ## Loading required package: wrapr

``` r
# configure a database connection
my_db <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                          host = 'localhost',
                          port = 5432,
                          user = 'johnmount',
                          password = '')
dbopts <- rq_connection_tests(my_db)
options(dbopts)
# build the shared handle
winvector_temp_db_handle <- list(db = my_db)
```

``` r
all_timings <- readRDS("all_timings.RDS")
all_timings$seconds <- all_timings$time/1e9
timings <- all_timings[all_timings$nrow == 2000000, , drop = FALSE]

cmap <- 
  c(dplyr = "#e6550d",
    dplyr_database_round_trip = "#a63603",
    data.table = "#756bb1",
    rquery_database_round_trip = "#31a354",
    rquery_data.table = "#006d2c")

# summarize by hand using rquery database connector
summary_pipeline <- timings %.>%
  as.data.frame(.) %.>%
  project_nse(., groupby = "expr", mean_time_seconds = avg(seconds)) %.>%
  orderby(., "mean_time_seconds")
means <- timings %.>% 
  as.data.frame(.) %.>%
  summary_pipeline 
knitr::kable(means)
```

| expr                          |  mean\_time\_seconds|
|:------------------------------|--------------------:|
| rquery\_data.table            |             2.706722|
| data.table                    |             2.727376|
| base\_r\_stats\_aggregate     |            23.966391|
| rquery\_database\_round\_trip |            27.142575|
| dplyr                         |            41.371857|
| dplyr\_database\_round\_trip  |            41.607898|

``` r
timings <- as.data.frame(timings)
timings$implementation <- factor(timings$expr, means$expr)
cutpt <- median(timings$seconds[timings$expr=="base_r_stats_aggregate"])
WVPlots::ScatterBoxPlotH(as.data.frame(timings), 
                         "seconds", "implementation", 
                         paste0("task time in seconds by implementation\n(",
                                timings$nrows[[1]], " row by ", timings$ncols[[1]], " column task)")) +
  geom_hline(yintercept = cutpt, linetype=2, alpha = 0.5) 
```

![](data_table_replot_files/figure-markdown_github/presenttimings-1.png)
