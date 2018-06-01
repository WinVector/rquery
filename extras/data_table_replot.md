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

``` r
summaries <- split(all_timings, all_timings$expr) %.>%
  lapply(., 
         function(gi) {
           model <- lm(seconds ~ nrows, data= gi)
           si <- as.data.frame(summary(model)$coefficients)
           si$coef <- rownames(si)
           si$impementation <- as.character(gi$expr[[1]])
           si
         }) %.>%
  data.table::rbindlist(.)
colnames(summaries) <- gsub("Pr(>|t|)", "P[g.t. abs(t)]", colnames(summaries), fixed = TRUE)
knitr::kable(summaries)
```

|    Estimate|  Std. Error|      t value|  P\[g.t. abs(t)\]| coef        | impementation                 |
|-----------:|-----------:|------------:|-----------------:|:------------|:------------------------------|
|  -0.3035627|   1.0797929|   -0.2811305|         0.7801355| (Intercept) | rquery\_database\_round\_trip |
|   0.0000148|   0.0000002|   97.3063224|         0.0000000| nrows       | rquery\_database\_round\_trip |
|  -0.0345898|   0.3450240|   -0.1002533|         0.9206703| (Intercept) | rquery\_data.table            |
|   0.0000015|   0.0000000|   31.8628736|         0.0000000| nrows       | rquery\_data.table            |
|  -0.0982218|   0.4016570|   -0.2445414|         0.8081272| (Intercept) | data.table                    |
|   0.0000017|   0.0000001|   30.8710124|         0.0000000| nrows       | data.table                    |
|  -0.3181092|   2.3751597|   -0.1339317|         0.8941634| (Intercept) | dplyr                         |
|   0.0000218|   0.0000003|   65.0994740|         0.0000000| nrows       | dplyr                         |
|  -0.6572150|   1.6329677|   -0.4024666|         0.6895958| (Intercept) | dplyr\_database\_round\_trip  |
|   0.0000235|   0.0000002|  102.1025822|         0.0000000| nrows       | dplyr\_database\_round\_trip  |
|  -2.1809550|   2.2043976|   -0.9893655|         0.3287426| (Intercept) | base\_r\_stats\_aggregate     |
|   0.0000194|   0.0000003|   62.6169134|         0.0000000| nrows       | base\_r\_stats\_aggregate     |
