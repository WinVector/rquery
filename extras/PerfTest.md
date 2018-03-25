PerfTest
================
2017-12-16

<!-- PerfTest.md is generated from PerfTest.Rmd. Please edit that file -->
Running on a C4.8xlarge EC2 intance.

Define and demonstrate pipelines:

``` r
DBI::dbGetQuery(my_db, paste("SELECT COUNT(1) FROM", d$table_name))
```

    ##   count(1)
    ## 1    4e+05

``` r
length(column_names(d))
```

    ## [1] 1003

``` r
sparklyr::sdf_nrow(dT)
```

    ## [1] 4e+05

``` r
sparklyr::sdf_ncol(dT)
```

    ## [1] 1003

``` r
scale <- 0.237
tng <- wrapr::mk_tmp_name_source("tmptab")

rquery_run <- function(collect) {
  dq <- d %.>%
    extend_nse(.,
               probability :=
                 exp(assessmentTotal * scale)/
                 sum(exp(assessmentTotal * scale)),
               count := count(1),
               partitionby = 'subjectID') %.>%
    extend_nse(.,
               rank := rank(),
               partitionby = 'subjectID',
               orderby = c('probability', 'surveyCategory'))  %.>%
    rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
    select_rows_nse(., rank == count) %.>%
    select_columns(., c('subjectID', 
                        'diagnosis', 
                        'probability')) %.>%
    orderby(., 'subjectID')
  sql <- to_sql(dq, my_db)
  if(collect) {
    dR <- DBI::dbGetQuery(my_db, sql)
  } else {
    # count and throw away the data
    sql <- paste("SELECT COUNT(1) FROM (", sql, ") ctab")
    dR <- DBI::dbGetQuery(my_db, sql)
  }
  dR
}


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
             sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
    arrange(probability, surveyCategory) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    rename(diagnosis = surveyCategory) %>%
    select(subjectID, diagnosis, probability) %>%
    arrange(subjectID)
  if(collect) {
    dR <- collect(dR)
  } else {
    # count and throw away the data (force calculation)
    dR <- as.data.frame(tally(dR))
  }
  dR
}

head(rquery_run(collect=TRUE))
```

    ##   subjectID           diagnosis probability
    ## 1         1 withdrawal behavior   0.6163301
    ## 2         2 positive re-framing   0.6163301
    ## 3         3 withdrawal behavior   0.8694381
    ## 4         4 withdrawal behavior   0.5589742
    ## 5         5 positive re-framing   0.7207128
    ## 6         6 withdrawal behavior   0.5000000

``` r
rquery_run(collect=FALSE) 
```

    ##   count(1)
    ## 1    2e+05

``` r
head(dplyr_run(narrow=FALSE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.616
    ## 2         2 positive re-framing       0.616
    ## 3         3 withdrawal behavior       0.869
    ## 4         4 withdrawal behavior       0.559
    ## 5         5 positive re-framing       0.721
    ## 6         6 withdrawal behavior       0.500

``` r
dplyr_run(narrow=FALSE, collect=FALSE)
```

    ##       n
    ## 1 2e+05

``` r
head(dplyr_run(narrow=TRUE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.616
    ## 2         2 positive re-framing       0.616
    ## 3         3 withdrawal behavior       0.869
    ## 4         4 withdrawal behavior       0.559
    ## 5         5 positive re-framing       0.721
    ## 6         6 withdrawal behavior       0.500

``` r
dplyr_run(narrow=TRUE, collect=FALSE)
```

    ##       n
    ## 1 2e+05

Get timings:

``` r
timings <- microbenchmark(rquery_run(collect=TRUE), 
                          rquery_run(collect=FALSE), 
                          dplyr_run(narrow=FALSE, collect=TRUE), 
                          dplyr_run(narrow=FALSE, collect=FALSE), 
                          dplyr_run(narrow=TRUE, collect=TRUE),
                          dplyr_run(narrow=TRUE, collect=FALSE))
saveRDS(timings, "PerfTest_timings.RDS")
```

Present results:

``` r
print(timings)
```

    ## Unit: milliseconds
    ##                                        expr       min        lq      mean
    ##                  rquery_run(collect = TRUE)  833.7267  929.3922  974.8569
    ##                 rquery_run(collect = FALSE)  669.4242  734.8816  774.6747
    ##   dplyr_run(narrow = FALSE, collect = TRUE) 2437.2860 2561.7170 2603.2164
    ##  dplyr_run(narrow = FALSE, collect = FALSE) 2295.2626 2399.2467 2467.7334
    ##    dplyr_run(narrow = TRUE, collect = TRUE) 1205.1181 1276.6383 1333.4385
    ##   dplyr_run(narrow = TRUE, collect = FALSE) 1027.0068 1114.8148 1173.5855
    ##     median        uq      max neval
    ##   976.3468 1004.0608 1537.182   100
    ##   762.9225  791.5005 1169.712   100
    ##  2593.1621 2638.7540 3058.012   100
    ##  2457.8133 2514.6530 3213.354   100
    ##  1328.4738 1374.1824 1839.873   100
    ##  1159.7749 1204.2670 2409.451   100

``` r
autoplot(timings)
```

![](PerfTest_files/figure-markdown_github/present-1.png)

``` r
tdf <- as.data.frame(timings)

# order the data
tdf <- tdf %.>%
  group_by_se(., "expr") %.>%
  mutate_se(., qae(mtime := median(time))) %.>%
  ungroup(.)

tdf$expr <- reorder(tdf$expr, tdf$mtime)
WVPlots::ScatterBoxPlotH(tdf, "time", "expr",  
                         pt_alpha=0.2,
                         title="Execution times in NS")
```

![](PerfTest_files/figure-markdown_github/present-2.png)

``` r
sparklyr::spark_disconnect(my_db)
```
