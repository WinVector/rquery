PerfTest
================
2017-12-16

<!-- PerfTest.md is generated from PerfTest.Rmd. Please edit that file -->
Define and demonstrate pipelines:

``` r
DBI::dbGetQuery(my_db, paste("SELECT COUNT(1) FROM", d$table_name))
```

    ##   count(1)
    ## 1    2e+05

``` r
length(column_names(d))
```

    ## [1] 1003

``` r
sparklyr::sdf_nrow(dT)
```

    ## [1] 2e+05

``` r
sparklyr::sdf_ncol(dT)
```

    ## [1] 1003

``` r
scale <- 0.237
tng <- cdata::makeTempNameGenerator("tmptab")

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
    tnam <- tng()
    sql <- paste("CREATE TABLE", tnam, "USING PARQUET OPTIONS ('compression'='snappy') AS", 
                 sql)
    DBI::dbGetQuery(my_db, sql)
    dR <- rquery::dbi_table(my_db, tnam)
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

head(rquery_run(collect=TRUE))
```

    ##   subjectID           diagnosis probability
    ## 1         1 positive re-framing   0.6163301
    ## 2         2 withdrawal behavior   0.6706221
    ## 3         3 withdrawal behavior   0.6163301
    ## 4         4 positive re-framing   0.6706221
    ## 5         5 withdrawal behavior   0.5589742
    ## 6         6 positive re-framing   0.7658456

``` r
dR <- rquery_run(collect=FALSE) 
DBI::dbGetQuery(my_db, 
                paste("SELECT * FROM", dR$table_name, "LIMIT 6"))
```

    ##   subjectID           diagnosis probability
    ## 1         1 positive re-framing   0.6163301
    ## 2         2 withdrawal behavior   0.6706221
    ## 3         3 withdrawal behavior   0.6163301
    ## 4         4 positive re-framing   0.6706221
    ## 5         5 withdrawal behavior   0.5589742
    ## 6         6 positive re-framing   0.7658456

``` r
head(dplyr_run(narrow=FALSE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.616
    ## 2         2 withdrawal behavior       0.671
    ## 3         3 withdrawal behavior       0.616
    ## 4         4 positive re-framing       0.671
    ## 5         5 withdrawal behavior       0.559
    ## 6         6 positive re-framing       0.766

``` r
head(dplyr_run(narrow=FALSE, collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.616
    ## 2         2 withdrawal behavior       0.671
    ## 3         3 withdrawal behavior       0.616
    ## 4         4 positive re-framing       0.671
    ## 5         5 withdrawal behavior       0.559
    ## 6         6 positive re-framing       0.766

``` r
head(dplyr_run(narrow=TRUE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.616
    ## 2         2 withdrawal behavior       0.671
    ## 3         3 withdrawal behavior       0.616
    ## 4         4 positive re-framing       0.671
    ## 5         5 withdrawal behavior       0.559
    ## 6         6 positive re-framing       0.766

``` r
head(dplyr_run(narrow=TRUE, collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.616
    ## 2         2 withdrawal behavior       0.671
    ## 3         3 withdrawal behavior       0.616
    ## 4         4 positive re-framing       0.671
    ## 5         5 withdrawal behavior       0.559
    ## 6         6 positive re-framing       0.766

Get timings:

``` r
timings <- microbenchmark(rquery_run(collect=TRUE), 
                          dplyr_run(narrow=FALSE, collect=TRUE), 
                          dplyr_run(narrow=TRUE, collect=TRUE),
                          rquery_run(collect=FALSE), 
                          dplyr_run(narrow=FALSE, collect=FALSE), 
                          dplyr_run(narrow=TRUE, collect=FALSE),
                          times = 10)
```

Present results:

``` r
library("seplyr")
print(timings)
```

    ## Unit: milliseconds
    ##                                        expr       min        lq     mean
    ##                  rquery_run(collect = TRUE)  941.3908  948.0837 1061.750
    ##   dplyr_run(narrow = FALSE, collect = TRUE) 2856.9647 2984.1733 3380.033
    ##    dplyr_run(narrow = TRUE, collect = TRUE) 1728.2559 1744.8906 1815.705
    ##                 rquery_run(collect = FALSE) 1154.0354 1186.5763 1314.425
    ##  dplyr_run(narrow = FALSE, collect = FALSE) 2410.8318 2421.4615 2617.452
    ##   dplyr_run(narrow = TRUE, collect = FALSE)  922.2051  951.4038 1120.629
    ##     median       uq      max neval
    ##  1005.5630 1159.463 1344.280    10
    ##  3060.1621 3532.807 4990.685    10
    ##  1791.1318 1820.059 2103.527    10
    ##  1243.6386 1423.024 1768.138    10
    ##  2466.8532 2552.663 3887.136    10
    ##   996.7949 1055.129 2232.477    10

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

![](PerfTest_files/figure-markdown_github/present-1.png)

My guess is the timings are seeing some trends and two irregularities:

-   `rquery` is landing its results on disk in `collect=FALSE` mode, and `dplyr` is (rightly) avoiding this cost.
-   `dplyr` seems to have some extra overhead on `collect=TRUE` (which may not be that bad a thing, as truly large workloads try to avoid this step).

``` r
sparklyr::spark_disconnect(my_db)
```
