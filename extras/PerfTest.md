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
    order_by(., 'subjectID')
  if(collect) {
    dR <- DBI::dbGetQuery(my_db, to_sql(dq))
  } else {
    tnam <- tng()
    sql <- paste("CREATE TABLE", tnam, "USING PARQUET OPTIONS ('compression'='snappy') AS", to_sql(dq))
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
    ## 1         1 positive re-framing   0.7207128
    ## 2         2 withdrawal behavior   0.7207128
    ## 3         3 withdrawal behavior   0.8694381
    ## 4         4 withdrawal behavior   0.6163301
    ## 5         5 withdrawal behavior   0.5589742
    ## 6         6 withdrawal behavior   0.5000000

``` r
dR <- rquery_run(collect=FALSE) 
DBI::dbGetQuery(my_db, 
                paste("SELECT * FROM", dR$table_name, "LIMIT 6"))
```

    ##   subjectID           diagnosis probability
    ## 1         1 positive re-framing   0.7207128
    ## 2         2 withdrawal behavior   0.7207128
    ## 3         3 withdrawal behavior   0.8694381
    ## 4         4 withdrawal behavior   0.6163301
    ## 5         5 withdrawal behavior   0.5589742
    ## 6         6 withdrawal behavior   0.5000000

``` r
head(dplyr_run(narrow=FALSE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.721
    ## 2         2 withdrawal behavior       0.721
    ## 3         3 withdrawal behavior       0.869
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 withdrawal behavior       0.559
    ## 6         6 withdrawal behavior       0.500

``` r
head(dplyr_run(narrow=FALSE, collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.721
    ## 2         2 withdrawal behavior       0.721
    ## 3         3 withdrawal behavior       0.869
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 withdrawal behavior       0.559
    ## 6         6 withdrawal behavior       0.500

``` r
head(dplyr_run(narrow=TRUE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.721
    ## 2         2 withdrawal behavior       0.721
    ## 3         3 withdrawal behavior       0.869
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 withdrawal behavior       0.559
    ## 6         6 withdrawal behavior       0.500

``` r
head(dplyr_run(narrow=TRUE, collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.721
    ## 2         2 withdrawal behavior       0.721
    ## 3         3 withdrawal behavior       0.869
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 withdrawal behavior       0.559
    ## 6         6 withdrawal behavior       0.500

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
    ##                  rquery_run(collect = TRUE)  890.9246  941.8151 1161.304
    ##   dplyr_run(narrow = FALSE, collect = TRUE) 2904.4212 3032.7618 3649.638
    ##    dplyr_run(narrow = TRUE, collect = TRUE) 1756.4779 1818.9597 2248.982
    ##                 rquery_run(collect = FALSE) 1129.4122 1154.4881 1495.734
    ##  dplyr_run(narrow = FALSE, collect = FALSE) 2502.9198 2518.5096 3061.362
    ##   dplyr_run(narrow = TRUE, collect = FALSE)  941.0073 1014.8044 1335.589
    ##    median       uq      max neval
    ##  1087.370 1246.242 1660.316    10
    ##  3690.537 3876.728 4846.509    10
    ##  2189.509 2621.193 3058.776    10
    ##  1566.103 1731.228 1934.375    10
    ##  2639.477 3272.162 5392.853    10
    ##  1247.096 1379.141 2756.205    10

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
