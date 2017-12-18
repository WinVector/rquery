PerfTest
================
2017-12-16

<!-- PerfTest.md is generated from PerfTest.Rmd. Please edit that file -->
Set up experiment:

``` r
library("microbenchmark")
suppressPackageStartupMessages(library("dplyr"))
library("rquery")

my_db <- sparklyr::spark_connect(version='2.2.0', 
                                 master = "local")

nSubj <- 100000
nIrrelCol <- 1000
dL <- data.frame(subjectID = sort(rep(seq_len(nSubj),2)),
                 surveyCategory = c(
                   'withdrawal behavior',
                   'positive re-framing'),
                 stringsAsFactors = FALSE)
dL$assessmentTotal <- sample.int(10, nrow(dL), replace = TRUE)
for(i in seq_len(nIrrelCol)) {
  ni <- paste("irrelevantCol", sprintf("%07g", i), sep = "_")
  dL[[ni]] <- sample(letters, size = nrow(dL), replace = TRUE)
}

d <- rquery::dbi_copy_to(my_db, 'd',
                 dL,
                 temporary = TRUE, 
                 overwrite = FALSE)
dL <- NULL

# copy to Parquet to simulate large external data source
dT <- dplyr::tbl(my_db, d$table_name)
sparklyr::spark_write_parquet(dT, "perf_tmp", mode = 'overwrite')
dplyr::db_drop_table(my_db, d$table_name)
```

    ## [1] 0

``` r
dT <- NULL
d <- NULL

# build new refs
dT <- sparklyr::spark_read_parquet(my_db, 'dparq', "perf_tmp", memory = FALSE)
d <- rquery::dbi_table(my_db, 'dparq')
```

Define and demonstrate pipelines:

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

dplyr_run <- function(collect) {
  dR <- dT %>%
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

dplyr_narrow_run <- function(collect) {
  dR <- dT %>%
    select(subjectID, surveyCategory, assessmentTotal) %>%
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
    ## 1         1 withdrawal behavior   0.7658456
    ## 2         2 withdrawal behavior   0.6163301
    ## 3         3 positive re-framing   0.7207128
    ## 4         4 withdrawal behavior   0.7207128
    ## 5         5 positive re-framing   0.8056518
    ## 6         6 positive re-framing   0.8940695

``` r
dR <- rquery_run(collect=FALSE) 
DBI::dbGetQuery(my_db, 
                paste("SELECT * FROM", dR$table_name, "LIMIT 6"))
```

    ##   subjectID           diagnosis probability
    ## 1     23266 withdrawal behavior   0.5589742
    ## 2     23267 positive re-framing   0.8401037
    ## 3     23268 withdrawal behavior   0.5000000
    ## 4     23269 positive re-framing   0.6163301
    ## 5     23270 withdrawal behavior   0.6706221
    ## 6     23271 withdrawal behavior   0.5589742

``` r
head(dplyr_run(collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.766
    ## 2         2 withdrawal behavior       0.616
    ## 3         3 positive re-framing       0.721
    ## 4         4 withdrawal behavior       0.721
    ## 5         5 positive re-framing       0.806
    ## 6         6 positive re-framing       0.894

``` r
head(dplyr_run(collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.766
    ## 2         2 withdrawal behavior       0.616
    ## 3         3 positive re-framing       0.721
    ## 4         4 withdrawal behavior       0.721
    ## 5         5 positive re-framing       0.806
    ## 6         6 positive re-framing       0.894

``` r
head(dplyr_narrow_run(collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.766
    ## 2         2 withdrawal behavior       0.616
    ## 3         3 positive re-framing       0.721
    ## 4         4 withdrawal behavior       0.721
    ## 5         5 positive re-framing       0.806
    ## 6         6 positive re-framing       0.894

``` r
head(dplyr_narrow_run(collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.766
    ## 2         2 withdrawal behavior       0.616
    ## 3         3 positive re-framing       0.721
    ## 4         4 withdrawal behavior       0.721
    ## 5         5 positive re-framing       0.806
    ## 6         6 positive re-framing       0.894

Get timings:

``` r
timings <- microbenchmark(rquery_run(collect=TRUE), 
                          dplyr_run(collect=TRUE), 
                          dplyr_narrow_run(collect=TRUE),
                          rquery_run(collect=FALSE), 
                          dplyr_run(collect=FALSE), 
                          dplyr_narrow_run(collect=FALSE),
                          times = 10)
```

Present results:

``` r
library("seplyr")
print(timings)
```

    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##         rquery_run(collect = TRUE)  874.5895  940.2566  951.2349  953.9594
    ##          dplyr_run(collect = TRUE) 2810.4525 2842.4260 2943.1012 2916.6831
    ##   dplyr_narrow_run(collect = TRUE) 1663.3073 1710.0458 1825.1881 1796.6628
    ##        rquery_run(collect = FALSE) 1006.8827 1089.1111 1174.9770 1133.8259
    ##         dplyr_run(collect = FALSE) 2431.3161 2434.2981 2642.2580 2491.4957
    ##  dplyr_narrow_run(collect = FALSE)  923.9540  928.4978 1055.5590  942.2475
    ##         uq      max neval
    ##   966.4726 1021.821    10
    ##  2981.7669 3166.250    10
    ##  1864.5365 2167.466    10
    ##  1280.6254 1420.037    10
    ##  2563.3218 3581.354    10
    ##   980.3150 1995.026    10

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

Guessing speed of the `dplyr_narrow_run(collect=FALSE)` is a sign it is probably an in-memory temp table.

``` r
sparklyr::spark_disconnect(my_db)
```
