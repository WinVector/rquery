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
    ## 1         1 positive re-framing   0.6706221
    ## 2         2 positive re-framing   0.5589742
    ## 3         3 positive re-framing   0.7658456
    ## 4         4 withdrawal behavior   0.6163301
    ## 5         5 withdrawal behavior   0.5000000
    ## 6         6 withdrawal behavior   0.5000000

``` r
dR <- rquery_run(collect=FALSE) 
DBI::dbGetQuery(my_db, 
                paste("SELECT * FROM", dR$table_name, "LIMIT 6"))
```

    ##   subjectID           diagnosis probability
    ## 1     23266 positive re-framing   0.8401037
    ## 2     23267 withdrawal behavior   0.5589742
    ## 3     23268 withdrawal behavior   0.5589742
    ## 4     23269 withdrawal behavior   0.5589742
    ## 5     23270 positive re-framing   0.8056518
    ## 6     23271 withdrawal behavior   0.6163301

``` r
head(dplyr_run(collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.671
    ## 2         2 positive re-framing       0.559
    ## 3         3 positive re-framing       0.766
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 withdrawal behavior       0.500
    ## 6         6 withdrawal behavior       0.500

``` r
head(dplyr_run(collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.671
    ## 2         2 positive re-framing       0.559
    ## 3         3 positive re-framing       0.766
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 withdrawal behavior       0.500
    ## 6         6 withdrawal behavior       0.500

``` r
head(dplyr_narrow_run(collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.671
    ## 2         2 positive re-framing       0.559
    ## 3         3 positive re-framing       0.766
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 withdrawal behavior       0.500
    ## 6         6 withdrawal behavior       0.500

``` r
head(dplyr_narrow_run(collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.671
    ## 2         2 positive re-framing       0.559
    ## 3         3 positive re-framing       0.766
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 withdrawal behavior       0.500
    ## 6         6 withdrawal behavior       0.500

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
    ##         rquery_run(collect = TRUE)  892.0101  896.8602  994.2866  972.2644
    ##          dplyr_run(collect = TRUE) 2884.5172 2946.1438 3085.2615 3063.6427
    ##   dplyr_narrow_run(collect = TRUE) 1713.5280 1763.7386 1859.0310 1798.0427
    ##        rquery_run(collect = FALSE) 1104.3129 1149.7069 1300.5927 1273.1459
    ##         dplyr_run(collect = FALSE) 2441.4055 2463.1594 2671.1164 2507.0583
    ##  dplyr_narrow_run(collect = FALSE)  963.2254  995.2204 1145.9472 1045.4177
    ##        uq      max neval
    ##  1058.638 1205.634    10
    ##  3178.923 3492.271    10
    ##  1944.545 2194.239    10
    ##  1337.420 1704.278    10
    ##  2601.071 4080.219    10
    ##  1120.694 2107.208    10

``` r
tdf <- as.data.frame(timings)
tdf <- tdf %.>%
  group_by_se(., "expr") %.>%
  mutate_se(., qae(mtime := median(time)))

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
