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
    ## 2         2 positive re-framing   0.8401037
    ## 3         3 withdrawal behavior   0.8056518
    ## 4         4 withdrawal behavior   0.5589742
    ## 5         5 positive re-framing   0.5589742
    ## 6         6 positive re-framing   0.6706221

``` r
dR <- rquery_run(collect=FALSE) 
DBI::dbGetQuery(my_db, 
                paste("SELECT * FROM", dR$table_name, "LIMIT 6"))
```

    ##   subjectID           diagnosis probability
    ## 1     23266 withdrawal behavior   0.5589742
    ## 2     23267 withdrawal behavior   0.5000000
    ## 3     23268 withdrawal behavior   0.5000000
    ## 4     23269 withdrawal behavior   0.6163301
    ## 5     23270 withdrawal behavior   0.8694381
    ## 6     23271 withdrawal behavior   0.6163301

``` r
head(dplyr_run(narrow=FALSE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.721
    ## 2         2 positive re-framing       0.840
    ## 3         3 withdrawal behavior       0.806
    ## 4         4 withdrawal behavior       0.559
    ## 5         5 positive re-framing       0.559
    ## 6         6 positive re-framing       0.671

``` r
head(dplyr_run(narrow=FALSE, collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.721
    ## 2         2 positive re-framing       0.840
    ## 3         3 withdrawal behavior       0.806
    ## 4         4 withdrawal behavior       0.559
    ## 5         5 positive re-framing       0.559
    ## 6         6 positive re-framing       0.671

``` r
head(dplyr_run(narrow=TRUE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.721
    ## 2         2 positive re-framing       0.840
    ## 3         3 withdrawal behavior       0.806
    ## 4         4 withdrawal behavior       0.559
    ## 5         5 positive re-framing       0.559
    ## 6         6 positive re-framing       0.671

``` r
head(dplyr_run(narrow=TRUE, collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.721
    ## 2         2 positive re-framing       0.840
    ## 3         3 withdrawal behavior       0.806
    ## 4         4 withdrawal behavior       0.559
    ## 5         5 positive re-framing       0.559
    ## 6         6 positive re-framing       0.671

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
    ##                  rquery_run(collect = TRUE)  891.6546  984.7117 1111.720
    ##   dplyr_run(narrow = FALSE, collect = TRUE) 2961.3765 3090.4798 3286.134
    ##    dplyr_run(narrow = TRUE, collect = TRUE) 1746.8691 1792.5143 1917.907
    ##                 rquery_run(collect = FALSE) 1145.6630 1218.2469 1380.408
    ##  dplyr_run(narrow = FALSE, collect = FALSE) 2507.8978 2515.2819 3103.473
    ##   dplyr_run(narrow = TRUE, collect = FALSE)  964.4651  994.9701 1195.040
    ##    median       uq      max neval
    ##  1040.346 1259.398 1454.955    10
    ##  3221.841 3536.910 3749.677    10
    ##  1884.773 1950.861 2409.531    10
    ##  1354.741 1454.342 1865.689    10
    ##  2793.214 3126.409 4974.525    10
    ##  1040.714 1218.468 2023.165    10

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
