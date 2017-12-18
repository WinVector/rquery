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
    ## 1         1 positive re-framing   0.5589742
    ## 2         2 positive re-framing   0.8056518
    ## 3         3 withdrawal behavior   0.6163301
    ## 4         4 positive re-framing   0.7207128
    ## 5         5 positive re-framing   0.7207128
    ## 6         6 withdrawal behavior   0.5589742

``` r
dR <- rquery_run(collect=FALSE) 
DBI::dbGetQuery(my_db, 
                paste("SELECT * FROM", dR$table_name, "LIMIT 6"))
```

    ##   subjectID           diagnosis probability
    ## 1     23266 withdrawal behavior   0.7207128
    ## 2     23267 positive re-framing   0.6163301
    ## 3     23268 withdrawal behavior   0.7207128
    ## 4     23269 positive re-framing   0.6706221
    ## 5     23270 withdrawal behavior   0.6163301
    ## 6     23271 withdrawal behavior   0.5000000

``` r
head(dplyr_run(narrow=FALSE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.559
    ## 2         2 positive re-framing       0.806
    ## 3         3 withdrawal behavior       0.616
    ## 4         4 positive re-framing       0.721
    ## 5         5 positive re-framing       0.721
    ## 6         6 withdrawal behavior       0.559

``` r
head(dplyr_run(narrow=FALSE, collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.559
    ## 2         2 positive re-framing       0.806
    ## 3         3 withdrawal behavior       0.616
    ## 4         4 positive re-framing       0.721
    ## 5         5 positive re-framing       0.721
    ## 6         6 withdrawal behavior       0.559

``` r
head(dplyr_run(narrow=TRUE, collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.559
    ## 2         2 positive re-framing       0.806
    ## 3         3 withdrawal behavior       0.616
    ## 4         4 positive re-framing       0.721
    ## 5         5 positive re-framing       0.721
    ## 6         6 withdrawal behavior       0.559

``` r
head(dplyr_run(narrow=TRUE, collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.559
    ## 2         2 positive re-framing       0.806
    ## 3         3 withdrawal behavior       0.616
    ## 4         4 positive re-framing       0.721
    ## 5         5 positive re-framing       0.721
    ## 6         6 withdrawal behavior       0.559

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
    ##                                        expr       min        lq      mean
    ##                  rquery_run(collect = TRUE)  842.3482  869.0729  936.7508
    ##   dplyr_run(narrow = FALSE, collect = TRUE) 2737.9100 2800.0907 2849.1706
    ##    dplyr_run(narrow = TRUE, collect = TRUE) 1680.6390 1745.3924 1811.7457
    ##                 rquery_run(collect = FALSE) 1055.3818 1135.1144 1221.7407
    ##  dplyr_run(narrow = FALSE, collect = FALSE) 2271.0574 2295.0817 2472.6400
    ##   dplyr_run(narrow = TRUE, collect = FALSE)  926.1705  932.5914 1073.8828
    ##     median        uq      max neval
    ##   910.8444  952.6031 1231.069    10
    ##  2824.5068 2875.8243 2997.982    10
    ##  1787.2461 1881.0530 2072.279    10
    ##  1196.6431 1246.5913 1566.196    10
    ##  2333.1417 2418.6568 3640.092    10
    ##   955.0510 1000.7769 2077.655    10

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
