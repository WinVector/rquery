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
    ## 2         2 positive re-framing   0.6706221
    ## 3         3 positive re-framing   0.8940695
    ## 4         4 withdrawal behavior   0.6163301
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
    ## 3     23268 withdrawal behavior   0.6706221
    ## 4     23269 positive re-framing   0.8401037
    ## 5     23270 withdrawal behavior   0.7207128
    ## 6     23271 positive re-framing   0.5589742

``` r
head(dplyr_run(collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.766
    ## 2         2 positive re-framing       0.671
    ## 3         3 positive re-framing       0.894
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 positive re-framing       0.559
    ## 6         6 positive re-framing       0.671

``` r
head(dplyr_run(collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.766
    ## 2         2 positive re-framing       0.671
    ## 3         3 positive re-framing       0.894
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 positive re-framing       0.559
    ## 6         6 positive re-framing       0.671

``` r
head(dplyr_narrow_run(collect=TRUE))
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.766
    ## 2         2 positive re-framing       0.671
    ## 3         3 positive re-framing       0.894
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 positive re-framing       0.559
    ## 6         6 positive re-framing       0.671

``` r
head(dplyr_narrow_run(collect=FALSE))
```

    ## # Source: lazy query [?? x 3]
    ## # Database: spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.766
    ## 2         2 positive re-framing       0.671
    ## 3         3 positive re-framing       0.894
    ## 4         4 withdrawal behavior       0.616
    ## 5         5 positive re-framing       0.559
    ## 6         6 positive re-framing       0.671

Get timings:

``` r
timings <- microbenchmark(rquery_run(collect=TRUE), 
                          dplyr_run(collect=TRUE), 
                          dplyr_narrow_run(collect=TRUE),
                          rquery_run(collect=FALSE), 
                          dplyr_run(collect=FALSE), 
                          dplyr_narrow_run(collect=FALSE),
                          times = 5)
```

Present results:

``` r
print(timings)
```

    ## Unit: milliseconds
    ##                               expr       min        lq     mean    median
    ##         rquery_run(collect = TRUE)  852.4234  883.2189 1035.888  971.1829
    ##          dplyr_run(collect = TRUE) 2846.5903 2947.5404 3221.077 2979.9342
    ##   dplyr_narrow_run(collect = TRUE) 1813.7023 1830.2815 2059.438 1868.6196
    ##        rquery_run(collect = FALSE) 1191.4176 1236.4188 1360.514 1250.1025
    ##         dplyr_run(collect = FALSE) 2412.9631 2440.9851 2760.945 2462.2562
    ##  dplyr_narrow_run(collect = FALSE) 1011.5834 1023.8456 1217.153 1024.3949
    ##        uq      max neval
    ##  1076.436 1396.178     5
    ##  3139.711 4191.610     5
    ##  1985.652 2798.933     5
    ##  1362.568 1762.064     5
    ##  2472.000 4016.522     5
    ##  1033.052 1992.887     5

``` r
tdf <- as.data.frame(timings)
tdf$expr <- reorder(tdf$expr, tdf$time)
WVPlots::ScatterBoxPlotH(tdf, "time", "expr",  
                         pt_alpha=0.2,
                         title="Execution times in NS")
```

![](PerfTest_files/figure-markdown_github/present-1.png)

``` r
sparklyr::spark_disconnect(my_db)
```
