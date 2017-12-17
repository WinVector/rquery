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

d <- dbi_copy_to(my_db, 'd',
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
d <- dbi_table(my_db, 'dparq')
```

Define and demonstrate pipelines:

``` r
scale <- 0.237

rquery_run <- function() {
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
  dR <- dq %.>%
    to_sql(.) %.>%
    DBI::dbGetQuery(my_db, .) 
  dR
}


dplyr_run <- function() {
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
    arrange(subjectID) %>% 
    collect()
  dR
}

head(rquery_run())
```

    ##   subjectID           diagnosis probability
    ## 1         1 withdrawal behavior   0.7207128
    ## 2         2 positive re-framing   0.5589742
    ## 3         3 positive re-framing   0.5589742
    ## 4         4 withdrawal behavior   0.5589742
    ## 5         5 positive re-framing   0.5589742
    ## 6         6 withdrawal behavior   0.5000000

``` r
head(dplyr_run())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.721
    ## 2         2 positive re-framing       0.559
    ## 3         3 positive re-framing       0.559
    ## 4         4 withdrawal behavior       0.559
    ## 5         5 positive re-framing       0.559
    ## 6         6 withdrawal behavior       0.500

Get timings:

``` r
timings <- microbenchmark(rquery_run(), 
                          dplyr_run(), 
                          times = 5)
```

Present results:

``` r
print(timings)
```

    ## Unit: milliseconds
    ##          expr      min       lq     mean   median       uq      max neval
    ##  rquery_run()  937.365 1073.515 1195.104 1126.578 1294.201 1543.863     5
    ##   dplyr_run() 2954.588 3101.749 3283.109 3156.848 3410.751 3791.607     5

``` r
plot(timings)
```

![](PerfTest_files/figure-markdown_github/present-1.png)

``` r
sparklyr::spark_disconnect(my_db)
```
