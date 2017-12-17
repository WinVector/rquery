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

dplyr_narrow_run <- function() {
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
    arrange(subjectID) %>% 
    collect()
  dR
}


head(rquery_run())
```

    ##   subjectID           diagnosis probability
    ## 1         1 positive re-framing   0.8056518
    ## 2         2 withdrawal behavior   0.7207128
    ## 3         3 withdrawal behavior   0.7207128
    ## 4         4 positive re-framing   0.6163301
    ## 5         5 positive re-framing   0.7658456
    ## 6         6 positive re-framing   0.8056518

``` r
head(dplyr_run())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.806
    ## 2         2 withdrawal behavior       0.721
    ## 3         3 withdrawal behavior       0.721
    ## 4         4 positive re-framing       0.616
    ## 5         5 positive re-framing       0.766
    ## 6         6 positive re-framing       0.806

``` r
head(dplyr_narrow_run())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.806
    ## 2         2 withdrawal behavior       0.721
    ## 3         3 withdrawal behavior       0.721
    ## 4         4 positive re-framing       0.616
    ## 5         5 positive re-framing       0.766
    ## 6         6 positive re-framing       0.806

Get timings:

``` r
timings <- microbenchmark(rquery_run(), 
                          dplyr_run(), 
                          dplyr_narrow_run(),
                          times = 5)
```

Present results:

``` r
print(timings)
```

    ## Unit: milliseconds
    ##                expr       min        lq     mean   median       uq
    ##        rquery_run()  877.9828  921.8874 1084.964  964.549 1178.351
    ##         dplyr_run() 3035.3073 3066.5757 3224.544 3157.523 3288.035
    ##  dplyr_narrow_run() 1707.0174 1753.3946 1818.521 1845.908 1859.348
    ##       max neval
    ##  1482.051     5
    ##  3575.279     5
    ##  1926.936     5

``` r
plot(timings)
```

![](PerfTest_files/figure-markdown_github/present-1.png)

``` r
sparklyr::spark_disconnect(my_db)
```
