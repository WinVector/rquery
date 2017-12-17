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

dT <- dplyr::tbl(my_db, d$table_name)
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
    ## 1         1 positive re-framing   0.6163301
    ## 2         2 withdrawal behavior   0.7658456
    ## 3         3 positive re-framing   0.6706221
    ## 4         4 withdrawal behavior   0.7658456
    ## 5         5 positive re-framing   0.7658456
    ## 6         6 positive re-framing   0.6706221

``` r
head(dplyr_run())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 positive re-framing       0.616
    ## 2         2 withdrawal behavior       0.766
    ## 3         3 positive re-framing       0.671
    ## 4         4 withdrawal behavior       0.766
    ## 5         5 positive re-framing       0.766
    ## 6         6 positive re-framing       0.671

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

    ## Unit: seconds
    ##          expr      min       lq     mean   median       uq      max neval
    ##  rquery_run() 5.714978 5.863546 6.029122 5.946924 6.012969 6.607193     5
    ##   dplyr_run() 7.524662 8.038975 8.211565 8.123074 8.478510 8.892605     5

``` r
plot(timings)
```

![](PerfTest_files/figure-markdown_github/present-1.png)

TODO: confirm this effect is not a query cache effect.

``` r
sparklyr::spark_disconnect(my_db)
```
