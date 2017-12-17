rquery
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

nSubj <- 100
nIrrelCol <- 5
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

d %.>%
  to_sql(., source_limit = 6) %.>%
  DBI::dbGetQuery(my_db, .) %.>%
  str(.)
```

    ## 'data.frame':    6 obs. of  8 variables:
    ##  $ subjectID            : int  1 1 2 2 3 3
    ##  $ surveyCategory       : chr  "withdrawal behavior" "positive re-framing" "withdrawal behavior" "positive re-framing" ...
    ##  $ assessmentTotal      : int  9 6 8 6 7 8
    ##  $ irrelevantCol_0000001: chr  "l" "p" "v" "h" ...
    ##  $ irrelevantCol_0000002: chr  "m" "n" "z" "a" ...
    ##  $ irrelevantCol_0000003: chr  "k" "u" "a" "j" ...
    ##  $ irrelevantCol_0000004: chr  "c" "r" "g" "x" ...
    ##  $ irrelevantCol_0000005: chr  "b" "i" "y" "x" ...

``` r
scale <- 0.237
```

Define and demonstrate pipelines:

``` r
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
    ## 1         1 withdrawal behavior   0.6706221
    ## 2         2 withdrawal behavior   0.6163301
    ## 3         3 positive re-framing   0.5589742
    ## 4         4 positive re-framing   0.7658456
    ## 5         5 withdrawal behavior   0.5000000
    ## 6         6 withdrawal behavior   0.5000000

``` r
head(dplyr_run())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##       <int> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.671
    ## 2         2 withdrawal behavior       0.616
    ## 3         3 positive re-framing       0.559
    ## 4         4 positive re-framing       0.766
    ## 5         5 withdrawal behavior       0.500
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
    ##  rquery_run() 293.6785 302.1365 338.0736 322.4487 369.5122 402.5923     5
    ##   dplyr_run() 508.5111 518.7965 548.0737 527.4243 543.5125 642.1240     5

``` r
plot(timings)
```

![](PerfTest_files/figure-markdown_github/present-1.png)

TODO: confirm this effect is not a query cache.

``` r
sparklyr::spark_disconnect(my_db)
```
