QTiming
================
Win-Vector LLC
1/7/2018

``` r
library("rquery")
```

    ## Loading required package: wrapr

    ## Loading required package: cdata

``` r
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("microbenchmark")
library("rbenchmark")
library("ggplot2")

db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'postgres',
                     password = 'pg')
winvector_temp_db_handle <- list(db = db)
```

``` r
nrep <- 1000

dL <- data.frame(
  subjectID = c(1,                   
                1,
                2,                   
                2),
  surveyCategory = c(
    'withdrawal behavior',
    'positive re-framing',
    'withdrawal behavior',
    'positive re-framing'
  ),
  assessmentTotal = c(5,                 
                      2,
                      3,                  
                      4),
  irrelevantCol1 = "irrel1",
  irrelevantCol2 = "irrel2",
  stringsAsFactors = FALSE)
norig <- nrow(dL)
dL <- dL[rep(seq_len(norig), nrep), , drop=FALSE]
dL$subjectID <- paste((seq_len(nrow(dL)) -1)%/% norig,
                      dL$subjectID, 
                      sep = "_")
rownames(dL) <- NULL
head(dL)
```

    ##   subjectID      surveyCategory assessmentTotal irrelevantCol1
    ## 1       0_1 withdrawal behavior               5         irrel1
    ## 2       0_1 positive re-framing               2         irrel1
    ## 3       0_2 withdrawal behavior               3         irrel1
    ## 4       0_2 positive re-framing               4         irrel1
    ## 5       1_1 withdrawal behavior               5         irrel1
    ## 6       1_1 positive re-framing               2         irrel1
    ##   irrelevantCol2
    ## 1         irrel2
    ## 2         irrel2
    ## 3         irrel2
    ## 4         irrel2
    ## 5         irrel2
    ## 6         irrel2

``` r
dR <- rquery::dbi_copy_to(db, 'dR',
                  dL,
                  temporary = TRUE, 
                  overwrite = TRUE)
cdata::qlook(db, dR$table_name)
```

    ## table "dR" PqConnection 
    ##  nrow: 4000 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ subjectID      : chr  "0_1" "0_1" "0_2" "0_2" ...
    ##  $ surveyCategory : chr  "withdrawal behavior" "positive re-framing" "withdrawal behavior" "positive re-framing" ...
    ##  $ assessmentTotal: num  5 2 3 4 5 2 3 4 5 2
    ##  $ irrelevantCol1 : chr  "irrel1" "irrel1" "irrel1" "irrel1" ...
    ##  $ irrelevantCol2 : chr  "irrel2" "irrel2" "irrel2" "irrel2" ...

``` r
dT <- dplyr::tbl(db, dR$table_name)
dplyr::glimpse(dT)
```

    ## Observations: NA
    ## Variables: 5
    ## $ subjectID       <chr> "0_1", "0_1", "0_2", "0_2", "1_1", "1_1", "1_2...
    ## $ surveyCategory  <chr> "withdrawal behavior", "positive re-framing", ...
    ## $ assessmentTotal <dbl> 5, 2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4...
    ## $ irrelevantCol1  <chr> "irrel1", "irrel1", "irrel1", "irrel1", "irrel...
    ## $ irrelevantCol2  <chr> "irrel2", "irrel2", "irrel2", "irrel2", "irrel...

``` r
scale <- 0.237

# this is a function, 
# so body not evaluated until used
rquery_pipeline <- . := {
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
    orderby(., 'subjectID') 
}



rquery_local <- function() {
 dL %.>% 
    rquery_pipeline
}

rquery_database <- function() {
 dR %.>% 
    rquery_pipeline %.>% 
    to_sql(., db) %.>% 
    DBI::dbGetQuery(db, .)
}

# this is a function, 
# so body not evaluated until used
dplyr_pipeline <- . %>%
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
  
dplyr_local <- function() {
  dL %>% dplyr_pipeline
}

dplyr_database <- function() {
  dT %>% dplyr_pipeline
}
```

These timings are strange. One theory is the query cache is getting in the way at some point (possibly not everything we think is executing is executing).

``` r
tm <- microbenchmark(
  rquery_local(),
  rquery_database(),
  dplyr_local(),
  dplyr_database())
print(tm)
```

    ## Unit: milliseconds
    ##               expr        min         lq       mean     median         uq
    ##     rquery_local()   4.651667   5.055626   6.456076   5.288307   5.705836
    ##  rquery_database()  37.137599  39.251669  42.088949  40.478906  42.638339
    ##      dplyr_local() 118.475581 123.654125 132.858871 127.207603 134.630901
    ##   dplyr_database() 122.755010 125.314128 133.429221 128.493126 133.844559
    ##        max neval
    ##   72.81031   100
    ##   75.65463   100
    ##  231.02567   100
    ##  284.03794   100

``` r
autoplot(tm)
```

![](QTiming_files/figure-markdown_github/timings-1.png)

``` r
tb <- benchmark(
  rquery_local = { rquery_local() },
  rquery_database = { rquery_database() },
  dplyr_local = { dplyr_local() },
  dplyr_database = { dplyr_database() }
)
knitr::kable(tb)
```

|     | test             |  replications|  elapsed|  relative|  user.self|  sys.self|  user.child|  sys.child|
|-----|:-----------------|-------------:|--------:|---------:|----------:|---------:|-----------:|----------:|
| 4   | dplyr\_database  |           100|   13.419|    26.209|     10.867|     0.067|           0|          0|
| 3   | dplyr\_local     |           100|   12.925|    25.244|     12.793|     0.047|           0|          0|
| 2   | rquery\_database |           100|    4.136|     8.078|      2.277|     0.039|           0|          0|
| 1   | rquery\_local    |           100|    0.512|     1.000|      0.508|     0.003|           0|          0|

``` r
winvector_temp_db_handle <- NULL
DBI::dbDisconnect(db)
```
