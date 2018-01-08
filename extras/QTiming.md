QTiming
================
Win-Vector LLC
1/7/2018

Let's time both [`rquery`](https://winvector.github.io/rquery/) and [`dplyr`](https://CRAN.R-project.org/package=dplyr) on a non-trivial example.

First let's load our packages, establish a database connection, and declare an [`rquery` ad hoc execution service](https://winvector.github.io/rquery/articles/AdHocQueries.html) (the "`winvector_temp_db_handle`").

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

We now build and extended version of the example from [Let’s Have Some Sympathy For The Part-time R User](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

``` r
nrep <- 10000

dLocal <- data.frame(
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
  stringsAsFactors = FALSE)
norig <- nrow(dLocal)
dLocal <- dLocal[rep(seq_len(norig), nrep), , drop=FALSE]
dLocal$subjectID <- paste((seq_len(nrow(dLocal)) -1)%/% norig,
                      dLocal$subjectID, 
                      sep = "_")
rownames(dLocal) <- NULL
head(dLocal)
```

    ##   subjectID      surveyCategory assessmentTotal
    ## 1       0_1 withdrawal behavior               5
    ## 2       0_1 positive re-framing               2
    ## 3       0_2 withdrawal behavior               3
    ## 4       0_2 positive re-framing               4
    ## 5       1_1 withdrawal behavior               5
    ## 6       1_1 positive re-framing               2

``` r
dR <- rquery::dbi_copy_to(db, 'dR',
                  dLocal,
                  temporary = TRUE, 
                  overwrite = TRUE)
cdata::qlook(db, dR$table_name)
```

    ## table "dR" PqConnection 
    ##  nrow: 40000 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  3 variables:
    ##  $ subjectID      : chr  "0_1" "0_1" "0_2" "0_2" ...
    ##  $ surveyCategory : chr  "withdrawal behavior" "positive re-framing" "withdrawal behavior" "positive re-framing" ...
    ##  $ assessmentTotal: num  5 2 3 4 5 2 3 4 5 2

``` r
dTbl <- dplyr::tbl(db, dR$table_name)
dplyr::glimpse(dTbl)
```

    ## Observations: NA
    ## Variables: 3
    ## $ subjectID       <chr> "0_1", "0_1", "0_2", "0_2", "1_1", "1_1", "1_2...
    ## $ surveyCategory  <chr> "withdrawal behavior", "positive re-framing", ...
    ## $ assessmentTotal <dbl> 5, 2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4...

Now we declare our operation pipelines, both on local (in-memory `data.frame`) and remote (already in a database) data.

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
 dLocal %.>% 
    rquery_pipeline(.) %.>%
    as.data.frame(.) # force execution
}

rquery_database_pull <- function() {
 dR %.>% 
    rquery_pipeline(.) %.>% 
    to_sql(., db) %.>% 
    DBI::dbGetQuery(db, .) %.>%
    as.data.frame(.) # shouldn't be needed
}

rquery_database_count <- function() {
 dR %.>% 
    rquery_pipeline(.) %.>% 
    sql_node(., "n" := "COUNT(1)") %.>% 
    to_sql(., db) %.>% 
    DBI::dbGetQuery(db, .) %.>%
    as.data.frame(.) # shouldn't be needed
}

# this is a function, 
# so body not evaluated until used
dplyr_pipeline <- . %>%
  group_by(subjectID) %>%
    mutate(probability =
             exp(assessmentTotal * scale)/
             sum(exp(assessmentTotal * scale)), na.rm = TRUE) %>%
    arrange(probability, surveyCategory) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    rename(diagnosis = surveyCategory) %>%
    select(subjectID, diagnosis, probability) %>%
    arrange(subjectID)
  
dplyr_local <- function() {
  dLocal %>% 
    dplyr_pipeline
}

dplyr_database_pull <- function() {
  dTbl %>% 
    dplyr_pipeline %>%
    collect()
}

dplyr_database_count <- function() {
  dTbl %>% 
    dplyr_pipeline %>%
    tally() %>%
    collect()
}

.datatable.aware <- TRUE

data.table_local <- function() {
  dDT <- data.table::data.table(dLocal)
  dDT[
   , one := 1 ][
   , probability := exp ( assessmentTotal * scale ) / 
        sum ( exp ( assessmentTotal * scale ) ) ,subjectID ][
   , count := sum ( one ) ,subjectID ][
   , rank := rank ( probability ) ,subjectID ][
   rank == count ][
   , diagnosis := surveyCategory ][
   , c('subjectID', 'diagnosis', 'probability') ][
   order(subjectID) ]
}
```

Let's inspect the functions.

``` r
head(rquery_local())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3    1000_1 withdrawal behavior   0.6706221
    ## 4    1000_2 positive re-framing   0.5589742
    ## 5     100_1 withdrawal behavior   0.6706221
    ## 6    1001_1 withdrawal behavior   0.6706221

``` r
head(rquery_database_pull())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3    1000_1 withdrawal behavior   0.6706221
    ## 4    1000_2 positive re-framing   0.5589742
    ## 5     100_1 withdrawal behavior   0.6706221
    ## 6    1001_1 withdrawal behavior   0.6706221

``` r
rquery_database_count()
```

    ##       n
    ## 1 20000

``` r
head(dplyr_local())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1_1       withdrawal behavior       0.671
    ## 4 1_2       positive re-framing       0.559
    ## 5 10_1      withdrawal behavior       0.671
    ## 6 10_2      positive re-framing       0.559

``` r
head(dplyr_database_pull())
```

    ## Warning: Missing values are always removed in SQL.
    ## Use `sum(x, na.rm = TRUE)` to silence this warning

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1000_1    withdrawal behavior       0.671
    ## 4 1000_2    positive re-framing       0.559
    ## 5 100_1     withdrawal behavior       0.671
    ## 6 1001_1    withdrawal behavior       0.671

``` r
dplyr_database_count()
```

    ## Warning: Missing values are always removed in SQL.
    ## Use `sum(x, na.rm = TRUE)` to silence this warning

    ## # A tibble: 1 x 1
    ##   n              
    ##   <S3: integer64>
    ## 1 20000

``` r
head(data.table_local())
```

    ##    subjectID           diagnosis probability
    ## 1:       0_1 withdrawal behavior   0.6706221
    ## 2:       0_2 positive re-framing   0.5589742
    ## 3:    1000_1 withdrawal behavior   0.6706221
    ## 4:    1000_2 positive re-framing   0.5589742
    ## 5:    1001_1 withdrawal behavior   0.6706221
    ## 6:    1001_2 positive re-framing   0.5589742

I have tried to get rid of the warnings that the dplyr database pipeline is producing, but adding the "`na.rm = TRUE`" appears to have no effect.

Now let's measure the speeds with `microbenchmark`.

``` r
tm <- microbenchmark(
  nrow(rquery_local()),
  nrow(rquery_database_pull()),
  rquery_database_count(),
  nrow(dplyr_local()),
  nrow(dplyr_database_pull()),
  dplyr_database_count(),
  nrow(data.table_local())
)
saveRDS(tm, "qtimings.RDS")
print(tm)
```

    ## Unit: milliseconds
    ##                          expr       min        lq      mean    median
    ##          nrow(rquery_local())  342.2402  347.6063  360.5564  354.2437
    ##  nrow(rquery_database_pull())  234.4064  238.1877  242.5422  239.1950
    ##       rquery_database_count()  202.6709  204.5242  207.7822  205.9777
    ##           nrow(dplyr_local()) 1138.5136 1151.6660 1184.0101 1161.7648
    ##   nrow(dplyr_database_pull())  377.9763  384.2265  397.9107  385.9140
    ##        dplyr_database_count()  364.3134  369.1134  378.9403  372.3799
    ##      nrow(data.table_local())  220.0828  226.5959  240.8028  230.9515
    ##         uq       max neval
    ##   361.7870  543.9606   100
    ##   243.0044  294.4359   100
    ##   208.1822  279.4446   100
    ##  1205.4690 1518.8667   100
    ##   395.7561  547.4420   100
    ##   376.7639  524.2173   100
    ##   236.9222  315.1740   100

``` r
autoplot(tm)
```

![](QTiming_files/figure-markdown_github/timings-1.png)

`rquery` appears to be fast. The extra time for "`rquery` local" is because `rquery` doesn't *really* have a local mode, it has to copy the data to the database and back in that case. I currently guess `rquery` and `dplyr` are both picking up parallelism in the database.

Let's re-measure with `rbenchmark`,

``` r
tb <- benchmark(
  rquery_local = { nrow(rquery_local()) },
  rquery_database_pull = { nrow(rquery_database_pull()) },
  rquery_database_count = { rquery_database_count() },
  dplyr_local = { nrow(dplyr_local()) },
  dplyr_database_pull = { nrow(dplyr_database_pull()) },
  dplyr_database_count = { dplyr_database_count() },
  data.table_local = { nrow(data.table_local()) }
)
knitr::kable(tb)
```

|     | test                    |  replications|  elapsed|  relative|  user.self|  sys.self|  user.child|  sys.child|
|-----|:------------------------|-------------:|--------:|---------:|----------:|---------:|-----------:|----------:|
| 7   | data.table\_local       |           100|   24.749|     1.195|     24.333|     0.365|           0|          0|
| 6   | dplyr\_database\_count  |           100|   37.315|     1.801|     12.088|     0.049|           0|          0|
| 5   | dplyr\_database\_pull   |           100|   39.237|     1.894|     12.617|     0.280|           0|          0|
| 4   | dplyr\_local            |           100|  117.066|     5.651|    115.968|     0.780|           0|          0|
| 3   | rquery\_database\_count |           100|   20.716|     1.000|      1.990|     0.016|           0|          0|
| 2   | rquery\_database\_pull  |           100|   24.163|     1.166|      4.332|     0.221|           0|          0|
| 1   | rquery\_local           |           100|   36.048|     1.740|     12.880|     0.578|           0|          0|

And that is it. `rquery` isn't slow, even on local data!

``` r
winvector_temp_db_handle <- NULL
DBI::dbDisconnect(db)
```
