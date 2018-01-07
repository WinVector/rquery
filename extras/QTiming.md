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
    ##  nrow: 40000 
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
 dL %.>% 
    rquery_pipeline %.>%
    as.data.frame(.) # force execution
}

rquery_database <- function() {
 dR %.>% 
    rquery_pipeline %.>% 
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
  dL %>% 
    dplyr_pipeline
}

dplyr_database <- function() {
  dT %>% 
    dplyr_pipeline %>%
    collect()
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
head(rquery_database())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3    1000_1 withdrawal behavior   0.6706221
    ## 4    1000_2 positive re-framing   0.5589742
    ## 5     100_1 withdrawal behavior   0.6706221
    ## 6    1001_1 withdrawal behavior   0.6706221

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
head(dplyr_database())
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

I have tried to get rid of the warnings that the dplyr database pipeline is producing, but adding the "`na.rm = TRUE`" appears to have no effect.

Now let's measure the speeds with `microbenchmark`.

``` r
# check from: 
# https://cran.r-project.org/web/packages/microbenchmark/microbenchmark.pdf
my_check <- function(values) {
  all(sapply(values[-1], 
             function(x) identical(values[[1]], x)))
}

tm <- microbenchmark(
  nrow(rquery_local()),
  nrow(rquery_database()),
  nrow(dplyr_local()),
  nrow(dplyr_database()),
  check=my_check)
print(tm)
```

    ## Unit: milliseconds
    ##                     expr       min        lq      mean    median        uq
    ##     nrow(rquery_local())  381.1088  399.5456  466.0389  430.1784  498.4860
    ##  nrow(rquery_database())  249.3077  257.4781  288.2167  264.4856  298.8743
    ##      nrow(dplyr_local()) 1206.1829 1243.1470 1365.7743 1314.5443 1422.0327
    ##   nrow(dplyr_database())  399.1491  410.6539  454.7992  421.1326  456.3880
    ##        max neval
    ##   860.6808   100
    ##   641.1706   100
    ##  2051.2886   100
    ##   731.2170   100

``` r
autoplot(tm)
```

![](QTiming_files/figure-markdown_github/timings-1.png)

`rquery` appears to be fast. The extra time for "`rquery` local" is because `rquery` doesn't *really* have a local mode, it has to copy the data to the database and back in that case. I currently guess `rquery` and `dplyr` are both picking up parallelism in the database.

Let's re-measure with `rbenchmark`,

``` r
tb <- benchmark(
  rquery_local = { nrow(rquery_local()) },
  rquery_database = { nrow(rquery_database()) },
  dplyr_local = { nrow(dplyr_local()) },
  dplyr_database = { nrow(dplyr_database()) }
)
knitr::kable(tb)
```

|     | test             |  replications|  elapsed|  relative|  user.self|  sys.self|  user.child|  sys.child|
|-----|:-----------------|-------------:|--------:|---------:|----------:|---------:|-----------:|----------:|
| 4   | dplyr\_database  |           100|   59.814|     2.250|     18.697|     0.435|           0|          0|
| 3   | dplyr\_local     |           100|  131.143|     4.933|    129.200|     0.864|           0|          0|
| 2   | rquery\_database |           100|   26.585|     1.000|      5.051|     0.238|           0|          0|
| 1   | rquery\_local    |           100|   42.401|     1.595|     16.124|     0.776|           0|          0|

And that is it. `rquery` isn't slow, even on local data!

``` r
winvector_temp_db_handle <- NULL
DBI::dbDisconnect(db)
```
