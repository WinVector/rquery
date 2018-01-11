QTiming
================
Win-Vector LLC
1/10/2018

Let's time [`rquery`](https://winvector.github.io/rquery/), [`dplyr`](https://CRAN.R-project.org/package=dplyr), and [`data.table`](https://CRAN.R-project.org/package=data.table) on a non-trivial example.

These timings are on an late 2014 Mac Mini with 8GB of RAM running OSX 10.12.6, version 3.4.3 (2017-11-30) -- "Kite-Eating Tree", and the current (2018-01-07) CRAN versions of all packages (except `rquery`, which is not yet up on CRAN). We are getting database services from PostgreSQL version `9.6.1` in a docker container.

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
library("ggplot2")

db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'postgres',
                     password = 'pg')
winvector_temp_db_handle <- list(db = db)

packageVersion("rquery")
```

    ## [1] '0.2.0'

``` r
packageVersion("dplyr")
```

    ## [1] '0.7.4'

``` r
packageVersion("dbplyr")
```

    ## [1] '1.2.0'

``` r
packageVersion("DBI")
```

    ## [1] '0.7'

``` r
packageVersion("data.table")
```

    ## [1] '1.10.4.3'

``` r
packageVersion("RPostgres")
```

    ## [1] '1.0.4'

``` r
print(db)
```

    ## <PqConnection> postgres@localhost:5432

``` r
DBI::dbGetQuery(db, "SELECT version()")
```

    ##                                                                                    version
    ## 1 PostgreSQL 9.6.1 on x86_64-pc-linux-gnu, compiled by gcc (Debian 4.9.2-10) 4.9.2, 64-bit

``` r
R.Version()
```

    ## $platform
    ## [1] "x86_64-apple-darwin15.6.0"
    ## 
    ## $arch
    ## [1] "x86_64"
    ## 
    ## $os
    ## [1] "darwin15.6.0"
    ## 
    ## $system
    ## [1] "x86_64, darwin15.6.0"
    ## 
    ## $status
    ## [1] ""
    ## 
    ## $major
    ## [1] "3"
    ## 
    ## $minor
    ## [1] "4.3"
    ## 
    ## $year
    ## [1] "2017"
    ## 
    ## $month
    ## [1] "11"
    ## 
    ## $day
    ## [1] "30"
    ## 
    ## $`svn rev`
    ## [1] "73796"
    ## 
    ## $language
    ## [1] "R"
    ## 
    ## $version.string
    ## [1] "R version 3.4.3 (2017-11-30)"
    ## 
    ## $nickname
    ## [1] "Kite-Eating Tree"

We now build and extended version of the example from [Let’s Have Some Sympathy For The Part-time R User](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

``` r
nrep <- 10


mkData <- function(nrep) {
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

  dLocal
}

dLocal <- mkData(nrep)
dR <- rquery::dbi_copy_to(db, 'dR',
                          dLocal,
                          temporary = TRUE, 
                          overwrite = TRUE)
dTbl <- dplyr::tbl(db, dR$table_name)

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
cdata::qlook(db, dR$table_name)
```

    ## table "dR" PqConnection 
    ##  nrow: 40 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  3 variables:
    ##  $ subjectID      : chr  "0_1" "0_1" "0_2" "0_2" ...
    ##  $ surveyCategory : chr  "withdrawal behavior" "positive re-framing" "withdrawal behavior" "positive re-framing" ...
    ##  $ assessmentTotal: num  5 2 3 4 5 2 3 4 5 2

``` r
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

# base-R function
# could also try base::split() or base:table()
base_r_calculate_tabular <- function(d) {
  d <- d[order(d$subjectID, d$surveyCategory), , drop=FALSE]
  # compute un-normalized probability
  d$probability <- exp(d$assessmentTotal * scale)
  # set up of for selection
  dmax <- stats::aggregate(d$probability, 
                           by = list(subjectID = d$subjectID), 
                           FUN = max)
  maxv <- dmax$x
  names(maxv) <- dmax$subjectID
  # set up for normalization
  dsum <- stats::aggregate(d$probability, 
                           by = list(subjectID = d$subjectID), 
                           FUN = sum)
  sumv <- dsum$x
  names(sumv) <- dsum$subjectID
  # start selection
  d$maxv <- maxv[d$subjectID]
  d <- d[d$probability >= d$maxv, 
                   , 
                   drop=FALSE]
  # de-dup
  d$rownum <- seq_len(nrow(d))
  drow <-  stats::aggregate(d$rownum, 
                           by = list(subjectID = d$subjectID), 
                           FUN = min)
  minv <- drow$x
  names(minv) <- drow$subjectID
  d$rmin <- minv[d$subjectID]
  d <- d[d$rownum <= d$rmin, , drop=FALSE]
  # renormalize
  d$probability <- d$probability/sumv[d$subjectID]
  d <- d[, c("subjectID", "surveyCategory", "probability")]
  colnames(d)[[2]] <- "diagnosis"
  d
}

# base-R function
# could also try base::split() or base:table()
base_r_calculate_sequenced <- function(d) {
  cats <- base::sort(base::unique(d$surveyCategory))
  res <- NULL
  for(ci in cats) {
    di <- d[d$surveyCategory == ci, , drop=FALSE]
    di <- di[base::order(di$subjectID), , drop=FALSE]
    di$probability <- exp(di$assessmentTotal * scale)
    if(length(base::unique(di$subjectID))!=nrow(di)) {
      stop("base_r_calculate repeated subjectID")
    }
    if(is.null(res)) {
      res <- data.frame(subjectID = di$subjectID,
                        totalProb = di$probability,
                        bestScore = di$probability,
                        diagnosis = ci,
                        stringsAsFactors = FALSE)
    } else {
      if((nrow(di)!=nrow(res)) ||
         (!all(di$subjectID == res$subjectID))) {
        stop("base_r_calculate saw irregular data")
      }
      change <- di$probability > res$bestScore
      res$diagnosis[change] <- ci
      res$bestScore <- base::pmax(res$bestScore, 
                                  di$probability)
      res$totalProb <- res$totalProb + di$probability
    }
  }
  res$probability <- res$bestScore/res$totalProb
  res <- res[, c("subjectID", 
                 "diagnosis", 
                 "probability")]
  res
}



# base-R function
# could also try base::split() or base:table()
base_r_calculate_rows <- function(d) {
  d <- d[base::order(d$subjectID, d$surveyCategory), , drop=FALSE]
  d$probability <- exp(d$assessmentTotal * scale)
  ids <- sort(unique(d$subjectID))
  totals <- numeric(length(ids))
  names(totals) <- ids
  n <- nrow(d)
  choices <- logical(n)
  sum <- 0
  maxID <- 1
  for(i in seq_len(n)) {
    id <- d$subjectID[[i]]
    probi <- d$probability[[i]]
    sum <- sum + probi
    if(probi>d$probability[[maxID]]) {
      maxID <- i
    }
    end_of_group <- (i>=n) || (d$subjectID[[i+1]]!=id)
    if(end_of_group) {
      choices[[maxID]] <- TRUE
      totals[id] <- sum
      sum <- 0
      maxID <- i+1
    }
  }
  d <- d[choices, , drop=FALSE]
  d$probability <- d$probability/totals[d$subjectID]
  d <- d[, c("subjectID", 
               "surveyCategory", 
               "probability")]
  colnames(d)[[2]] <- "diagnosis"
  d
}

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


base_R_row_calculation <- function() {
  base_r_calculate_rows(dLocal)
}

base_R_sequential_calculation <- function() {
  base_r_calculate_sequenced(dLocal)
}

base_R_tabular_calculation <- function() {
  base_r_calculate_tabular(dLocal)
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

rquery_database_land <- function() {
  tabName <- "rquery_tmpx"
  sqlc <- dR %.>% 
    rquery_pipeline(.) %.>% 
    to_sql(., db)
  DBI::dbExecute(db, paste("CREATE TABLE", tabName, "AS", sqlc))
  DBI::dbExecute(db, paste("DROP TABLE", tabName))
  NULL
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
           sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
  arrange(probability, surveyCategory) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID)

# this is a function, 
# so body not evaluated until used
# pipeline re-factored to have filter outside
# mutate 
# work around: https://github.com/tidyverse/dplyr/issues/3294
dplyr_pipeline2 <- . %>%
  group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
  arrange(probability, surveyCategory) %>%
  mutate(count = n(), rank = row_number()) %>%
  ungroup() %>%
  filter(count == rank) %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID)


dplyr_local <- function() {
  dLocal %>% 
    dplyr_pipeline
}

dplyr_local_no_grouped_filter <- function() {
  dLocal %>% 
    dplyr_pipeline2
}

dplyr_tbl <- function() {
  dLocal %>%
    as_tibble %>%
    dplyr_pipeline
}

dplyr_round_trip <- function() {
  dTmp <- dplyr::copy_to(db, dLocal, "dplyr_tmp",
                         # overwrite = TRUE,
                         temporary = TRUE
  )
  res <- dTmp %>% 
    dplyr_pipeline %>%
    collect()
  dplyr::db_drop_table(db, "dplyr_tmp")
  res
}

dplyr_database_pull <- function() {
  dTbl %>% 
    dplyr_pipeline %>%
    collect()
}

dplyr_database_land <- function() {
  tabName = "dplyr_ctmpx"
  dTbl %>% 
    dplyr_pipeline %>%
    compute(name = tabName)
  dplyr::db_drop_table(db, table = tabName)
  NULL
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
head(base_R_sequential_calculation())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3       1_1 withdrawal behavior   0.6706221
    ## 4       1_2 positive re-framing   0.5589742
    ## 5       2_1 withdrawal behavior   0.6706221
    ## 6       2_2 positive re-framing   0.5589742

``` r
head(base_R_row_calculation())
```

    ##    subjectID           diagnosis probability
    ## 1        0_1 withdrawal behavior   0.6706221
    ## 4        0_2 positive re-framing   0.5589742
    ## 5        1_1 withdrawal behavior   0.6706221
    ## 8        1_2 positive re-framing   0.5589742
    ## 9        2_1 withdrawal behavior   0.6706221
    ## 12       2_2 positive re-framing   0.5589742

``` r
head(base_R_tabular_calculation())
```

    ##    subjectID           diagnosis probability
    ## 1        0_1 withdrawal behavior   0.6706221
    ## 4        0_2 positive re-framing   0.5589742
    ## 5        1_1 withdrawal behavior   0.6706221
    ## 8        1_2 positive re-framing   0.5589742
    ## 9        2_1 withdrawal behavior   0.6706221
    ## 12       2_2 positive re-framing   0.5589742

``` r
head(rquery_local())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3       1_1 withdrawal behavior   0.6706221
    ## 4       1_2 positive re-framing   0.5589742
    ## 5       2_1 withdrawal behavior   0.6706221
    ## 6       2_2 positive re-framing   0.5589742

``` r
rquery_database_land()
```

    ## NULL

``` r
head(rquery_database_pull())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3       1_1 withdrawal behavior   0.6706221
    ## 4       1_2 positive re-framing   0.5589742
    ## 5       2_1 withdrawal behavior   0.6706221
    ## 6       2_2 positive re-framing   0.5589742

``` r
rquery_database_count()
```

    ##    n
    ## 1 20

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
    ## 5 2_1       withdrawal behavior       0.671
    ## 6 2_2       positive re-framing       0.559

``` r
head(dplyr_tbl())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1_1       withdrawal behavior       0.671
    ## 4 1_2       positive re-framing       0.559
    ## 5 2_1       withdrawal behavior       0.671
    ## 6 2_2       positive re-framing       0.559

``` r
head(dplyr_local_no_grouped_filter())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1_1       withdrawal behavior       0.671
    ## 4 1_2       positive re-framing       0.559
    ## 5 2_1       withdrawal behavior       0.671
    ## 6 2_2       positive re-framing       0.559

``` r
dplyr_database_land()
```

    ## NULL

``` r
head(dplyr_database_pull())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1_1       withdrawal behavior       0.671
    ## 4 1_2       positive re-framing       0.559
    ## 5 2_1       withdrawal behavior       0.671
    ## 6 2_2       positive re-framing       0.559

``` r
dplyr_database_count()
```

    ## # A tibble: 1 x 1
    ##   n              
    ##   <S3: integer64>
    ## 1 20

``` r
head(dplyr_round_trip())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1_1       withdrawal behavior       0.671
    ## 4 1_2       positive re-framing       0.559
    ## 5 2_1       withdrawal behavior       0.671
    ## 6 2_2       positive re-framing       0.559

``` r
head(data.table_local())
```

    ##    subjectID           diagnosis probability
    ## 1:       0_1 withdrawal behavior   0.6706221
    ## 2:       0_2 positive re-framing   0.5589742
    ## 3:       1_1 withdrawal behavior   0.6706221
    ## 4:       1_2 positive re-framing   0.5589742
    ## 5:       2_1 withdrawal behavior   0.6706221
    ## 6:       2_2 positive re-framing   0.5589742

Now let's measure the speeds with `microbenchmark`.

``` r
timings <- NULL

expressions <- list("rquery in memory" = bquote({ nrow(rquery_local())}),
    # "rquery from db to memory" =  bquote({nrow(rquery_database_pull())}),
    "rquery database count" =  bquote({rquery_database_count()}),
    "rquery database land" =  bquote({rquery_database_land()}),
    # "dplyr in memory" =  bquote({nrow(dplyr_local())}),
    "dplyr tbl in memory" =  bquote({nrow(dplyr_tbl())}),
    "dplyr in memory no grouped filter" =  bquote({nrow(dplyr_local_no_grouped_filter())}),
    "dplyr from memory to db and back" =  bquote({nrow(dplyr_round_trip())}),
    # "dplyr from db to memory" =  bquote({nrow(dplyr_database_pull())}),
    "dplyr database count" =  bquote({dplyr_database_count()}),
    "dplyr database land" =  bquote({dplyr_database_land()}),
    "data.table in memory" =  bquote({nrow(data.table_local())}),
    "base R tabular calculation" =  bquote({nrow(base_R_tabular_calculation())}),
    "base R sequential calculation" =  bquote({nrow(base_R_sequential_calculation())}),
    "base R row calculation" =  bquote({nrow(base_R_row_calculation())})
)
for(nrep in c(1, 10, 100, 1000, 10000, 100000, 1000000)) {
  print(nrep)
  dLocal <- mkData(nrep)
  dR <- rquery::dbi_copy_to(db, 'dR',
                            dLocal,
                            temporary = TRUE, 
                            overwrite = TRUE)
  dTbl <- dplyr::tbl(db, dR$table_name)
  tm <- microbenchmark(
    list = expressions,
    times = 10L
  )
  print(tm)
  print(autoplot(tm))
  tmi <- as.data.frame(tm)
  tmi$data_size <- nrow(dLocal)
  timings <- rbind(timings, tmi)
  baddies <- unique(tmi$expr[tmi$time > 10*1e+9])
  for(bi in baddies) {
    expressions[[bi]] <- NULL
  }
  if(length(expressions)<=0) {
    break
  }
}
```

    ## [1] 1
    ## Unit: microseconds
    ##                               expr        min         lq        mean
    ##                   rquery in memory  35900.235  36351.614  40322.7052
    ##              rquery database count  20234.340  20753.814  24311.9709
    ##               rquery database land  27614.480  30158.037  37775.8202
    ##                dplyr tbl in memory  17783.726  18878.386  22748.8414
    ##  dplyr in memory no grouped filter  18219.920  19994.613  23080.7114
    ##   dplyr from memory to db and back 124483.124 133585.102 161094.3503
    ##               dplyr database count 120352.017 120765.122 131245.6212
    ##                dplyr database land 140773.786 143216.490 159490.1057
    ##               data.table in memory   3165.023   3530.426   4542.4007
    ##         base R tabular calculation   2545.961   2583.026   3500.3826
    ##      base R sequential calculation   1171.841   1209.914   1681.8296
    ##             base R row calculation    353.512    451.247    814.2369
    ##      median         uq        max neval
    ##   39542.440  44490.176  46796.527    10
    ##   23606.214  26097.230  34653.887    10
    ##   35271.435  45518.747  58011.260    10
    ##   20412.376  23193.332  35969.329    10
    ##   22472.243  23284.377  34821.431    10
    ##  147502.280 179754.227 251800.594    10
    ##  127304.590 138163.477 152617.722    10
    ##  150697.146 181457.361 189827.097    10
    ##    4318.716   5234.930   7424.208    10
    ##    3155.713   4309.442   5233.807    10
    ##    1482.299   2166.654   2750.652    10
    ##     489.085    559.960   3840.723    10

![](QTiming_files/figure-markdown_github/timings-1.png)

    ## [1] 10
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##                   rquery in memory  39.519630  43.299780  46.035652
    ##              rquery database count  20.308965  21.312391  24.064010
    ##               rquery database land  27.982688  28.564395  32.747605
    ##                dplyr tbl in memory  18.033756  18.823407  22.070289
    ##  dplyr in memory no grouped filter  18.110649  19.091273  22.000820
    ##   dplyr from memory to db and back 124.121267 130.297600 150.002139
    ##               dplyr database count 118.513779 127.849760 139.677531
    ##                dplyr database land 140.952856 142.003254 152.160042
    ##               data.table in memory   3.237425   3.306937   3.898316
    ##         base R tabular calculation   2.774416   2.943523   3.291921
    ##      base R sequential calculation   1.017442   1.114556   1.464613
    ##             base R row calculation   1.259414   1.341766   1.478195
    ##      median         uq        max neval
    ##   44.076175  45.244997  67.283705    10
    ##   24.320788  26.150987  29.971522    10
    ##   31.345512  36.595156  41.003123    10
    ##   21.969776  24.039479  27.360250    10
    ##   21.036632  23.137066  31.878694    10
    ##  151.406093 160.474328 178.940453    10
    ##  138.553226 154.641486 165.839971    10
    ##  146.116432 153.795371 179.973184    10
    ##    3.369938   4.866950   4.918845    10
    ##    3.303997   3.589392   3.727910    10
    ##    1.177318   1.351494   3.436733    10
    ##    1.365555   1.728630   1.843706    10

![](QTiming_files/figure-markdown_github/timings-2.png)

    ## [1] 100
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##                   rquery in memory  37.752357  37.925336  40.474644
    ##              rquery database count  21.388994  21.931447  23.064862
    ##               rquery database land  29.009633  29.837551  32.019081
    ##                dplyr tbl in memory  26.358110  28.074371  29.394359
    ##  dplyr in memory no grouped filter  24.333913  24.657681  25.828429
    ##   dplyr from memory to db and back 126.340118 129.616127 132.721306
    ##               dplyr database count 120.035108 121.304923 123.002936
    ##                dplyr database land 140.698485 143.642318 145.863168
    ##               data.table in memory   5.315258   5.411258   6.150488
    ##         base R tabular calculation   5.312807   5.319417   5.641278
    ##      base R sequential calculation   1.331077   1.459879   1.594000
    ##             base R row calculation  10.673533  10.951874  11.787738
    ##      median         uq        max neval
    ##   38.992238  41.208295  49.734612    10
    ##   22.944729  23.967484  26.016197    10
    ##   32.277925  33.562566  36.623859    10
    ##   28.917100  29.326388  37.561652    10
    ##   25.003409  27.221667  29.626067    10
    ##  130.970170 135.914716 145.485808    10
    ##  122.473010 123.260690 129.329149    10
    ##  144.625954 148.764621 152.438220    10
    ##    5.675505   6.030981   8.532247    10
    ##    5.464787   5.723722   6.599830    10
    ##    1.498816   1.538982   2.280969    10
    ##   11.255852  11.675341  15.023592    10

![](QTiming_files/figure-markdown_github/timings-3.png)

    ## [1] 1000
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##                   rquery in memory  60.734028  61.937492  63.187264
    ##              rquery database count  34.988913  35.613665  37.221241
    ##               rquery database land  43.971419  45.107840  45.994600
    ##                dplyr tbl in memory 119.913767 121.182710 122.514203
    ##  dplyr in memory no grouped filter  86.079751  87.940632  90.062056
    ##   dplyr from memory to db and back 162.710099 164.594946 170.825584
    ##               dplyr database count 135.902716 137.909109 140.774541
    ##                dplyr database land 161.271576 163.105720 167.062759
    ##               data.table in memory  24.674876  25.296985  26.689344
    ##         base R tabular calculation  35.789179  36.190746  37.176648
    ##      base R sequential calculation   7.625182   7.693666   8.151204
    ##             base R row calculation 159.806049 165.042566 178.904051
    ##      median         uq        max neval
    ##   62.178098  62.781297  72.519621    10
    ##   36.842852  38.995001  40.717973    10
    ##   45.532522  46.558700  50.797217    10
    ##  122.422809 123.779329 125.554146    10
    ##   89.042677  89.623541  97.256106    10
    ##  169.322960 171.290190 192.195970    10
    ##  140.322323 142.269621 149.556393    10
    ##  167.432609 169.089103 175.610820    10
    ##   26.698448  27.679296  29.801339    10
    ##   36.658907  37.642448  40.147823    10
    ##    7.804708   8.160978   9.749994    10
    ##  172.966504 181.841846 250.206360    10

![](QTiming_files/figure-markdown_github/timings-4.png)

    ## [1] 10000
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##                   rquery in memory   343.5841   348.6992   356.6062
    ##              rquery database count   202.3400   202.5900   208.3433
    ##               rquery database land   221.0918   222.1335   225.9282
    ##                dplyr tbl in memory  1201.3678  1216.5000  1241.3705
    ##  dplyr in memory no grouped filter   815.6988   823.6222   832.7460
    ##   dplyr from memory to db and back   587.0350   594.5619   601.1190
    ##               dplyr database count   365.0578   369.1380   374.9252
    ##                dplyr database land   413.7077   415.2548   433.9779
    ##               data.table in memory   232.3260   237.0731   251.4926
    ##         base R tabular calculation   483.5251   566.9789   573.8032
    ##      base R sequential calculation   106.4228   109.1746   111.4282
    ##             base R row calculation 11876.2306 11940.7862 11964.1120
    ##      median         uq        max neval
    ##    353.1696   356.2464   379.8709    10
    ##    205.0670   211.1211   223.1072    10
    ##    224.7556   229.1216   233.7671    10
    ##   1230.5292  1280.2709  1304.5171    10
    ##    829.8446   839.5780   856.6560    10
    ##    595.9018   611.4529   619.9026    10
    ##    372.2518   384.9621   389.1394    10
    ##    416.7694   442.3192   533.1200    10
    ##    245.1721   247.3569   324.0880    10
    ##    577.9346   583.2353   663.8148    10
    ##    111.0274   111.8200   124.1525    10
    ##  11967.1380 11979.8516 12038.9739    10

![](QTiming_files/figure-markdown_github/timings-5.png)

    ## [1] 1e+05
    ## Unit: seconds
    ##                               expr       min        lq      mean    median
    ##                   rquery in memory  4.150865  4.311680  4.359878  4.350870
    ##              rquery database count  2.895449  2.903284  2.943552  2.934309
    ##               rquery database land  3.041163  3.052214  3.083266  3.063201
    ##                dplyr tbl in memory 14.550632 14.854923 15.041578 14.978318
    ##  dplyr in memory no grouped filter  9.901734 10.016488 10.237998 10.190815
    ##   dplyr from memory to db and back  4.695623  4.738706  4.815202  4.808099
    ##               dplyr database count  3.251765  3.285252  3.358889  3.362887
    ##                dplyr database land  3.391732  3.426520  3.481695  3.477398
    ##               data.table in memory  2.497895  2.533746  2.584407  2.582926
    ##         base R tabular calculation  6.785277  7.005090  7.082258  7.037319
    ##      base R sequential calculation  1.471981  1.486844  1.539166  1.536554
    ##         uq       max neval
    ##   4.416104  4.618364    10
    ##   2.995430  3.002913    10
    ##   3.124392  3.174039    10
    ##  15.099404 16.150974    10
    ##  10.438355 10.681118    10
    ##   4.839697  5.044050    10
    ##   3.431452  3.441929    10
    ##   3.541202  3.572980    10
    ##   2.609445  2.756181    10
    ##   7.279753  7.349476    10
    ##   1.571751  1.633308    10

![](QTiming_files/figure-markdown_github/timings-6.png)

    ## [1] 1e+06
    ## Unit: seconds
    ##                              expr      min       lq     mean   median
    ##                  rquery in memory 50.99813 51.05550 52.74460 52.83717
    ##             rquery database count 37.06661 37.21231 37.65776 37.33732
    ##              rquery database land 38.16104 38.74123 39.84951 39.80365
    ##  dplyr from memory to db and back 59.99884 61.52042 63.78353 62.80150
    ##              dplyr database count 45.82290 45.92967 46.45309 46.14583
    ##               dplyr database land 46.27345 46.92582 48.49090 47.53372
    ##              data.table in memory 27.37473 28.83352 30.20697 29.82049
    ##        base R tabular calculation 84.23374 86.52253 89.22455 86.95730
    ##     base R sequential calculation 20.85254 21.23680 21.63105 21.48766
    ##        uq       max neval
    ##  53.75544  56.77225    10
    ##  37.58334  39.20013    10
    ##  40.94334  41.86709    10
    ##  64.87975  73.58398    10
    ##  46.38777  49.13521    10
    ##  47.87087  59.17003    10
    ##  30.64157  34.17469    10
    ##  88.37730 103.78354    10
    ##  22.14852  22.42634    10

![](QTiming_files/figure-markdown_github/timings-7.png)

``` r
saveRDS(timings, "qtimings.RDS")
```

`rquery` appears to be fast. The extra time for "`rquery` local" is because `rquery` doesn't *really* have a local mode, it has to copy the data to the database and back in that case.

``` r
sessionInfo()
```

    ## R version 3.4.3 (2017-11-30)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Sierra 10.12.6
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] bindrcpp_0.2         ggplot2_2.2.1        microbenchmark_1.4-3
    ## [4] dplyr_0.7.4          rquery_0.2.0         cdata_0.5.1         
    ## [7] wrapr_1.1.1         
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.14.2      dbplyr_1.2.0        pillar_1.0.1       
    ##  [4] compiler_3.4.3      plyr_1.8.4          bindr_0.1          
    ##  [7] tools_3.4.3         RPostgres_1.0-4     digest_0.6.13      
    ## [10] bit_1.1-12          evaluate_0.10.1     tibble_1.4.1       
    ## [13] gtable_0.2.0        pkgconfig_2.0.1     rlang_0.1.6        
    ## [16] cli_1.0.0           DBI_0.7             yaml_2.1.16        
    ## [19] withr_2.1.1         stringr_1.2.0       knitr_1.18         
    ## [22] hms_0.4.0           tidyselect_0.2.3    rprojroot_1.3-2    
    ## [25] bit64_0.9-7         grid_3.4.3          data.table_1.10.4-3
    ## [28] glue_1.2.0          R6_2.2.2            rmarkdown_1.8      
    ## [31] purrr_0.2.4         blob_1.1.0          magrittr_1.5       
    ## [34] backports_1.1.2     scales_0.5.0        htmltools_0.3.6    
    ## [37] assertthat_0.2.0    colorspace_1.3-2    utf8_1.1.3         
    ## [40] stringi_1.1.6       lazyeval_0.2.1      munsell_0.4.3      
    ## [43] crayon_1.3.4

``` r
winvector_temp_db_handle <- NULL
DBI::dbDisconnect(db)
```
