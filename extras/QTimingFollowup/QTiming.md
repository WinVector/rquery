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
    # "base R row calculation" =  bquote({nrow(base_R_row_calculation())}),
    "base R tabular calculation" =  bquote({nrow(base_R_tabular_calculation())}),
    "base R sequential calculation" =  bquote({nrow(base_R_sequential_calculation())})
)

prune <- FALSE

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
  if(prune) {
    baddies <- unique(tmi$expr[tmi$time > 10*1e+9])
    for(bi in baddies) {
      expressions[[bi]] <- NULL
    }
    if(length(expressions)<=0) {
      break
    }
  }
}
```

    ## [1] 1
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##                   rquery in memory  35.275822  35.459316  36.706637
    ##              rquery database count  20.968550  22.110666  25.394548
    ##               rquery database land  27.866863  28.796505  30.283396
    ##                dplyr tbl in memory  18.002100  18.372758  20.497336
    ##  dplyr in memory no grouped filter  19.167499  19.792182  23.773914
    ##   dplyr from memory to db and back 126.244953 128.213893 135.011388
    ##               dplyr database count 120.086749 122.044516 133.587845
    ##                dplyr database land 143.258933 143.815383 146.665686
    ##               data.table in memory   3.221432   3.335601   3.459755
    ##         base R tabular calculation   2.575181   2.592733   2.944919
    ##      base R sequential calculation   1.037406   1.088024   1.301663
    ##      median         uq        max neval
    ##   36.684519  37.474583  39.454573    10
    ##   22.974068  27.687178  39.584981    10
    ##   29.998531  31.653162  33.388999    10
    ##   19.224795  22.900207  25.514718    10
    ##   20.853391  25.694889  42.495387    10
    ##  129.954030 140.455800 151.757783    10
    ##  122.775190 126.032123 209.369473    10
    ##  144.710857 146.994803 154.484835    10
    ##    3.466945   3.624933   3.684667    10
    ##    2.806853   2.952123   4.419043    10
    ##    1.136255   1.335111   2.532545    10

![](QTiming_files/figure-markdown_github/timings-1.png)

    ## [1] 10
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##                   rquery in memory  34.186959  36.312021  37.721733
    ##              rquery database count  19.768439  20.364913  21.662205
    ##               rquery database land  28.343121  28.803452  29.741973
    ##                dplyr tbl in memory  17.709002  18.207098  19.754256
    ##  dplyr in memory no grouped filter  19.056936  19.566140  19.959601
    ##   dplyr from memory to db and back 124.708300 127.280218 130.425726
    ##               dplyr database count 120.867955 121.209250 123.510832
    ##                dplyr database land 142.220359 143.269195 144.689622
    ##               data.table in memory   3.272065   3.341882   3.522076
    ##         base R tabular calculation   2.742046   2.806366   2.908628
    ##      base R sequential calculation   1.056398   1.090940   1.329385
    ##      median         uq        max neval
    ##   37.548345  39.798456  40.702955    10
    ##   20.937771  23.867173  24.491327    10
    ##   29.603540  30.402717  32.245322    10
    ##   18.942428  19.357534  25.946998    10
    ##   19.836039  19.952188  21.806798    10
    ##  128.323673 129.077197 152.659083    10
    ##  123.561470 124.386850 127.384125    10
    ##  144.452221 145.714166 148.817203    10
    ##    3.401024   3.791001   4.016664    10
    ##    2.896818   3.022597   3.075682    10
    ##    1.117241   1.180817   3.182559    10

![](QTiming_files/figure-markdown_github/timings-2.png)

    ## [1] 100
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##                   rquery in memory  36.691947  38.764010  39.399514
    ##              rquery database count  21.190458  22.229819  23.509549
    ##               rquery database land  30.054286  30.255128  34.040829
    ##                dplyr tbl in memory  26.640734  26.753023  28.705963
    ##  dplyr in memory no grouped filter  24.424390  24.721543  26.694084
    ##   dplyr from memory to db and back 128.383756 128.900060 132.602552
    ##               dplyr database count 120.996916 122.176968 128.787012
    ##                dplyr database land 142.748124 143.258938 151.774151
    ##               data.table in memory   5.253314   5.538949   5.971650
    ##         base R tabular calculation   5.327949   5.370912   5.564920
    ##      base R sequential calculation   1.411605   1.507603   1.755118
    ##      median         uq        max neval
    ##   39.227018  39.806816  42.535729    10
    ##   22.779916  24.569199  26.921635    10
    ##   31.073324  31.788094  61.027302    10
    ##   28.513522  30.550003  31.003563    10
    ##   26.882137  27.645667  30.685553    10
    ##  130.596852 132.864990 148.461020    10
    ##  122.932785 125.106899 179.788278    10
    ##  145.857548 149.000882 192.630536    10
    ##    5.891063   6.372643   7.217993    10
    ##    5.502181   5.770583   5.958840    10
    ##    1.541833   1.663978   3.628899    10

![](QTiming_files/figure-markdown_github/timings-3.png)

    ## [1] 1000
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##                   rquery in memory  62.240949  63.497385  65.613117
    ##              rquery database count  34.784402  35.986230  37.075851
    ##               rquery database land  43.845733  45.224200  49.663749
    ##                dplyr tbl in memory 122.956474 123.173159 127.966278
    ##  dplyr in memory no grouped filter  87.037731  89.760679  92.429663
    ##   dplyr from memory to db and back 165.232936 166.213898 176.439299
    ##               dplyr database count 138.212064 139.808216 146.840964
    ##                dplyr database land 163.322838 164.373917 175.669322
    ##               data.table in memory  24.943112  26.848204  29.264435
    ##         base R tabular calculation  34.923932  35.517849  36.963948
    ##      base R sequential calculation   7.710513   7.785088   8.870607
    ##      median         uq       max neval
    ##   64.394587  66.036390  76.01949    10
    ##   36.945357  37.272312  42.14548    10
    ##   46.296071  47.023835  81.59570    10
    ##  125.155047 128.730291 148.82684    10
    ##   91.054384  92.313521 104.55609    10
    ##  169.474874 188.926898 202.39035    10
    ##  143.351626 152.023774 168.62783    10
    ##  166.094347 169.348829 258.83993    10
    ##   27.650205  29.358684  42.81951    10
    ##   36.223365  36.962857  43.20510    10
    ##    7.946266   8.285506  17.05297    10

![](QTiming_files/figure-markdown_github/timings-4.png)

    ## [1] 10000
    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##                   rquery in memory  342.6769  347.0060  359.2062  348.8588
    ##              rquery database count  201.5343  203.4338  204.9993  204.0121
    ##               rquery database land  221.4394  222.0239  227.0595  225.4057
    ##                dplyr tbl in memory 1184.8494 1211.7016 1229.0892 1221.7607
    ##  dplyr in memory no grouped filter  810.3147  819.4261  828.9808  824.0485
    ##   dplyr from memory to db and back  588.5373  592.5309  606.8102  601.2274
    ##               dplyr database count  368.9639  369.1326  372.7465  371.3514
    ##                dplyr database land  413.5361  415.9080  424.0785  419.5638
    ##               data.table in memory  232.3168  236.9555  267.3961  241.6737
    ##         base R tabular calculation  489.5771  500.4831  550.9727  570.6838
    ##      base R sequential calculation  105.4358  106.2565  108.1891  107.8456
    ##         uq       max neval
    ##   353.4182  436.2139    10
    ##   206.2752  212.4329    10
    ##   227.1074  242.0219    10
    ##  1244.4000 1300.9269    10
    ##   841.8792  852.5668    10
    ##   612.4579  663.0745    10
    ##   376.2852  381.3455    10
    ##   421.7336  455.1814    10
    ##   324.2687  332.3841    10
    ##   590.1135  597.5852    10
    ##   109.2313  112.4869    10

![](QTiming_files/figure-markdown_github/timings-5.png)

    ## [1] 1e+05
    ## Unit: seconds
    ##                               expr       min        lq      mean    median
    ##                   rquery in memory  4.248772  4.322726  4.705146  4.685321
    ##              rquery database count  2.914666  2.958786  3.201528  3.029489
    ##               rquery database land  3.104646  3.108849  3.529717  3.219996
    ##                dplyr tbl in memory 14.951601 15.017907 16.486908 15.979535
    ##  dplyr in memory no grouped filter 10.136586 10.269898 10.917278 10.778662
    ##   dplyr from memory to db and back  4.794815  4.905432  5.351083  5.145552
    ##               dplyr database count  3.329416  3.387248  3.561656  3.548532
    ##                dplyr database land  3.415441  3.473637  3.621872  3.608901
    ##               data.table in memory  2.619223  2.684118  2.882483  2.716862
    ##         base R tabular calculation  6.773490  6.956928  7.684973  7.146159
    ##      base R sequential calculation  1.477328  1.527098  1.623535  1.608895
    ##         uq       max neval
    ##   5.023437  5.383822    10
    ##   3.278131  4.059372    10
    ##   3.411172  5.721698    10
    ##  17.322392 20.680209    10
    ##  10.983491 13.200597    10
    ##   5.527079  6.820657    10
    ##   3.693479  4.064957    10
    ##   3.678917  3.972836    10
    ##   2.967374  3.601426    10
    ##   8.700373  9.868621    10
    ##   1.630215  2.045693    10

![](QTiming_files/figure-markdown_github/timings-6.png)

    ## [1] 1e+06
    ## Unit: seconds
    ##                               expr       min        lq      mean    median
    ##                   rquery in memory  51.53797  52.54550  56.05219  54.94732
    ##              rquery database count  37.31170  37.90091  38.89447  38.95924
    ##               rquery database land  38.30829  38.95225  43.62137  44.88007
    ##                dplyr tbl in memory 183.30329 183.49821 193.80837 194.73786
    ##  dplyr in memory no grouped filter 122.80937 126.17845 132.87984 131.14359
    ##   dplyr from memory to db and back  60.96287  62.13837  65.20245  62.91296
    ##               dplyr database count  45.48334  46.52205  50.52453  47.59175
    ##                dplyr database land  46.95389  47.97101  49.38060  48.15996
    ##               data.table in memory  27.89087  28.89632  31.25141  30.37458
    ##         base R tabular calculation  87.45792  89.81391  97.07545  93.40767
    ##      base R sequential calculation  21.65428  22.12080  22.85761  22.55359
    ##         uq       max neval
    ##   56.20417  68.87183    10
    ##   39.26412  42.09080    10
    ##   46.39751  48.80525    10
    ##  197.04689 215.59232    10
    ##  137.54028 153.41201    10
    ##   64.95390  82.90235    10
    ##   51.95067  62.71559    10
    ##   51.81494  54.21004    10
    ##   33.14701  38.06063    10
    ##  106.98940 111.26226    10
    ##   23.50381  25.73523    10

![](QTiming_files/figure-markdown_github/timings-7.png)

``` r
saveRDS(timings, "qtimings.RDS")
```

`rquery` appears to be relatively fast. Some of the time for "`rquery` local" is because `rquery` doesn't *really* have a local mode, it has to copy the data to the database and back in that case.

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
