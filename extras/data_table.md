data.table backend for rquery
================
John Mount, Win-Vector LLC
06/02/2018

We can work an example similar to the [`rquery`](https://winvector.github.io/rquery/) [example](https://winvector.github.io/rquery/index.html) using a [`data.table`](http://r-datatable.com/) back-end ([`rqdatatable`](https://github.com/WinVector/rqdatatable)).

Some details: OSX 10.13.4 on a 2.8 GHz Intel Core i5 Mac Mini (Late 2015 model) with 8GB ram and hybrid disk drive.

``` r
library("microbenchmark")
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
library("dtplyr")
# https://github.com/WinVector/rqdatatable
library("rqdatatable") # devtools::install.packages("WinVector/rqdatatable")

print("R.version.string")
```

    ## [1] "R.version.string"

``` r
packageVersion("dplyr")
```

    ## [1] '0.7.5'

``` r
packageVersion("dtplyr")
```

    ## [1] '0.0.2'

``` r
packageVersion("dbplyr")
```

    ## [1] '1.2.1'

``` r
packageVersion("data.table")
```

    ## [1] '1.11.4'

``` r
packageVersion("rquery")
```

    ## [1] '0.5.0'

``` r
# data example
set.seed(2362)
mk_example <- function(nsubjects, nirrelcols) {
  d <- rbind(data.frame(subjectID = seq_len(nsubjects), 
                        surveyCategory = "withdrawal behavior",
                        stringsAsFactors = FALSE),
             data.frame(subjectID = seq_len(nsubjects), 
                        surveyCategory = "positive re-framing",
                        stringsAsFactors = FALSE))
  d <- d[order(d$subjectID, d$surveyCategory), , drop = FALSE]
  d$assessmentTotal <- rbinom(nrow(d), 10, 0.3)
  for(i in seq_len(nirrelcols)) {
    d[[paste0("irrelevantCol_", i)]] <- runif(nrow(d))
  }
  rownames(d) <- NULL
  d
}

dL <- mk_example(2, 0)
```

``` r
scale <- 0.237

# example rquery pipeline
rquery_pipeline <- local_td(dL) %.>%
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale))  %.>% 
  normalize_cols(.,
                 "probability",
                 partitionby = 'subjectID') %.>%
  pick_top_k(.,
             k = 1,
             partitionby = 'subjectID',
             orderby = c('probability', 'surveyCategory'),
             reverse = c('probability', 'surveyCategory')) %.>% 
  rename_columns(., c('diagnosis' = 'surveyCategory')) %.>%
  select_columns(., c('subjectID', 
                      'diagnosis', 
                      'probability')) %.>%
  orderby(., cols = 'subjectID')
```

Show expanded form of query tree.

``` r
cat(format(rquery_pipeline))
```

    table('dL'; 
      subjectID,
      surveyCategory,
      assessmentTotal) %.>%
     extend(.,
      probability := exp(assessmentTotal * scale)) %.>%
     extend(.,
      probability := probability / sum(probability),
      p= subjectID) %.>%
     extend(.,
      row_rank := rank(),
      p= subjectID,
      o= "probability" DESC, "surveyCategory" DESC) %.>%
     select_rows(.,
       row_rank <= 1) %.>%
     rename(.,
      c('diagnosis' = 'surveyCategory')) %.>%
     select_columns(.,
       subjectID, diagnosis, probability) %.>%
     orderby(., subjectID)

Execute `rquery` pipeline using `data.table` as the implementation.

``` r
ex_data_table(rquery_pipeline) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| positive re-framing |    0.6706221|
|          2| positive re-framing |    0.5589742|

Execute `rquery` pipeline using `PostgreSQL` as the implementation.

``` r
# configure a database connection
my_db <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                          host = 'localhost',
                          port = 5432,
                          user = 'johnmount',
                          password = '')
dbopts <- rquery::rq_connection_tests(my_db)
options(dbopts)
# build the shared handle
winvector_temp_db_handle <- list(db = my_db)

# run the job
execute(dL, rquery_pipeline) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| positive re-framing |    0.6706221|
|          2| positive re-framing |    0.5589742|

`dplyr` pipeline.

``` r
scale <- 0.237

dplyr_pipeline <- . %>% 
  select(subjectID, surveyCategory, assessmentTotal) %>% # narrow to columns of interest
  rename(diagnosis = surveyCategory) %>%
  mutate(probability = exp(assessmentTotal * scale)) %>%
  group_by(subjectID) %>%
  mutate(probability = probability / sum(probability, na.rm = TRUE)) %>%
  arrange(probability, diagnosis) %>%
  mutate(isDiagnosis = row_number() == n()) %>% # try to avoid grouped filtering overhead
  ungroup() %>% 
  filter(isDiagnosis) %>% 
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID) 

dL %>% 
  dplyr_pipeline %>%
  knitr::kable()
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| positive re-framing |    0.6706221|
|          2| positive re-framing |    0.5589742|

Try `dtplyr`.

``` r
data.table::as.data.table(dL) %>% 
  dplyr_pipeline
```

    ## Error in rank(x, ties.method = "first", na.last = "keep"): argument "x" is missing, with no default

Idiomatic `data.table` pipeline.

``` r
# improved code from:
# http://www.win-vector.com/blog/2018/01/base-r-can-be-fast/#comment-66746
data.table_function <- function(dL) {
  # data.table is paying for this copy in its timings (not quite fair)
  # so we will try to minimize it by narrowing columns.
  dDT <- data.table::as.data.table(dL[, c("subjectID", "surveyCategory", "assessmentTotal")])
  data.table::setnames(dDT, old = "surveyCategory", new = "diagnosis")
  dDT[, probability := exp(assessmentTotal * scale)]
  dDT[, probability := probability / sum( probability ), subjectID ]
  data.table::setorder(dDT, subjectID, probability, diagnosis)
  dDT <- dDT[, .SD[.N], subjectID]
  dDT[, assessmentTotal := NULL]
  data.table::setorder(dDT, subjectID)
}

data.table_function(dL) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| positive re-framing |    0.6706221|
|          2| positive re-framing |    0.5589742|

``` r
stats_aggregate_soln <- function(d) {
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
                            FUN = max)
  maxv <- drow$x
  names(maxv) <- drow$subjectID
  d$rmax <- maxv[d$subjectID]
  d <- d[d$rownum >= d$rmax, , drop=FALSE]
  # renormalize
  d$probability <- d$probability/sumv[d$subjectID]
  d <- d[, c("subjectID", "surveyCategory", "probability")]
  colnames(d)[[2]] <- "diagnosis"
  rownames(d) <- NULL
  d
}
```

Timings on a larger example.

``` r
nSubj <- 10000
dL <- mk_example(nSubj, 10)

# and an in-database copy
dR <- rquery::rq_copy_to(my_db, table_name = "dL", dL, 
                         temporary = TRUE, overwrite = TRUE)
dRtbl <- dplyr::tbl(my_db, dR$table_name)

dplyr_round_trip <- function(dL) {
  # https://github.com/tidyverse/dplyr/issues/3026#issuecomment-339035129
  DBI::dbWriteTable(my_db, "dplyr_tmp", 
                    select(dL, subjectID, surveyCategory, assessmentTotal), 
                    overwrite = TRUE, temporary = TRUE)
  as.data.frame(dplyr_pipeline(dplyr::tbl(my_db, "dplyr_tmp")))
}
```

``` r
# show we are working on the new larger data and results agree
dLorig <- dL

ref <- as.data.frame(ex_data_table(rquery_pipeline))
# sensible consequences we can check
assertthat::assert_that(min(ref$probability)>=0.5)
```

    ## [1] TRUE

``` r
assertthat::assert_that(assertthat::are_equal(nSubj, nrow(ref)))
```

    ## [1] TRUE

``` r
assertthat::assert_that(assertthat::are_equal(ref$subjectID, seq_len(nSubj)))
```

    ## [1] TRUE

``` r
assertthat::assert_that(assertthat::are_equal(colnames(ref), c("subjectID", "diagnosis", "probability")))
```

    ## [1] TRUE

``` r
# from database version
c0 <- as.data.frame(execute(my_db, rquery_pipeline))
assertthat::assert_that(assertthat::are_equal(ref, c0))
```

    ## [1] TRUE

``` r
# database round trip version
c1 <- as.data.frame(execute(dL, rquery_pipeline))
assertthat::assert_that(assertthat::are_equal(ref, c1))
```

    ## [1] TRUE

``` r
c2 <- as.data.frame(dplyr_pipeline(dL))
assertthat::assert_that(assertthat::are_equal(ref, c2))
```

    ## [1] TRUE

``` r
c2b <- as.data.frame(dplyr_pipeline(dplyr::as.tbl(dL)))
assertthat::assert_that(assertthat::are_equal(ref, c2b))
```

    ## [1] TRUE

``` r
# from database version
c3 <- as.data.frame(dplyr_pipeline(dRtbl))
assertthat::assert_that(assertthat::are_equal(ref, c3))
```

    ## [1] TRUE

``` r
# database round trip version
# narrow by hand before copying to give all advantages.
c4 <- dplyr_round_trip(dL)
assertthat::assert_that(assertthat::are_equal(ref, c4))
```

    ## [1] TRUE

``` r
c5 <- as.data.frame(data.table_function(dL))
assertthat::assert_that(assertthat::are_equal(ref, c5))
```

    ## [1] TRUE

``` r
c6 <- stats_aggregate_soln(dL)
assertthat::assert_that(assertthat::are_equal(ref, c6))
```

    ## [1] TRUE

``` r
# confirm no side-effects back to orginal frame
assertthat::assert_that(assertthat::are_equal(dLorig, dL))
```

    ## [1] TRUE

``` r
rm(list = c("dL", "dLorig", "dR", "dRtbl"))
```

``` r
all_timings <- NULL

sizes <- expand.grid(c(1, 2, 5), 10^(0:6)) %.>% 
  (.$Var1 * .$Var2) %.>% 
  sort(.)
for(nSubj in sizes) {
  # print("******")
  # print(paste("nSubj", nSubj))
  
  dL <- mk_example(nSubj, 10)
  
  # and a tbl version
  dLt <- dplyr::as.tbl(dL)
  
  # # and an in-database copy
  # dR <- rq_copy_to(my_db, table_name = "dL", dL, 
  #                  temporary = TRUE, overwrite = TRUE)
  # dRtbl <- dplyr::tbl(my_db, dR$table_name)
  
  timings <- microbenchmark(times = 5L,
                            rquery_database_round_trip = nrow(execute(dL, rquery_pipeline)),
                            # rquery_database_read = nrow(as.data.frame(execute(my_db, rquery_pipeline))),
                            rquery_data.table = nrow(ex_data_table(rquery_pipeline)),
                            data.table = nrow(data.table_function(dL)),
                            dplyr_data_frame = nrow(dplyr_pipeline(dL)),
                            dplyr_tbl = nrow(dplyr_pipeline(dLt)),
                            # dplyr_database_read = nrow(as.data.frame(dplyr_pipeline(dRtbl))),
                            dplyr_database_round_trip = nrow(dplyr_round_trip(dL)),
                            base_r_stats_aggregate = nrow(stats_aggregate_soln(dL))
  )
  
  #print(timings)
  timings <- as.data.frame(timings) 
  timings$nrows <- nrow(dL)
  timings$ncols <- ncol(dL)
  all_timings <- rbind(all_timings, as.data.frame(timings))
}

saveRDS(all_timings, "all_timings.RDS")
```

Please see [here](https://github.com/WinVector/rquery/blob/master/extras/data_table_replot.md) for presentation and plots.
