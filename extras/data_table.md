data.table backend for rquery
================
John Mount, Win-Vector LLC
05/31/2018

We can work an example similar to the [`rquery`](https://winvector.github.io/rquery/) [example](https://winvector.github.io/rquery/index.html) using a [`data.table`](http://r-datatable.com/) back-end ([`rqdatatable`](https://github.com/WinVector/rqdatatable)).

``` r
library("ggplot2")
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
```

    ## Loading required package: rquery

    ## Loading required package: wrapr

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     coalesce

``` r
packageVersion("dplyr")
```

    ## [1] '0.7.5'

``` r
packageVersion("data.table")
```

    ## [1] '1.10.4.3'

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
             partitionby = 'subjectID',
             orderby = c('probability', 'surveyCategory'),
             reverse = c('probability', 'surveyCategory')) %.>% 
  rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
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

``` r
rquery_pipeline %.>%
  op_diagram(.) %.>% 
  DiagrammeR::grViz(.)
```

![](data_table_files/figure-markdown_github/printrqueryp-1.png)

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
dbopts <- rq_connection_tests(my_db)
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
  data.table::setorder(dDT, subjectID, probability, -diagnosis)
  dDT <- dDT[, .SD[.N], subjectID]
  data.table::setorder(dDT, subjectID)
}

data.table_function(dL) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  assessmentTotal|  probability|
|----------:|:--------------------|----------------:|------------:|
|          1| positive re-framing |                6|    0.6706221|
|          2| positive re-framing |                3|    0.5589742|

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
dR <- rq_copy_to(my_db, table_name = "dL", dL, 
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
assertthat::are_equal(nSubj, nrow(ref))
```

    ## [1] TRUE

``` r
assertthat::are_equal(ref$subjectID, seq_len(nSubj))
```

    ## [1] TRUE

``` r
assertthat::are_equal(colnames(ref), c("subjectID", "diagnosis", "probability"))
```

    ## [1] TRUE

``` r
# from database version
c0 <- as.data.frame(execute(my_db, rquery_pipeline))
assertthat::are_equal(ref, c0)
```

    ## [1] TRUE

``` r
# database round trip version
c1 <- as.data.frame(execute(dL, rquery_pipeline))
assertthat::are_equal(ref, c1)
```

    ## [1] TRUE

``` r
c2 <- as.data.frame(dplyr_pipeline(dL))
assertthat::are_equal(ref, c2)
```

    ## [1] TRUE

``` r
# from database version
c3 <- as.data.frame(dplyr_pipeline(dRtbl))
assertthat::are_equal(ref, c3)
```

    ## [1] TRUE

``` r
# database round trip version
# narrow by hand before copying to give all advantages.
c4 <- dplyr_round_trip(dL)
assertthat::are_equal(ref, c4)
```

    ## [1] TRUE

``` r
c5 <- as.data.frame(data.table_function(dL))
assertthat::are_equal(ref, c5)
```

    ## [1] FALSE

``` r
c6 <- stats_aggregate_soln(dL)
assertthat::are_equal(ref, c6)
```

    ## [1] TRUE

``` r
# confirm no side-effects back to orginal frame
assertthat::are_equal(dLorig, dL)
```

    ## [1] TRUE

``` r
all_timings <- NULL

for(nSubj in 10^(0:7)) {
  print("******")
  print(paste("nSubj", nSubj))
  
  dL <- mk_example(nSubj, 10)
  
  # and an in-database copy
  dR <- rq_copy_to(my_db, table_name = "dL", dL, 
                   temporary = TRUE, overwrite = TRUE)
  dRtbl <- dplyr::tbl(my_db, dR$table_name)
  
  timings <- microbenchmark(times = 5L,
                            rquery_database_round_trip = nrow(execute(dL, rquery_pipeline)),
                            # rquery_database_read = nrow(as.data.frame(execute(my_db, rquery_pipeline))),
                            rquery_data.table = nrow(ex_data_table(rquery_pipeline)),
                            data.table = nrow(data.table_function(dL)),
                            dplyr = nrow(dplyr_pipeline(dL)),
                            # dplyr_database_read = nrow(as.data.frame(dplyr_pipeline(dRtbl))),
                            dplyr_database_round_trip = nrow(dplyr_round_trip(dL)),
                            base_r_stats_aggregate = nrow(stats_aggregate_soln(dL))
  )
  
  print(timings)
  timings <- as.data.frame(timings) 
  timings$nrows <- nrow(dL)
  timings$ncols <- ncol(dL)
  all_timings <- rbind(all_timings, as.data.frame(timings))
}
```

    ## [1] "******"
    ## [1] "nSubj 1"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip  69.029450  74.377429  82.379580  80.474265
    ##           rquery_data.table   7.127195   8.760327   9.518508   8.897789
    ##                  data.table   2.051221   2.754013   2.847343   2.853673
    ##                       dplyr   6.272380   6.516135   8.181771   7.757346
    ##   dplyr_database_round_trip 144.512507 144.927179 165.566531 173.417670
    ##      base_r_stats_aggregate   2.615925   2.766526   3.074400   2.816567
    ##          uq        max neval
    ##   89.696811  98.319946     5
    ##    9.661508  13.145722     5
    ##    2.929362   3.648446     5
    ##   10.109809  10.253185     5
    ##  174.707281 190.268019     5
    ##    3.074001   4.098979     5
    ## [1] "******"
    ## [1] "nSubj 10"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip  67.156440  73.060626  82.920443  80.510089
    ##           rquery_data.table   7.570936   8.689809   9.777107   9.030820
    ##                  data.table   2.426472   2.456784   2.618393   2.515110
    ##                       dplyr  10.086663  10.414506  11.512049  10.575411
    ##   dplyr_database_round_trip 135.769876 142.734392 162.646726 159.879842
    ##      base_r_stats_aggregate   3.549091   4.463815   4.678033   4.823753
    ##          uq        max neval
    ##   96.526495  97.348565     5
    ##   10.630271  12.963699     5
    ##    2.765184   2.928414     5
    ##   11.969104  14.514560     5
    ##  186.323910 188.525612     5
    ##    5.153381   5.400124     5
    ## [1] "******"
    ## [1] "nSubj 100"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip  68.280159  73.989883  73.936998  74.659266
    ##           rquery_data.table   8.375911   8.378464  10.549381   8.692715
    ##                  data.table   2.315128   2.681626   2.942087   2.686290
    ##                       dplyr  10.857070  11.261897  13.411790  14.374825
    ##   dplyr_database_round_trip 138.566279 147.701370 174.968423 194.220404
    ##      base_r_stats_aggregate   4.273186   4.864971   5.635442   6.170974
    ##          uq        max neval
    ##   75.820915  76.934769     5
    ##   12.730262  14.569554     5
    ##    2.742238   4.285151     5
    ##   15.228917  15.336241     5
    ##  196.047638 198.306423     5
    ##    6.390031   6.478047     5
    ## [1] "******"
    ## [1] "nSubj 1000"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip  94.986501 100.771336 118.889078 128.822785
    ##           rquery_data.table   9.746145  10.946424  11.927772  11.016319
    ##                  data.table   4.691801   5.407834   5.694072   5.435403
    ##                       dplyr  42.611197  44.262719  50.811715  45.120315
    ##   dplyr_database_round_trip 213.902981 220.829752 222.092339 222.388739
    ##      base_r_stats_aggregate  14.211198  14.245266  19.065685  14.510214
    ##         uq        max neval
    ##  132.60659 137.258174     5
    ##   11.81526  16.114708     5
    ##    5.44712   7.488202     5
    ##   49.51443  72.549909     5
    ##  223.22590 230.114323     5
    ##   25.25964  27.102103     5
    ## [1] "******"
    ## [1] "nSubj 10000"
    ## Unit: milliseconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 317.22013 318.06215 327.66363 322.86430
    ##           rquery_data.table  31.45375  35.43023  57.98260  38.85422
    ##                  data.table  28.94477  29.23703  33.47433  31.36060
    ##                       dplyr 405.79107 454.09806 452.05943 457.46704
    ##   dplyr_database_round_trip 459.03060 460.07174 477.24078 466.56148
    ##      base_r_stats_aggregate 158.70025 160.63810 185.14380 166.09851
    ##         uq       max neval
    ##  325.06523 355.10634     5
    ##   45.92310 138.25168     5
    ##   35.91055  41.91869     5
    ##  457.64969 485.29128     5
    ##  466.67314 533.86696     5
    ##  219.04842 221.23373     5
    ## [1] "******"
    ## [1] "nSubj 1e+05"
    ## Unit: milliseconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 2562.9472 2638.2651 3176.6772 2638.4237
    ##           rquery_data.table  276.6464  323.7509  330.4065  325.2370
    ##                  data.table  290.1218  312.0355  324.5827  317.1223
    ##                       dplyr 4124.8296 4140.0092 5096.0283 4174.3510
    ##   dplyr_database_round_trip 3781.7411 3835.2941 4260.0795 4121.7297
    ##      base_r_stats_aggregate 2223.9412 2257.1987 2824.7896 2260.4321
    ##         uq       max neval
    ##  2885.0333 5158.7170     5
    ##   344.4928  381.9053     5
    ##   339.1800  364.4538     5
    ##  6365.2965 6675.6551     5
    ##  4457.4407 5104.1918     5
    ##  2422.9427 4959.4331     5
    ## [1] "******"
    ## [1] "nSubj 1e+06"
    ## Unit: seconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 25.790816 26.236574 26.656177 27.000283
    ##           rquery_data.table  2.107890  2.192299  2.604533  2.289485
    ##                  data.table  2.115763  2.188365  2.279532  2.190283
    ##                       dplyr 39.675550 40.776398 41.237920 40.779588
    ##   dplyr_database_round_trip 42.433460 43.103537 43.767424 43.196339
    ##      base_r_stats_aggregate 22.234256 22.258174 23.358680 22.677609
    ##         uq       max neval
    ##  27.115695 27.137518     5
    ##   2.833142  3.599851     5
    ##   2.285832  2.617417     5
    ##  41.125615 43.832448     5
    ##  44.879094 45.224692     5
    ##  23.807583 25.815779     5
    ## [1] "******"
    ## [1] "nSubj 1e+07"
    ## Unit: seconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 299.42903 321.26485 323.47056 324.49628
    ##           rquery_data.table  20.52984  23.37411  31.57351  25.49861
    ##                  data.table  24.10960  24.27099  30.34697  26.06031
    ##                       dplyr 408.80455 418.61819 453.77822 470.67962
    ##   dplyr_database_round_trip 467.47587 470.47306 504.37742 512.16376
    ##      base_r_stats_aggregate 364.34942 376.03824 420.56751 405.05481
    ##         uq       max neval
    ##  328.53626 343.62639     5
    ##   35.75514  52.70985     5
    ##   38.11929  39.17465     5
    ##  478.46271 492.32604     5
    ##  531.76685 540.00756     5
    ##  471.07160 486.32345     5

``` r
saveRDS(all_timings, "all_timings.RDS")
```

``` r
all_timings$seconds <- all_timings$time/1e9
timings <- all_timings[all_timings$nrow == max(all_timings$nrow), , drop = FALSE]

cmap <- 
  c(dplyr = "#e6550d",
    dplyr_database_round_trip = "#a63603",
    data.table = "#756bb1",
    rquery_database_round_trip = "#31a354",
    rquery_data.table = "#006d2c")

# summarize by hand using rquery database connector
summary_pipeline <- timings %.>%
  as.data.frame(.) %.>%
  project_nse(., groupby = "expr", mean_time_seconds = avg(seconds)) %.>%
  orderby(., "mean_time_seconds")
means <- timings %.>% 
  as.data.frame(.) %.>%
  summary_pipeline 
knitr::kable(means)
```

| expr                          |  mean\_time\_seconds|
|:------------------------------|--------------------:|
| data.table                    |             30.34697|
| rquery\_data.table            |             31.57351|
| rquery\_database\_round\_trip |            323.47056|
| base\_r\_stats\_aggregate     |            420.56751|
| dplyr                         |            453.77822|
| dplyr\_database\_round\_trip  |            504.37742|

``` r
timings <- as.data.frame(timings)
timings$implementation <- factor(timings$expr, means$expr)
cutpt <- median(timings$seconds[timings$expr=="base_r_stats_aggregate"])
WVPlots::ScatterBoxPlotH(as.data.frame(timings), 
                         "seconds", "implementation", 
                         paste0("task time in seconds by implementation\n(",
                                timings$nrows[[1]], " row by ", timings$ncols[[1]], " column task)")) +
  geom_hline(yintercept = cutpt, linetype=2, alpha = 0.5) 
```

![](data_table_files/figure-markdown_github/presenttimings-1.png)

``` r
all_timings$seconds <- all_timings$time/1e9
all_timings$implementation <- factor(all_timings$expr, rev(means$expr))
ex1 <- all_timings[all_timings$expr != 'base_r_stats_aggregate', , drop = FALSE]
exb <- all_timings[all_timings$expr == 'base_r_stats_aggregate', , drop = FALSE]
sm <- loess(seconds ~ nrows, data= exb)
smf <- data.frame(nrows = exp(seq(log(min(ex1$nrows)), log(max(ex1$nrows)), length.out = 100)))
smf$seconds <- predict(sm, newdata=smf)
ymin = min(all_timings$seconds)
ggplot(mapping = aes(x = nrows, y = seconds, ymax = seconds, ymin = ymin)) +
  geom_ribbon(data = smf, alpha = 0.3) +
  geom_line(data = smf, linetype=2, alpha = 0.5) +
  geom_smooth(data = ex1, se = FALSE, aes(color = implementation)) + 
  scale_x_log10() + scale_y_log10() +
  scale_color_manual(values = cmap) +
  ggtitle("task time in seconds by nrows and implementation",
          subtitle = "shading boundary time taken by base R stats::aggregate() solution")
```

![](data_table_files/figure-markdown_github/presenttimings-2.png)
