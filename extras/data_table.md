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

for(nSubj in 10^(0:6)) {
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
    ##  rquery_database_round_trip  70.282058  72.469500  79.849114  73.142262
    ##           rquery_data.table   7.452056   8.087751   9.660426  10.261709
    ##                  data.table   1.999957   2.353060   2.545551   2.353551
    ##                       dplyr   6.700161   8.141722   8.693564   8.889595
    ##   dplyr_database_round_trip 140.600498 161.671067 170.876028 171.440398
    ##      base_r_stats_aggregate   2.553525   3.271092   3.574711   3.616765
    ##          uq        max neval
    ##   83.364466  99.987285     5
    ##   10.262934  12.237682     5
    ##    2.440486   3.580699     5
    ##    9.139235  10.597105     5
    ##  183.722540 196.945636     5
    ##    4.215769   4.216404     5
    ## [1] "******"
    ## [1] "nSubj 10"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip  66.115183  74.571028  80.535474  79.747578
    ##           rquery_data.table   9.144851   9.341991  10.203280   9.449925
    ##                  data.table   2.261884   2.351525   2.863551   2.436394
    ##                       dplyr   9.450341   9.460369  11.272210   9.545603
    ##   dplyr_database_round_trip 132.966102 135.237660 161.882694 165.525793
    ##      base_r_stats_aggregate   3.655355   3.986386   4.502577   4.467952
    ##          uq        max neval
    ##   89.861591  92.381991     5
    ##    9.966853  13.112781     5
    ##    3.625221   3.642733     5
    ##   12.761395  15.143342     5
    ##  182.719002 192.964912     5
    ##    4.736836   5.666355     5
    ## [1] "******"
    ## [1] "nSubj 100"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip  67.422402  71.076670  74.024345  72.136057
    ##           rquery_data.table   8.531425   8.600629   9.089157   8.905274
    ##                  data.table   2.694509   2.716044   2.990792   3.013510
    ##                       dplyr  10.084117  11.330607  13.642775  14.681082
    ##   dplyr_database_round_trip 138.421512 145.312808 173.418781 191.359042
    ##      base_r_stats_aggregate   4.840476   4.884449   5.797249   6.029146
    ##          uq        max neval
    ##   78.919612  80.566986     5
    ##    9.476156   9.932299     5
    ##    3.121853   3.408043     5
    ##   15.377460  16.740607     5
    ##  193.879518 198.121025     5
    ##    6.437601   6.794574     5
    ## [1] "******"
    ## [1] "nSubj 1000"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip  91.690265  93.945856 118.259833 134.303865
    ##           rquery_data.table  11.777830  12.029881  13.109822  12.760565
    ##                  data.table   4.849465   5.339167   5.947016   5.408452
    ##                       dplyr  43.118846  44.264693  49.278931  44.471337
    ##   dplyr_database_round_trip 214.534733 219.498560 220.674008 220.553923
    ##      base_r_stats_aggregate  14.231214  15.319441  19.527569  15.491673
    ##          uq       max neval
    ##  134.323624 137.03555     5
    ##   13.303849  15.67699     5
    ##    5.926275   8.21172     5
    ##   45.273989  69.26579     5
    ##  223.546086 225.23674     5
    ##   24.579619  28.01590     5
    ## [1] "******"
    ## [1] "nSubj 10000"
    ## Unit: milliseconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 319.04613 320.72138 324.57940 322.59672
    ##           rquery_data.table  33.27938  33.46148  57.96221  38.14410
    ##                  data.table  29.17582  29.95408  33.32392  32.28594
    ##                       dplyr 401.28211 449.43631 443.81507 452.92194
    ##   dplyr_database_round_trip 459.30635 459.82237 487.78332 469.49951
    ##      base_r_stats_aggregate 149.90993 151.29998 180.77225 151.55082
    ##         uq       max neval
    ##  329.11238 331.42035     5
    ##   46.16486 138.76122     5
    ##   33.59021  41.61355     5
    ##  455.24025 460.19472     5
    ##  517.24159 533.04676     5
    ##  224.43907 226.66145     5
    ## [1] "******"
    ## [1] "nSubj 1e+05"
    ## Unit: milliseconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 2536.1233 2571.7063 2591.0856 2591.3387
    ##           rquery_data.table  272.6761  276.3677  285.3021  278.6558
    ##                  data.table  289.5129  295.1952  309.0163  296.4627
    ##                       dplyr 3990.7356 4125.8017 4114.5314 4127.9229
    ##   dplyr_database_round_trip 3770.7715 3791.6872 3812.8853 3804.1543
    ##      base_r_stats_aggregate 2345.7062 2386.9994 2429.6260 2414.5994
    ##         uq       max neval
    ##  2626.8632 2629.3966     5
    ##   298.3020  300.5089     5
    ##   307.1364  356.7741     5
    ##  4149.7745 4178.4220     5
    ##  3815.3279 3882.4856     5
    ##  2469.3592 2531.4657     5
    ## [1] "******"
    ## [1] "nSubj 1e+06"
    ## Unit: seconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 25.946029 26.298117 26.406245 26.320275
    ##           rquery_data.table  2.150720  2.189799  2.387604  2.340513
    ##                  data.table  2.107955  2.172632  2.238410  2.216478
    ##                       dplyr 39.269384 39.366011 39.872973 39.369928
    ##   dplyr_database_round_trip 39.640864 39.705289 40.291738 39.797655
    ##      base_r_stats_aggregate 22.358983 22.946313 23.603838 23.761234
    ##         uq       max neval
    ##  26.720343 26.746460     5
    ##   2.378751  2.878236     5
    ##   2.279337  2.415650     5
    ##  39.582833 41.776712     5
    ##  40.916120 41.398762     5
    ##  24.001487 24.951174     5

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
| data.table                    |             2.238410|
| rquery\_data.table            |             2.387604|
| base\_r\_stats\_aggregate     |            23.603838|
| rquery\_database\_round\_trip |            26.406245|
| dplyr                         |            39.872974|
| dplyr\_database\_round\_trip  |            40.291738|

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
