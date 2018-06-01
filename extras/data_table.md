data.table backend for rquery
================
John Mount, Win-Vector LLC
06/01/2018

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

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-a953ff6e9c99ea0b2309">{"x":{"diagram":"\ndigraph rquery_optree {\n  graph [ layout = dot, rankdir = TB, overlap = prism, compound = true, nodesep = .5, ranksep = .25]\n  edge [decorate = true, arrowhead = normal]\n  node [style=filled, fillcolor=lightgrey]\n\nnode_1 [ shape = \"folder\" , label = \"table(dL; \\l  subjectID,\\l  surveyCategory,\\l  assessmentTotal)\\l\"]\n\nnode_2 [ shape = \"tab\" , label = \"extend(.,\\l  probability := exp(assessmentTotal * scale))\\l\"]\n\nnode_3 [ shape = \"tab\" , label = \"extend(.,\\l  probability := probability / sum(probability),\\l  p= subjectID)\\l\"]\n\nnode_4 [ shape = \"tab\" , label = \"extend(.,\\l  row_rank := rank(),\\l  p= subjectID,\\l  o= probability DESC, surveyCategory DESC)\\l\"]\n\nnode_5 [ shape = \"tab\" , label = \"select_rows(.,\\l   row_rank <= 1)\\l\"]\n\nnode_6 [ shape = \"tab\" , label = \"rename(.,\\l  c(diagnosis = surveyCategory))\\l\"]\n\nnode_7 [ shape = \"tab\" , label = \"select_columns(.,\\l   subjectID, diagnosis, probability)\\l\"]\n\nnode_8 [ shape = \"tab\" , label = \"orderby(., subjectID)\\l\"]\nnode_1 -> node_2 [ label = \".\"]\nnode_2 -> node_3 [ label = \".\"]\nnode_3 -> node_4 [ label = \".\"]\nnode_4 -> node_5 [ label = \".\"]\nnode_5 -> node_6 [ label = \".\"]\nnode_6 -> node_7 [ label = \".\"]\nnode_7 -> node_8 [ label = \".\"]\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
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
    ##  rquery_database_round_trip  71.138681  83.393231  83.147045  84.833661
    ##           rquery_data.table   9.608766   9.971618  10.323950  10.195958
    ##                  data.table   2.075675   2.180758   2.456860   2.497990
    ##                       dplyr   7.612825  11.489418  12.256675  13.087308
    ##   dplyr_database_round_trip 147.132946 201.682862 209.952480 217.776187
    ##      base_r_stats_aggregate   3.085184   3.785181   4.658687   3.806105
    ##          uq        max neval
    ##   84.865362  91.504292     5
    ##   10.318854  11.524552     5
    ##    2.764750   2.765127     5
    ##   13.905702  15.188124     5
    ##  227.597322 255.573083     5
    ##    4.969376   7.647591     5
    ## [1] "******"
    ## [1] "nSubj 10"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip  67.845813  69.336879  81.562058  87.188163
    ##           rquery_data.table   9.497021   9.934465  15.616941  10.279672
    ##                  data.table   2.090796   2.190110   2.308872   2.361299
    ##                       dplyr   9.452232  11.476213  30.016245  11.974590
    ##   dplyr_database_round_trip 162.622927 186.228870 202.306599 203.067094
    ##      base_r_stats_aggregate   4.200695   4.735195  11.830658   8.206472
    ##         uq        max neval
    ##   88.31182  95.127611     5
    ##   12.34138  36.032167     5
    ##    2.40091   2.501245     5
    ##   12.88391 104.294283     5
    ##  207.49438 252.119725     5
    ##   13.50275  28.508181     5
    ## [1] "******"
    ## [1] "nSubj 100"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip  66.815639  73.894296  79.361467  76.052675
    ##           rquery_data.table   8.229293   8.950303  14.725861   9.802629
    ##                  data.table   2.786475   3.068548   4.051763   3.070573
    ##                       dplyr  10.333904  10.484546  16.072997  14.972173
    ##   dplyr_database_round_trip 148.623101 159.382159 182.954371 200.123005
    ##      base_r_stats_aggregate   5.082886   5.883054   6.326022   6.077374
    ##          uq        max neval
    ##   78.736378 101.308346     5
    ##   10.685754  35.961328     5
    ##    3.503149   7.830071     5
    ##   20.218128  24.356233     5
    ##  201.934320 204.709268     5
    ##    6.257049   8.329749     5
    ## [1] "******"
    ## [1] "nSubj 1000"
    ## Unit: milliseconds
    ##                        expr        min         lq       mean     median
    ##  rquery_database_round_trip 127.568661 141.437378 159.243990 143.464920
    ##           rquery_data.table  12.676699  13.250249  14.418486  13.856080
    ##                  data.table   5.668883   6.140717   8.336774   7.738183
    ##                       dplyr  56.862057  59.126697  60.837393  59.275752
    ##   dplyr_database_round_trip 233.333187 251.438677 262.828636 260.351315
    ##      base_r_stats_aggregate  14.741730  15.019510  21.130873  17.589373
    ##          uq       max neval
    ##  168.810764 214.93823     5
    ##   14.798054  17.51135     5
    ##    7.781801  14.35429     5
    ##   61.420543  67.50192     5
    ##  264.086126 304.93388     5
    ##   26.282366  32.02139     5
    ## [1] "******"
    ## [1] "nSubj 10000"
    ## Unit: milliseconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 323.06561 324.99739 364.25126 348.38187
    ##           rquery_data.table  44.87562  47.42099  48.44307  48.20416
    ##                  data.table  34.68167  35.58400  43.90367  41.19149
    ##                       dplyr 422.10237 429.92637 461.00641 463.65321
    ##   dplyr_database_round_trip 464.34444 547.46560 597.25658 629.65887
    ##      base_r_stats_aggregate 137.86605 152.89420 194.53529 161.97059
    ##         uq       max neval
    ##  395.85655 428.95486     5
    ##   49.75908  51.95547     5
    ##   46.67471  61.38648     5
    ##  490.44204 498.90806     5
    ##  631.87214 712.94184     5
    ##  247.05048 272.89515     5
    ## [1] "******"
    ## [1] "nSubj 1e+05"
    ## Unit: milliseconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 2534.0234 2542.1845 2607.5515 2605.2087
    ##           rquery_data.table  330.9433  336.4919  350.7774  348.4246
    ##                  data.table  329.7817  333.4134  335.8603  333.5674
    ##                       dplyr 3812.4155 3876.5725 3987.9899 3906.9410
    ##   dplyr_database_round_trip 3749.2049 3819.9913 4006.8039 4142.7523
    ##      base_r_stats_aggregate 2263.6486 2300.8380 2430.0946 2365.4907
    ##         uq       max neval
    ##  2661.6754 2694.6654     5
    ##   352.9374  385.0898     5
    ##   334.2273  348.3116     5
    ##  3997.6523 4346.3683     5
    ##  4151.4921 4170.5791     5
    ##  2601.1329 2619.3627     5
    ## [1] "******"
    ## [1] "nSubj 1e+06"
    ## Unit: seconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 25.897362 26.143649 27.142575 26.677837
    ##           rquery_data.table  2.438871  2.500997  2.706722  2.503838
    ##                  data.table  2.483940  2.610533  2.727376  2.655502
    ##                       dplyr 37.874685 38.211236 41.371857 42.072642
    ##   dplyr_database_round_trip 38.570147 40.874372 41.607898 42.832009
    ##      base_r_stats_aggregate 22.033998 23.257504 23.966391 23.637800
    ##         uq       max neval
    ##  28.312361 28.681667     5
    ##   2.920099  3.169806     5
    ##   2.760413  3.126494     5
    ##  42.809214 45.891508     5
    ##  42.835209 42.927754     5
    ##  23.866012 27.036639     5
    ## [1] "******"
    ## [1] "nSubj 1e+07"
    ## Unit: seconds
    ##                        expr       min        lq      mean    median
    ##  rquery_database_round_trip 276.57327 284.39720 295.60391 289.33218
    ##           rquery_data.table  24.89220  26.62269  30.93817  28.65483
    ##                  data.table  26.70312  30.98182  34.86265  33.73271
    ##                       dplyr 381.63089 419.96944 435.00829 428.00963
    ##   dplyr_database_round_trip 440.34283 453.07472 469.02831 459.56673
    ##      base_r_stats_aggregate 348.90100 351.54848 387.53749 394.07234
    ##         uq       max neval
    ##  302.13699 325.57989     5
    ##   34.82853  39.69262     5
    ##   37.24384  45.65175     5
    ##  447.89854 497.53295     5
    ##  477.89150 514.26578     5
    ##  412.54710 430.61854     5

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
| rquery\_data.table            |             30.93817|
| data.table                    |             34.86265|
| rquery\_database\_round\_trip |            295.60391|
| base\_r\_stats\_aggregate     |            387.53749|
| dplyr                         |            435.00829|
| dplyr\_database\_round\_trip  |            469.02831|

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
