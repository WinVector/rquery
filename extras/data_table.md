data.table backend for rquery
================
John Mount, Win-Vector LLC
05/29/2018

We can work an example similar to the [`rquery`](https://winvector.github.io/rquery/) [example](https://winvector.github.io/rquery/index.html) using a [`data.table`](http://r-datatable.com/) back-end.

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
library("rquery")
```

    ## Loading required package: wrapr

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     coalesce

``` r
# load data.table second so its definiton of := wins
library("data.table")
```

    ## 
    ## Attaching package: 'data.table'

    ## The following object is masked from 'package:wrapr':
    ## 
    ##     :=

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
library("dtplyr")
source("data_table.R") # our example rquery data.table back-end
```

``` r
# data example
dL <- build_frame(
   "subjectID", "surveyCategory"     , "assessmentTotal" |
   1          , "withdrawal behavior", 5                 |
   1          , "positive re-framing", 2                 |
   2          , "withdrawal behavior", 3                 |
   2          , "positive re-framing", 4                 )
```

``` r
scale <- 0.237

test_p <- local_td(dL) %.>%
  extend_nse(.,
             one := 1) %.>%
  project_nse(.,
             maxscore = max(assessmentTotal),
             groupby = 'subjectID')
ex_data_table(test_p) %.>%
  knitr::kable(.)
```

|  subjectID|  maxscore|
|----------:|---------:|
|          1|         5|
|          2|         4|

``` r
# example rquery pipeline
rquery_pileline <- local_td(dL) %.>%
  extend_nse(.,
             one := 1) %.>%
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale)/
               sum(exp(assessmentTotal * scale)),
             count := sum(one),
             rank := rank(probability, surveyCategory),
             partitionby = 'subjectID') %.>%
  extend_nse(.,
             isdiagnosis := rank == count,
             diagnosis := surveyCategory) %.>%
  select_rows_nse(., 
                  isdiagnosis == TRUE) %.>%
  select_columns(., 
                 c('subjectID', 'diagnosis', 'probability')) %.>%
  orderby(., 'subjectID')
```

Show expanded form of query tree.

``` r
cat(format(rquery_pileline))
```

    table('dL'; 
      subjectID,
      surveyCategory,
      assessmentTotal) %.>%
     extend(.,
      one := 1) %.>%
     extend(.,
      probability := exp(assessmentTotal * scale)/sum(exp(assessmentTotal * scale)),
      count := sum(one),
      p= subjectID) %.>%
     extend(.,
      rank := rank(probability, surveyCategory),
      p= subjectID) %.>%
     extend(.,
      isdiagnosis := rank == count,
      diagnosis := surveyCategory) %.>%
     select_rows(.,
       isdiagnosis == TRUE) %.>%
     select_columns(.,
       subjectID, diagnosis, probability) %.>%
     orderby(., subjectID)

``` r
# execute
# https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package
#.datatable.aware <- TRUE

ex_data_table(rquery_pileline) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|

`dplyr` pipeline.

``` r
scale <- 0.237

dplyr_pipeline <- . %>% group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
  arrange(probability, surveyCategory) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID) 

dL %>% 
  dplyr_pipeline %>%
  knitr::kable()
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|

Try `dtplyr`.

``` r
data.table::as.data.table(dL) %>% 
  dplyr_pipeline
```

    ## Error in rank(x, ties.method = "first", na.last = "keep"): argument "x" is missing, with no default

Timings.

``` r
# fatten up data.frame a bit
dL <- dL[rep(seq_len(nrow(dL)), 10000), , drop = FALSE]
dL$subjectID <- paste(dL$subjectID, (1+seq_len(nrow(dL))) %/% 2, sep = "_")
for(i in seq_len(10)) {
  dL[[paste0("irrelevantCol", i)]] <- runif(nrow(dL))
}

# show we are working on the new larger data
system.time(print(nrow(ex_data_table(rquery_pileline))))
```

    ## [1] 20000

    ##    user  system elapsed 
    ##   0.635   0.026   0.416

``` r
system.time(print(nrow(dplyr_pipeline(dL))))
```

    ## [1] 20000

    ##    user  system elapsed 
    ##   1.398   0.027   1.455

``` r
timings <- microbenchmark(
  nrow(ex_data_table(rquery_pileline)),
  nrow(dplyr_pipeline(dL)))
```

``` r
print(timings)
```

    ## Unit: milliseconds
    ##                                  expr       min        lq      mean
    ##  nrow(ex_data_table(rquery_pileline))  273.6537  295.5555  344.2774
    ##              nrow(dplyr_pipeline(dL)) 1230.6227 1275.6322 1403.5798
    ##     median        uq       max neval
    ##   323.5548  370.5334  564.7431   100
    ##  1325.5366 1413.5868 2355.5739   100

``` r
# summarize by hand using rquery database connector
summary_pipeline <- timings %.>%
  as.data.frame(.) %.>%
  project_nse(., groupby = "expr", mean = avg(time)) 
timings %.>% 
  as.data.frame(.) %.>%
  summary_pipeline %.>%
  knitr::kable(.)
```

| expr                                    |        mean|
|:----------------------------------------|-----------:|
| nrow(dplyr\_pipeline(dL))               |  1403579784|
| nrow(ex\_data\_table(rquery\_pileline)) |   344277450|

``` r
autoplot(timings)
```

![](data_table_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
WVPlots::ScatterBoxPlotH(as.data.frame(timings), "time", "expr", "runtime by expression in nanoseconds")
```

![](data_table_files/figure-markdown_github/unnamed-chunk-10-2.png)
