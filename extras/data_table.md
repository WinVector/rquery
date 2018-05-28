data\_table
================
Win-Vector LLC
12/14/2017

We can work an example similar to the [`rquery`](https://winvector.github.io/rquery/) [example](https://winvector.github.io/rquery/index.html) using a [`data.table`](http://r-datatable.com/) back-end.

``` r
library("rquery")
```

    ## Loading required package: wrapr

``` r
# load data.table second so its definiton of := wins
library("data.table")
```

    ## 
    ## Attaching package: 'data.table'

    ## The following object is masked from 'package:wrapr':
    ## 
    ##     :=

``` r
source("data_table.R") # our example data.table back-end

dL <- data.table(
  data.frame(
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
    stringsAsFactors = FALSE))

scale <- 0.237

# example pipeline
dq <- local_td(dL) %.>%
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
cat(format(dq))
```

    table('dL'; 
      subjectID,
      surveyCategory,
      assessmentTotal,
      irrelevantCol1,
      irrelevantCol2) %.>%
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
.datatable.aware <- TRUE
# Note: data.table has in-place mutate semantics
res <- ex_data_table(dq)

knitr::kable(as.data.frame(res))
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|
