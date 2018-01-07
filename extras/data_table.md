data\_table
================
Win-Vector LLC
12/14/2017

We can work an example similar to the [`rquery`](https://winvector.github.io/rquery/) [example](https://winvector.github.io/rquery/index.html) using a [`data.table`](http://r-datatable.com/) back-end.

``` r
library("rquery")
```

    ## Loading required package: wrapr

    ## Loading required package: cdata

``` r
suppressPackageStartupMessages(library("data.table"))
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

d <- data_table_source(dL)

# example pipeline
dq <- d %.>%
  extend_nse(.,
             one := 1) %.>%
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale)/
               sum(exp(assessmentTotal * scale)),
             count := sum(one),
             rank := rank(probability),
             partitionby = 'subjectID') %.>%
  extend_nse(.,
             isdiagnosis := rank == count,
             diagnosis := surveyCategory)
```

``` r
cat(format(dq))
```

    table('dL') %.>%
     extend(.,
       := one := 1) %.>%
     extend(.,
       := probability := exp(assessmentTotal * scale) / sum(exp(assessmentTotal * scale)),
       := count := sum(one),
      p= subjectID) %.>%
     extend(.,
       := rank := rank(probability),
      p= subjectID) %.>%
     extend(.,
       := isdiagnosis := rank = count,
       := diagnosis := surveyCategory)

``` r
# translation to data.table
expr <- to_data_table(dq)
cat(gsub("][", " ][\n  ", 
         expr, 
         fixed = TRUE))
```

    dL[, one := 1 ][
      , probability := exp ( `assessmentTotal` * 0.237 ) / sum ( exp ( `assessmentTotal` * 0.237 ) ) ,subjectID ][
      , count := sum ( `one` ) ,subjectID ][
      , rank := rank ( `probability` ) ,subjectID ][
      , isdiagnosis := `rank` == `count` ][
      , diagnosis := `surveyCategory`]

``` r
# execute
# https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package
.datatable.aware <- TRUE
# Note: data.table has in-place mutate semantics
res <- as.data.frame(eval(parse(text = expr)))

# finish in base-R 
# (we only implemented a couple of operators for this demonstration)
res <- res[res$isdiagnosis, 
           c('subjectID', 'diagnosis', 'probability'),
           drop = FALSE]
row.names(res) <- NULL
res <- res[order(res$subjectID), , drop = FALSE]

knitr::kable(res)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|

Notice how "`][`" looks a lot like it is already a pipe operator for `data.table`.
