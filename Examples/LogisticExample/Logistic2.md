Logistic2.Rmd
================

``` r
library(wrapr)
library(rquery)
```

``` r
prob_calculation <- locum() %.>%
 extend(.,
  probability := exp(assessmentTotal * 0.237)) %.>%
 extend(.,
  probability := probability / sum(probability),
  partitionby = c('subjectID'))

print(prob_calculation)
```

    ## locum() %.>%
    ##    extend(., `:=`(probability, exp(assessmentTotal * 0.237))) %.>%
    ##    extend(., `:=`(probability, probability/sum(probability)), partitionby = c("subjectID"))

``` r
top_rank <- locum() %.>%
 extend(.,
  row_number := row_number(),
  partitionby = c('subjectID'),
  orderby = c('probability', 'surveyCategory'),
  reverse = c('probability')) %.>%
 select_rows(.,
   row_number <= 1)

print(top_rank)
```

    ## locum() %.>%
    ##    extend(., `:=`(row_number, row_number()), partitionby = c("subjectID"), 
    ##      orderby = c("probability", "surveyCategory"), reverse = c("probability")) %.>%
    ##    select_rows(., row_number <= 1)

``` r
clean_up <- locum() %.>%
 rename_columns(.,
  c('diagnosis' = 'surveyCategory')) %.>%
 select_columns(., c(
   "subjectID", "diagnosis", "probability")) %.>%
 order_rows(.,
  c('subjectID'))

print(clean_up)
```

    ## locum() %.>%
    ##    rename_columns(., c(diagnosis = "surveyCategory")) %.>%
    ##    select_columns(., c("subjectID", "diagnosis", "probability")) %.>%
    ##    order_rows(., c("subjectID"))

``` r
ops <- mk_td("d", c(
  "subjectID",
  "surveyCategory",
  "assessmentTotal",
  "irrelevantCol1",
  "irrelevantCol2")) %.>%
  prob_calculation %.>%
  top_rank %.>%
  clean_up

cat(format(ops))
```

    ## mk_td("d", c(
    ##   "subjectID",
    ##   "surveyCategory",
    ##   "assessmentTotal",
    ##   "irrelevantCol1",
    ##   "irrelevantCol2")) %.>%
    ##  extend(.,
    ##   probability := exp(assessmentTotal * 0.237)) %.>%
    ##  extend(.,
    ##   probability := probability / sum(probability),
    ##   partitionby = c('subjectID'),
    ##   orderby = c(),
    ##   reverse = c()) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   partitionby = c('subjectID'),
    ##   orderby = c('probability', 'surveyCategory'),
    ##   reverse = c('probability')) %.>%
    ##  select_rows(.,
    ##    row_number <= 1) %.>%
    ##  rename_columns(.,
    ##   c('diagnosis' = 'surveyCategory')) %.>%
    ##  select_columns(., c(
    ##    "subjectID", "diagnosis", "probability")) %.>%
    ##  order_rows(.,
    ##   c('subjectID'),
    ##   reverse = c(),
    ##   limit = NULL)

``` r
d_local <- build_frame(
   "subjectID", "surveyCategory"     , "assessmentTotal", "irrelevantCol1", "irrelevantCol2" |
   1L         , "withdrawal behavior", 5                , "irrel1"        , "irrel2"         |
   1L         , "positive re-framing", 2                , "irrel1"        , "irrel2"         |
   2L         , "withdrawal behavior", 3                , "irrel1"        , "irrel2"         |
   2L         , "positive re-framing", 4                , "irrel1"        , "irrel2"         )
   
d_local %.>% ops
```

    ##    subjectID           diagnosis probability
    ## 1:         1 withdrawal behavior   0.6706221
    ## 2:         2 positive re-framing   0.5589742
