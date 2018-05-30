SparkR Example
================

Connect to a `SparkR` cluster and work a small example.

[`rquery`](https://winvector.github.io/rquery/) example.

``` r
library("rquery")
```

    ## Loading required package: wrapr

``` r
print(db_hdl)
```

    ## [1] "rquery_db_info(is_dbi=FALSE, SparkR, <environment: 0x7fe22ed8b5c0>)"

``` r
print(test_df)
```

    ## SparkDataFrame[subjectID:int, surveyCategory:string, assessmentTotal:int, irrelevantCol_0000001:double]

``` r
d_hdl <- sparkr_table(db_hdl, test_df)

print(d_hdl)
```

    ## [1] "table('rs_81899890864282959322_0000000000'; subjectID, surveyCategory, assessmentTotal, irrelevantCol_0000001)"

``` r
print(column_names(d_hdl))
```

    ## [1] "subjectID"             "surveyCategory"        "assessmentTotal"      
    ## [4] "irrelevantCol_0000001"

``` r
scale <- 0.237

rquery_pipeline <- d_hdl %.>%
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

rquery_pipeline %.>%
  op_diagram(.) %.>% 
  DiagrammeR::grViz(.)
```

![](SparkR_files/figure-markdown_github/example-1.png)

``` r
execute(db_hdl, rquery_pipeline) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.5000000|
|          2| withdrawal behavior |    0.6706221|
|          3| withdrawal behavior |    0.5000000|
|          4| positive re-framing |    0.5589742|
|          5| withdrawal behavior |    0.5589742|
|          6| withdrawal behavior |    0.5000000|
|          7| positive re-framing |    0.6706221|
|          8| positive re-framing |    0.5589742|
|          9| positive re-framing |    0.6706221|
|         10| positive re-framing |    0.5589742|
