SparkR Example
================
John Mount, Win-Vector LLC
06/02/2018

Connect to a `SparkR` cluster and work a small example.

To install a practice version of `Spark`/`SparkR` v2.3.0 on a stand-alone workstation:

-   First download Spark 2.3.0 Pre-built for Apache Hadoop 2.7 or later ([spark-2.3.0-bin-hadoop2.7.tgz](https://www.apache.org/dyn/closer.lua/spark/spark-2.3.0/spark-2.3.0-bin-hadoop2.7.tgz)) from [Apache Spark Downloads](https://spark.apache.org/downloads.html).
-   Uncompress this into a directory named `spark-2.3.0-bin-hadoop2.7`.
-   Install `SparkR` from `spark-2.3.0-bin-hadoop2.7/spark-2.3.0-bin-hadoop2.7/R/lib/SparkR`: `install.packages("~/Downloads/spark-2.3.0-bin-hadoop2.7/R/lib/SparkR/", repos = NULL, type = "source")`.
-   Use `SparkR` package to install its own local `Spark`: `SparkR::install.spark()` (based on [sparkr-vignettes.Rmd](https://github.com/apache/spark/blob/master/R/pkg/vignettes/sparkr-vignettes.Rmd)).

TODO: switch to number of times ordering from different restaurants. Looking for the favorite and fraction of orders from orders from that restaunt (or quisine). Slow down and define problem and show data, before any work.

[`rquery`](https://winvector.github.io/rquery/) example.

``` r
library("rquery")

print(db_hdl)
```

    ## [1] "rquery_db_info(is_dbi=FALSE, SparkR, <environment: 0x7fc95c5fec90>)"

``` r
print(test_df)
```

    ## SparkDataFrame[subjectID:int, surveyCategory:string, assessmentTotal:int, irrelevantCol_0000001:double]

``` r
table_description <- sparkr_table(test_df, "survey_table")

print(table_description)
```

    ## [1] "table('survey_table'; subjectID, surveyCategory, assessmentTotal, irrelevantCol_0000001)"

``` r
print(column_names(table_description))
```

    ## [1] "subjectID"             "surveyCategory"        "assessmentTotal"      
    ## [4] "irrelevantCol_0000001"

``` r
rquery_pipeline <- table_description %.>%
  extend_nse(.,
             assessment_fraction = assessmentTotal)  %.>% 
  normalize_cols(.,
                 "assessment_fraction",
                 partitionby = 'subjectID') %.>%
  pick_top_k(.,
             k = 1,
             partitionby = 'subjectID',
             orderby = c('assessment_fraction', 'surveyCategory'),
             reverse = c('assessment_fraction', 'surveyCategory')) %.>% 
  rename_columns(., c('favorite_category' = 'surveyCategory')) %.>%
  select_columns(., c('subjectID', 
                      'favorite_category', 
                      'assessment_fraction')) %.>%
  orderby(., cols = 'subjectID')
```

``` r
cat(format(rquery_pipeline))
```

    ## table('survey_table'; 
    ##   subjectID,
    ##   surveyCategory,
    ##   assessmentTotal,
    ##   irrelevantCol_0000001) %.>%
    ##  extend(.,
    ##   assessment_fraction := assessmentTotal) %.>%
    ##  extend(.,
    ##   assessment_fraction := assessment_fraction / sum(assessment_fraction),
    ##   p= subjectID) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   p= subjectID,
    ##   o= "assessment_fraction" DESC, "surveyCategory" DESC) %.>%
    ##  select_rows(.,
    ##    row_number <= 1) %.>%
    ##  rename(.,
    ##   c('favorite_category' = 'surveyCategory')) %.>%
    ##  select_columns(.,
    ##    subjectID, favorite_category, assessment_fraction) %.>%
    ##  orderby(., subjectID)

``` r
rquery_pipeline %.>%
  op_diagram(.) %.>% 
  DiagrammeR::DiagrammeR(diagram = ., type = "grViz") %.>% 
  DiagrammeRsvg::export_svg(.) %.>% 
  charToRaw(.) %.>%
  rsvg::rsvg_png(., file = "Sparkr_files/diagram1.png")
```

![](Sparkr_files/diagram1.png)

``` r
columns_used(rquery_pipeline)
```

    ## $survey_table
    ## [1] "subjectID"       "surveyCategory"  "assessmentTotal"

``` r
execute(db_hdl, rquery_pipeline) %.>%
  knitr::kable(.)
```

|  subjectID| favorite\_category  |  assessment\_fraction|
|----------:|:--------------------|---------------------:|
|          1| withdrawal behavior |             0.5000000|
|          2| withdrawal behavior |             0.6000000|
|          3| withdrawal behavior |             0.5000000|
|          4| positive re-framing |             0.5333333|
|          5| withdrawal behavior |             0.6000000|
|          6| withdrawal behavior |             0.5000000|
|          7| positive re-framing |             0.6666667|
|          8| positive re-framing |             0.6666667|
|          9| positive re-framing |             0.6363636|
|         10| positive re-framing |             0.5333333|
