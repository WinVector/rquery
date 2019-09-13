yaml
================

This is an example of `R` reading a `YAML` representation of a `Python`
[`data_algebra`](https://github.com/WinVector/data_algebra) pipeline.
The example being read comes from
[here](https://github.com/WinVector/data_algebra/blob/master/Examples/LogisticExample/ScoringExample.ipynb).

``` r
library(yaml)
library(wrapr)
library(rquery)
library(rqdatatable)
source("R_fns.R")  # https://github.com/WinVector/rquery/blob/master/Examples/yaml/R_fns.R

rep <- yaml.load_file("pipeline_yaml.txt")
ops <- convert_yaml_to_pipeline(rep)
cat(format(ops))
```

    ## table(d; 
    ##   subjectID,
    ##   surveyCategory,
    ##   assessmentTotal,
    ##   irrelevantCol1,
    ##   irrelevantCol2) %.>%
    ##  extend(.,
    ##   probability := exp(assessmentTotal * 0.237)) %.>%
    ##  extend(.,
    ##   total := sum(probability),
    ##   p= subjectID) %.>%
    ##  extend(.,
    ##   probability := probability / total) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   p= subjectID,
    ##   o= "probability" DESC, "surveyCategory") %.>%
    ##  select_rows(.,
    ##    row_number == 1) %.>%
    ##  select_columns(.,
    ##    subjectID, surveyCategory, probability) %.>%
    ##  rename(.,
    ##   c('diagnosis' = 'surveyCategory')) %.>%
    ##  orderby(., subjectID)

``` r
cat(to_sql(ops, rquery_default_db_info()))
```

    ## SELECT * FROM (
    ##  SELECT
    ##   "subjectID" AS "subjectID",
    ##   "surveyCategory" AS "diagnosis",
    ##   "probability" AS "probability"
    ##  FROM (
    ##   SELECT
    ##    "subjectID",
    ##    "surveyCategory",
    ##    "probability"
    ##   FROM (
    ##    SELECT * FROM (
    ##     SELECT
    ##      "subjectID",
    ##      "surveyCategory",
    ##      "probability",
    ##      row_number ( ) OVER (  PARTITION BY "subjectID" ORDER BY "probability" DESC, "surveyCategory" ) AS "row_number"
    ##     FROM (
    ##      SELECT
    ##       "subjectID",
    ##       "surveyCategory",
    ##       "probability" / "total"  AS "probability"
    ##      FROM (
    ##       SELECT
    ##        "subjectID",
    ##        "surveyCategory",
    ##        "probability",
    ##        sum ( "probability" ) OVER (  PARTITION BY "subjectID" ) AS "total"
    ##       FROM (
    ##        SELECT
    ##         "subjectID",
    ##         "surveyCategory",
    ##         exp ( "assessmentTotal" * 0.237 )  AS "probability"
    ##        FROM (
    ##         SELECT
    ##          "subjectID",
    ##          "surveyCategory",
    ##          "assessmentTotal"
    ##         FROM
    ##          "d"
    ##         ) tsql_51659372499255731718_0000000000
    ##        ) tsql_51659372499255731718_0000000001
    ##       ) tsql_51659372499255731718_0000000002
    ##      ) tsql_51659372499255731718_0000000003
    ##    ) tsql_51659372499255731718_0000000004
    ##    WHERE "row_number" = 1
    ##   ) tsql_51659372499255731718_0000000005
    ##  ) tsql_51659372499255731718_0000000006
    ## ) tsql_51659372499255731718_0000000007 ORDER BY "subjectID"

``` r
d_local <- build_frame(
   "subjectID", "surveyCategory"     , "assessmentTotal", "irrelevantCol1", "irrelevantCol2" |
   1L         , "withdrawal behavior", 5                , "irrel1"        , "irrel2"         |
   1L         , "positive re-framing", 2                , "irrel1"        , "irrel2"         |
   2L         , "withdrawal behavior", 3                , "irrel1"        , "irrel2"         |
   2L         , "positive re-framing", 4                , "irrel1"        , "irrel2"         )

d_local %.>% 
  ops %.>% 
  knitr::kable(.)
```

| subjectID | diagnosis           | probability |
| --------: | :------------------ | ----------: |
|         1 | withdrawal behavior |   0.6706221 |
|         2 | positive re-framing |   0.5589742 |

``` r
ops %.>%
  op_diagram(.) %.>% 
  DiagrammeR::grViz(.)
```

![](yaml_files/figure-gfm/diagram-1.png)<!-- -->

See also
<https://github.com/WinVector/data_algebra/tree/master/Examples/LogisticExample>
.
