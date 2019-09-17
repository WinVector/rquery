iterop
================

Example of moving an [`rquery`](https://winvector.github.io/rquery/)
pipeline from `R` to `Python`
[`data_algebra`](https://github.com/WinVector/data_algebra). Another
example (from the `Python` point of view) can be found
[here](https://github.com/WinVector/data_algebra/blob/master/Examples/LogisticExample/ScoringExample.ipynb).

``` r
library(reticulate)
use_condaenv("aiAcademy") 
```

``` r
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
ops <- mk_td("d", c(
  "subjectID",
  "surveyCategory",
  "assessmentTotal",
  "irrelevantCol1",
  "irrelevantCol2")) %.>%
 extend(.,
  probability %:=% exp(assessmentTotal * 0.237)) %.>%
 extend(.,
  total %:=% sum(probability),
  partitionby = c('subjectID'),
  orderby = c(),
  reverse = c()) %.>%
 extend(.,
  probability %:=% probability / total) %.>%
 extend(.,
  sort_key %:=% -(probability)) %.>%
 extend(.,
  row_number %:=% row_number(),
  partitionby = c('subjectID'),
  orderby = c('sort_key'),
  reverse = c()) %.>%
 select_rows(.,
   row_number == 1) %.>%
 select_columns(., c(
   "subjectID", "surveyCategory", "probability")) %.>%
 rename_columns(.,
  c('diagnosis' = 'surveyCategory')) %.>%
 order_rows(., 'subjectID')

print(ops)
```

    ## [1] "mk_td(\"d\", c( \"subjectID\", \"surveyCategory\", \"assessmentTotal\", \"irrelevantCol1\", \"irrelevantCol2\")) %.>% extend(., probability %:=% exp(assessmentTotal * 0.237)) %.>% extend(., total %:=% sum(probability), partitionby = c('subjectID'), orderby = c(), reverse = c()) %.>% extend(., probability %:=% probability / total) %.>% extend(., sort_key %:=% -((probability))) %.>% extend(., row_number %:=% row_number(), partitionby = c('subjectID'), orderby = c('sort_key'), reverse = c()) %.>% select_rows(., row_number == 1) %.>% select_columns(., c( \"subjectID\", \"surveyCategory\", \"probability\")) %.>% rename_columns(., c('diagnosis' = 'surveyCategory')) %.>% order_rows(., c('subjectID'), reverse = c(), limit = NULL)"

``` r
ops_obj <- to_transport_representation(ops)

convert_named_vectors_to_lists <- function(obj) {
  if(is.list(obj)) {
    return(lapply(obj, convert_named_vectors_to_lists)) # preserves names
  }
  if(is.vector(obj)) {
    if(length(names(obj))<=0) {
      return(obj)
    }
    return(as.list(obj))
  }
  return(obj)
}

ops_obj <- convert_named_vectors_to_lists(ops_obj)
ops_rep <- yaml::as.yaml(ops_obj)
cat(ops_rep)
```

    ## - op: TableDescription
    ##   table_name: d
    ##   column_names:
    ##   - subjectID
    ##   - surveyCategory
    ##   - assessmentTotal
    ##   - irrelevantCol1
    ##   - irrelevantCol2
    ## - op: Extend
    ##   ops:
    ##     probability: exp ( assessmentTotal * 0.237 )
    ##   partition_by: ~
    ##   order_by: ~
    ##   reverse: ~
    ## - op: Extend
    ##   ops:
    ##     total: sum ( probability )
    ##   partition_by: subjectID
    ##   order_by: ~
    ##   reverse: ~
    ## - op: Extend
    ##   ops:
    ##     probability: probability / total
    ##   partition_by: ~
    ##   order_by: ~
    ##   reverse: ~
    ## - op: Extend
    ##   ops:
    ##     sort_key: '- ( ( probability ) )'
    ##   partition_by: ~
    ##   order_by: ~
    ##   reverse: ~
    ## - op: Extend
    ##   ops:
    ##     row_number: row_number ( )
    ##   partition_by: subjectID
    ##   order_by: sort_key
    ##   reverse: ~
    ## - op: SelectRows
    ##   expr:
    ##     '': row_number = 1
    ## - op: SelectColumns
    ##   columns:
    ##   - subjectID
    ##   - surveyCategory
    ##   - probability
    ## - op: Rename
    ##   column_remapping:
    ##     diagnosis: surveyCategory
    ## - op: Order
    ##   column_remapping: ~
    ##   order_columns: subjectID
    ##   reverse: ~

Object transfer.

``` python
import data_algebra.yaml
import data_algebra.expr

ops = data_algebra.yaml.to_pipeline(r.ops_obj, parse_env=data_algebra.expr.r_parse_env())
print(ops.to_python(pretty=True))
```

    ## TableDescription(
    ##     table_name="d",
    ##     column_names=[
    ##         "subjectID",
    ##         "surveyCategory",
    ##         "assessmentTotal",
    ##         "irrelevantCol1",
    ##         "irrelevantCol2",
    ##     ],
    ## ).extend({"probability": "(assessmentTotal * 0.237).exp()"}).extend(
    ##     {"total": "probability.sum()"}, partition_by=["subjectID"]
    ## ).extend(
    ##     {"probability": "probability / total"}
    ## ).extend(
    ##     {"sort_key": "-probability"}
    ## ).extend(
    ##     {"row_number": "_row_number()"}, partition_by=["subjectID"], order_by=["sort_key"]
    ## ).select_rows(
    ##     "row_number == 1"
    ## ).select_columns(
    ##     ["subjectID", "surveyCategory", "probability"]
    ## ).rename_columns(
    ##     {"diagnosis": "surveyCategory"}
    ## ).order_rows(
    ##     ["subjectID"]
    ## )

YAML transfer:

``` python
import yaml
import data_algebra.yaml

r_parse_env = {
  'exp': lambda x: x.exp(),
  'sum': lambda x: x.sum(),
  'row_number': lambda: _row_number()
}
obj = yaml.safe_load(r.ops_rep)
ops = data_algebra.yaml.to_pipeline(obj, parse_env=data_algebra.expr.r_parse_env())
print(ops.to_python(pretty=True))
```

    ## TableDescription(
    ##     table_name="d",
    ##     column_names=[
    ##         "subjectID",
    ##         "surveyCategory",
    ##         "assessmentTotal",
    ##         "irrelevantCol1",
    ##         "irrelevantCol2",
    ##     ],
    ## ).extend({"probability": "(assessmentTotal * 0.237).exp()"}).extend(
    ##     {"total": "probability.sum()"}, partition_by=["subjectID"]
    ## ).extend(
    ##     {"probability": "probability / total"}
    ## ).extend(
    ##     {"sort_key": "-probability"}
    ## ).extend(
    ##     {"row_number": "_row_number()"}, partition_by=["subjectID"], order_by=["sort_key"]
    ## ).select_rows(
    ##     "row_number == 1"
    ## ).select_columns(
    ##     ["subjectID", "surveyCategory", "probability"]
    ## ).rename_columns(
    ##     {"diagnosis": "surveyCategory"}
    ## ).order_rows(
    ##     ["subjectID"]
    ## )
