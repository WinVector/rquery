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
  probability %:=% exp(assessmentTotal * 0.237)) 

# %.>%
#  extend(.,
#   total %:=% sum(probability),
#   partitionby = c('subjectID'),
#   orderby = c(),
#   reverse = c()) %.>%
#  extend(.,
#   probability %:=% probability / total) %.>%
#  extend(.,
#   sort_key %:=% -(probability)) %.>%
#  extend(.,
#   row_number %:=% row_number(),
#   partitionby = c('subjectID'),
#   orderby = c('sort_key'),
#   reverse = c()) %.>%
#  select_rows(.,
#    row_number == 1) %.>%
#  select_columns(., c(
#    "subjectID", "surveyCategory", "probability")) %.>%
#  rename_columns(.,
#   c('diagnosis' = 'surveyCategory')) %.>%
#   order_rows(., 'subjectID')

ops_obj <- to_transport_representation(ops)

convert_named_vectors_to_lists <- function(obj) {
  if(is.list(obj)) {
    return(lapply(obj, convert_named_vectors_to_lists)) # preserves names
  }
  if(is.vector(obj)) {
    return(as.list(obj))
  }
  return(obj)
}

ops_obj <- convert_named_vectors_to_lists(ops_obj)

print(ops_obj)
```

    ## [[1]]
    ## [[1]]$op
    ## [[1]]$op[[1]]
    ## [1] "TableDescription"
    ## 
    ## 
    ## [[1]]$table_name
    ## [[1]]$table_name[[1]]
    ## [1] "d"
    ## 
    ## 
    ## [[1]]$column_names
    ## [[1]]$column_names[[1]]
    ## [1] "subjectID"
    ## 
    ## [[1]]$column_names[[2]]
    ## [1] "surveyCategory"
    ## 
    ## [[1]]$column_names[[3]]
    ## [1] "assessmentTotal"
    ## 
    ## [[1]]$column_names[[4]]
    ## [1] "irrelevantCol1"
    ## 
    ## [[1]]$column_names[[5]]
    ## [1] "irrelevantCol2"
    ## 
    ## 
    ## 
    ## [[2]]
    ## [[2]]$op
    ## [[2]]$op[[1]]
    ## [1] "Extend"
    ## 
    ## 
    ## [[2]]$ops
    ## [[2]]$ops$probability
    ## [1] "exp ( assessmentTotal * 0.237 )"
    ## 
    ## 
    ## [[2]]$partition_by
    ## NULL
    ## 
    ## [[2]]$order_by
    ## NULL
    ## 
    ## [[2]]$reverse
    ## NULL

``` r
ops_rep <- yaml::as.yaml(ops_obj)
```

``` python
import yaml
import data_algebra.yaml

print(r.ops_obj)
```

    ## [{'op': ['TableDescription'], 'table_name': ['d'], 'column_names': ['subjectID', 'surveyCategory', 'assessmentTotal', 'irrelevantCol1', 'irrelevantCol2']}, {'op': ['Extend'], 'ops': {'probability': 'exp ( assessmentTotal * 0.237 )'}, 'partition_by': None, 'order_by': None, 'reverse': None}]

``` python
print(r.ops_rep)
#ops = data_algebra.yaml.to_pipeline(r.ops_obj)
#ops = data_algebra.yaml.to_pipeline(yaml.safe_load(r.ops_rep))
#print(ops.to_python(pretty=True))
```

    ## - op:
    ##   - TableDescription
    ##   table_name:
    ##   - d
    ##   column_names:
    ##   - subjectID
    ##   - surveyCategory
    ##   - assessmentTotal
    ##   - irrelevantCol1
    ##   - irrelevantCol2
    ## - op:
    ##   - Extend
    ##   ops:
    ##     probability: exp ( assessmentTotal * 0.237 )
    ##   partition_by: ~
    ##   order_by: ~
    ##   reverse: ~
