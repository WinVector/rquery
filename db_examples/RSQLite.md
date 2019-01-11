
Let's work a non-trivial example: the `dplyr` pipeline from [Let’s Have Some Sympathy For The Part-time R User](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

For `RSQlite` this is going to be a mess, as we do not have window functions and self-joins can be problematic in `RSQlite`.

``` r
library("rquery")
library("wrapr")

raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
db <- rquery_db_info(
  connection = raw_connection,
  is_dbi = TRUE,
  connection_options = rq_connection_tests(raw_connection))
tmps <- mk_tmp_name_source("ex")


# copy data in so we have an example
d_local <- build_frame(
   "subjectID", "surveyCategory"     , "assessmentTotal", "irrelevantCol1", "irrelevantCol2" |
   1L         , "withdrawal behavior", 5                , "irrel1"        , "irrel2"         |
   1L         , "positive re-framing", 2                , "irrel1"        , "irrel2"         |
   2L         , "withdrawal behavior", 3                , "irrel1"        , "irrel2"         |
   2L         , "positive re-framing", 4                , "irrel1"        , "irrel2"         )
rq_copy_to(db, 'd',
            d_local,
            temporary = TRUE, 
            overwrite = TRUE)
```

    ## [1] "table(`d`; subjectID, surveyCategory, assessmentTotal, irrelevantCol1, irrelevantCol2)"

``` r
# produce a hande to existing table
d <- db_td(db, "d")
```

``` r
scale <- 0.237

dqp <- d %.>%
  extend(.,
         probability :=
           exp(assessmentTotal * scale)) %.>%
  materialize_node(., table_name = tmps())

dqs <- dqp %.>%
  project(., 
       tot_prob := sum(probability),
       groupby = 'subjectID') %.>%
  materialize_node(., table_name = tmps())

dqx <- natural_join(dqp, dqs,
                   by = 'subjectID',
                   jointype = 'LEFT') %.>%
  extend(., 
         probability := probability/tot_prob) %.>% 
  materialize_node(., table_name = tmps()) 

mp <- dqx %.>%
  project(., 
          probability := max(probability),
          groupby = 'subjectID') %.>% 
  materialize_node(., table_name = tmps()) 

dq <- natural_join(mp, dqx,
                    by = c("subjectID", "probability")) %.>%
  project(., 
          probability := max(probability), # pseudo aggregator
          surveyCategory := max(surveyCategory),
          groupby = 'subjectID') %.>%
  rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
  select_columns(., c('subjectID', 
                      'diagnosis', 
                      'probability')) %.>%
  orderby(., cols = 'subjectID')

cat(format(dq))
```

    ## table(`d`; 
    ##   subjectID,
    ##   surveyCategory,
    ##   assessmentTotal,
    ##   irrelevantCol1,
    ##   irrelevantCol2) %.>%
    ##  extend(.,
    ##   probability := exp(assessmentTotal * 0.237)) %.>%
    ##  non_sql_node(., materialize_node(ex_41439864325863495117_0000000000)) %.>%
    ##  natural_join(.,
    ##   table(`d`; 
    ##     subjectID,
    ##     surveyCategory,
    ##     assessmentTotal,
    ##     irrelevantCol1,
    ##     irrelevantCol2) %.>%
    ##    extend(.,
    ##     probability := exp(assessmentTotal * 0.237)) %.>%
    ##    non_sql_node(., materialize_node(ex_41439864325863495117_0000000000)) %.>%
    ##    project(., tot_prob := sum(probability),
    ##     g= subjectID) %.>%
    ##    non_sql_node(., materialize_node(ex_41439864325863495117_0000000001)),
    ##   j= LEFT, by= subjectID) %.>%
    ##  extend(.,
    ##   probability := probability / tot_prob) %.>%
    ##  non_sql_node(., materialize_node(ex_41439864325863495117_0000000002)) %.>%
    ##  project(., probability := max(probability),
    ##   g= subjectID) %.>%
    ##  non_sql_node(., materialize_node(ex_41439864325863495117_0000000003)) %.>%
    ##  natural_join(.,
    ##   table(`d`; 
    ##     subjectID,
    ##     surveyCategory,
    ##     assessmentTotal,
    ##     irrelevantCol1,
    ##     irrelevantCol2) %.>%
    ##    extend(.,
    ##     probability := exp(assessmentTotal * 0.237)) %.>%
    ##    non_sql_node(., materialize_node(ex_41439864325863495117_0000000000)) %.>%
    ##    natural_join(.,
    ##     table(`d`; 
    ##       subjectID,
    ##       surveyCategory,
    ##       assessmentTotal,
    ##       irrelevantCol1,
    ##       irrelevantCol2) %.>%
    ##      extend(.,
    ##       probability := exp(assessmentTotal * 0.237)) %.>%
    ##      non_sql_node(., materialize_node(ex_41439864325863495117_0000000000)) %.>%
    ##      project(., tot_prob := sum(probability),
    ##       g= subjectID) %.>%
    ##      non_sql_node(., materialize_node(ex_41439864325863495117_0000000001)),
    ##     j= LEFT, by= subjectID) %.>%
    ##    extend(.,
    ##     probability := probability / tot_prob) %.>%
    ##    non_sql_node(., materialize_node(ex_41439864325863495117_0000000002)),
    ##   j= INNER, by= subjectID, probability) %.>%
    ##  project(., probability := max(probability), surveyCategory := max(surveyCategory),
    ##   g= subjectID) %.>%
    ##  rename(.,
    ##   c('diagnosis' = 'surveyCategory')) %.>%
    ##  select_columns(.,
    ##    subjectID, diagnosis, probability) %.>%
    ##  orderby(., subjectID)

(Note one can also use the named map builder alias `%:=%` if there is concern of aliasing with `data.table`'s definition of `:=`.)

We then generate our result:

``` r
result <- materialize(db, dq)

class(result)
```

    ## [1] "relop_table_source" "relop"

``` r
result
```

    ## [1] "table(`rquery_mat_09117987609820632164_0000000000`; subjectID, diagnosis, probability)"

``` r
DBI::dbReadTable(db$connection, result$table_name) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|

``` r
dq %.>%
  op_diagram(., merge_tables = TRUE) %.>% 
  DiagrammeR::grViz(.)
```
