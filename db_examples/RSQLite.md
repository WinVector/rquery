
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

stages <- list()
add_stage <- function(ops, table_name = tmps()) {
  ops$materialize_as <- table_name
  table <- mk_td(table_name, column_names(ops))
  stages <<- c(stages, list(ops))
  table
}


# convert assessmentTotal to unscaled proabilities
dqp_ops <- d %.>%
  extend(.,
         probability :=
           exp(assessmentTotal * scale)) 
dqp_table <- add_stage(dqp_ops)

# total the probabilities per-group
dqs_ops <- dqp_table %.>%
  project(., 
          tot_prob := sum(probability),
          groupby = 'subjectID') 
dqs_table <- add_stage(dqs_ops)

# join total back in and scale
dqx_ops <- natural_join(dqp_table, dqs_table,
                    by = 'subjectID',
                    jointype = 'LEFT') %.>%
  extend(., 
         probability := probability/tot_prob) 
dqx_table <- add_stage(dqx_ops)

# find largest per subject probability
mp_ops <- dqx_table %.>%
  project(., 
          probability := max(probability),
          groupby = 'subjectID') 
mp_table <- add_stage(mp_ops)

# join in by best score and probability per subject 
# (to break ties)
# and finish the scoring as before
dq <- natural_join(mp_table, dqx_table,
                   by = c("subjectID", "probability")) %.>%
  project(., 
          probability := max(probability), # pseudo aggregator
          surveyCategory := min(surveyCategory),
          groupby = 'subjectID') %.>%
  rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
  select_columns(., c('subjectID', 
                      'diagnosis', 
                      'probability')) %.>%
  orderby(., cols = 'subjectID')
result = add_stage(dq, "result_table")
```

We then build our result.

``` r
for(stage in stages) {
  materialize(db, stage, table_name = stage$materialize_as,
              temporary = TRUE, overwrite = TRUE)
}
```

And take a look.

``` r
class(result)
```

    ## [1] "relop_table_source" "relop"

``` r
result
```

    ## [1] "table(result_table; subjectID, diagnosis, probability)"

``` r
DBI::dbReadTable(db$connection, result$table_name) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|

``` r
# clean up tmps
intermediates <- tmps(dumpList = TRUE)
for(ti in intermediates) {
  rquery::rq_remove_table(db, ti)
}

DBI::dbDisconnect(raw_connection)
rm(list = c("raw_connection", "db"))
```
