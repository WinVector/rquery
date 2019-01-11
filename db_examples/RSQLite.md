
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
score_table <- function(db, d, result_name) {
  scale <- 0.237
  
  tmps <- mk_tmp_name_source("ex")
  
  # convert assessmentTotal to unscaled proabilities
  dqp <- d %.>%
    extend(.,
           probability :=
             exp(assessmentTotal * scale)) %.>%
    materialize(db, ., table_name = tmps(),
                overwrite = TRUE,
                temporary = FALSE)
  
  # total the probabilities per-group
  dqs <- dqp %.>%
    project(., 
            tot_prob := sum(probability),
            groupby = 'subjectID') %.>%
    materialize(db, ., table_name = tmps(),
                overwrite = TRUE,
                temporary = FALSE)
  
  # join total back in and scale
  dqx <- natural_join(dqp, dqs,
                      by = 'subjectID',
                      jointype = 'LEFT') %.>%
    extend(., 
           probability := probability/tot_prob) %.>% 
    materialize(db, ., table_name = tmps(),
                overwrite = TRUE,
                temporary = FALSE)
  
  # find largest per subject probability
  mp <- dqx %.>%
    project(., 
            probability := max(probability),
            groupby = 'subjectID') %.>% 
    materialize(db, ., table_name = tmps(),
                overwrite = TRUE,
                temporary = FALSE)
  
  # join in by best score and probability per subject 
  # (to break ties)
  # and finish the scoring as before
  dq <- natural_join(mp, dqx,
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
  
  result <- materialize(db, dq, table_name = result_name)
  
  # clean up tmps
  intermediates <- tmps(dumpList = TRUE)
  for(ti in intermediates) {
    rquery::rq_remove_table(db, ti)
  }
  
  result
}

result <- score_table(db, d, "result_table")
```

(Note one can also use the named map builder alias `%:=%` if there is concern of aliasing with `data.table`'s definition of `:=`.)

We then look at our result:

``` r
class(result)
```

    ## [1] "relop_table_source" "relop"

``` r
result
```

    ## [1] "table(`result_table`; subjectID, diagnosis, probability)"

``` r
DBI::dbReadTable(db$connection, result$table_name) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|
