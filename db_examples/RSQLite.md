
Let’s work a non-trivial example: the `dplyr` pipeline from [Let’s Have
Some Sympathy For The Part-time R
User](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

For `RSQlite` this is going to be a mess, as we do not have window
functions and self-joins can be problematic in `RSQlite`.

``` r
library("rquery")
library("wrapr")

raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
RSQLite::initExtension(raw_connection)
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

dq <- d %.>%
  extend(.,
         probability :=
           exp(assessmentTotal * scale))  %.>% 
  normalize_cols(.,
                 "probability",
                 partitionby = 'subjectID') %.>%
  pick_top_k(.,
             partitionby = 'subjectID',
             orderby = c('probability', 'surveyCategory'),
             reverse = c('probability')) %.>% 
  rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
  select_columns(., c('subjectID', 
                      'diagnosis', 
                      'probability')) %.>%
  orderby(., cols = 'subjectID')
```

(Note one can also use the named map builder alias `%:=%` if there is
concern of aliasing with `data.table`’s definition of `:=`.)

We then generate our result:

``` r
result <- materialize(db, dq)

class(result)
```

    ## [1] "relop_table_source" "relop"

``` r
result
```

    ## [1] "table(`rquery_mat_40827193001928517693_0000000000`; subjectID, diagnosis, probability)"

``` r
DBI::dbReadTable(db$connection, result$table_name) %.>%
  knitr::kable(.)
```

| subjectID | diagnosis           | probability |
| --------: | :------------------ | ----------: |
|         1 | withdrawal behavior |   0.6706221 |
|         2 | positive re-framing |   0.5589742 |

We see we have quickly reproduced the original result using the new
database operators. This means such a calculation could easily be
performed at a “big data” scale (using a database or `Spark`; in this
case we would not take the results back, but instead use `CREATE TABLE
tname AS` to build a remote materialized view of the results).

A bonus is, thanks to `data.table` and the `rqdatatable` packages we can
run the exact same operator pipeline on local data.

``` r
library("rqdatatable")

d_local %.>% 
  dq %.>%
  knitr::kable(.)
```

| subjectID | diagnosis           | probability |
| --------: | :------------------ | ----------: |
|         1 | withdrawal behavior |   0.6706221 |
|         2 | positive re-framing |   0.5589742 |

Notice we applied the pipeline by piping data into it. This ability is a
feature of the [dot arrow
pipe](https://journal.r-project.org/archive/2018/RJ-2018-042/index.html)
we are using here.

The actual `SQL` query that produces the database result is, in fact,
quite involved:

``` r
cat(to_sql(dq, db, source_limit = 1000))
```

    SELECT * FROM (
     SELECT
      `subjectID`,
      `diagnosis`,
      `probability`
     FROM (
      SELECT
       `subjectID` AS `subjectID`,
       `surveyCategory` AS `diagnosis`,
       `probability` AS `probability`
      FROM (
       SELECT * FROM (
        SELECT
         `subjectID`,
         `surveyCategory`,
         `probability`,
         row_number ( ) OVER (  PARTITION BY `subjectID` ORDER BY `probability` DESC, `surveyCategory` ) AS `row_number`
        FROM (
         SELECT
          `subjectID`,
          `surveyCategory`,
          `probability` / sum ( `probability` ) OVER (  PARTITION BY `subjectID` ) AS `probability`
         FROM (
          SELECT
           `subjectID`,
           `surveyCategory`,
           exp ( `assessmentTotal` * 0.237 )  AS `probability`
          FROM (
           SELECT
            `subjectID`,
            `surveyCategory`,
            `assessmentTotal`
           FROM
            `d` LIMIT 1000
           ) tsql_60675901566361948073_0000000000
          ) tsql_60675901566361948073_0000000001
         ) tsql_60675901566361948073_0000000002
       ) tsql_60675901566361948073_0000000003
       WHERE `row_number` <= 1
      ) tsql_60675901566361948073_0000000004
     ) tsql_60675901566361948073_0000000005
    ) tsql_60675901566361948073_0000000006 ORDER BY `subjectID`

The query is large, but due to its regular structure it should be very
amenable to query optimization.

A feature to notice is: the query was automatically restricted to just
columns actually needed from the source table to complete the
calculation. This has the possibility of decreasing data volume and
greatly speeding up query performance. Our [initial
experiments](https://github.com/WinVector/rquery/blob/master/extras/PerfTest%2Emd)
show `rquery` narrowed queries to be twice as fast as un-narrowed
`dplyr` on a synthetic problem simulating large disk-based queries. We
think if we connected directly to `Spark`’s relational operators
(avoiding the `SQL` layer) we may be able to achieve even faster
performance.

The above optimization is possible because the `rquery` representation
is an intelligible tree of nodes, so we can interrogate the tree for
facts about the query. For example:

``` r
column_names(dq)
```

    ## [1] "subjectID"   "diagnosis"   "probability"

``` r
tables_used(dq)
```

    ## [1] "d"

``` r
columns_used(dq)
```

    ## $d
    ## [1] "subjectID"       "surveyCategory"  "assessmentTotal"

The additional record-keeping in the operator nodes allows checking and
optimization (such as [query
narrowing](http://www.win-vector.com/blog/2017/12/how-to-greatly-speed-up-your-spark-queries/)).
The flow itself is represented as follows:

``` r
cat(format(dq))
```

    table(`d`; 
      subjectID,
      surveyCategory,
      assessmentTotal,
      irrelevantCol1,
      irrelevantCol2) %.>%
     extend(.,
      probability := exp(assessmentTotal * 0.237)) %.>%
     extend(.,
      probability := probability / sum(probability),
      p= subjectID) %.>%
     extend(.,
      row_number := row_number(),
      p= subjectID,
      o= "probability" DESC, "surveyCategory") %.>%
     select_rows(.,
       row_number <= 1) %.>%
     rename(.,
      c('diagnosis' = 'surveyCategory')) %.>%
     select_columns(.,
       subjectID, diagnosis, probability) %.>%
     orderby(., subjectID)

``` r
dq %.>%
  op_diagram(., merge_tables = TRUE) %.>% 
  DiagrammeR::grViz(.) %.>%
  DiagrammeRsvg::export_svg(.) %.>%
  write(., file="RSQLite_diagram.svg")
```

![](RSQLite_diagram.svg)
