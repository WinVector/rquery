
<!-- README.md is generated from README.Rmd. Please edit that file -->
`rquery`
========

[`rquery`](https://winvector.github.io/rquery/) is a piped query generator based on [Codd's relational algebra](https://en.wikipedia.org/wiki/Relational_algebra) (updated to reflect lessons learned from working with [`R`](https://www.r-project.org), [`SQL`](https://en.wikipedia.org/wiki/SQL), and [`dplyr`](https://CRAN.R-project.org/package=dplyr) at big data scale in production).

`rquery` is currently recommended for user with `Spark` and `PostgreSQL` (and with non-window functionality with `RSQLite`).

To install: `devtools::install_github("WinVector/rquery")` or `install.packages("rquery")`.

A good place to start is the [`rquery` introductory vignette](https://winvector.github.io/rquery/articles/rquery_intro.html).

![](https://github.com/WinVector/rquery/raw/master/tools/rquery.jpg)

Discussion
==========

[`rquery`](https://github.com/WinVector/rquery) can be an excellent advanced `SQL` training tool (it shows how some very deep `SQL` by composing `rquery` operators). Currently `rquery` is biased towards the `Spark` and `PostgeSQL` `SQL` dialects.

There are many prior relational algebra inspired specialized query languages. Just a few include:

-   [`Alpha`](https://en.wikipedia.org/wiki/Alpha_(programming_language)) ~1971.
-   `ISBL` / Information system based language ~1973
-   [`QUEL`](https://en.wikipedia.org/wiki/QUEL_query_languages) ~1974.
-   [`IBM System R`](https://en.wikipedia.org/wiki/IBM_System_R) ~1974.
-   [`SQL`](https://en.wikipedia.org/wiki/SQL) ~1974.
-   [`Tutorial D`](https://en.wikipedia.org/wiki/D_(data_language_specification)#Tutorial_D) ~1994.
-   [`data.table`](http://r-datatable.com/) ~2006.
-   [`LINQ`](https://msdn.microsoft.com/en-us/library/bb308959.aspx) ~2007.
-   [`pandas`](http://pandas.pydata.org) ~2008.
-   [`dplyr`](http://dplyr.tidyverse.org) ~2014.

`rquery` is realized as a thin translation to an underlying `SQL` provider. We are trying to put the Codd relational operators front and center (using the original naming, and back-porting `SQL` progress such as window functions to the appropriate relational operator).

The primary relational operators include:

-   [`extend()`](https://winvector.github.io/rquery/reference/extend_nse.html). Extend adds derived columns to a relation table. With a sufficiently powerful `SQL` provider this includes ordered and partitioned window functions. This operator also includes built-in [`seplyr`](https://winvector.github.io/seplyr/)-style [assignment partitioning](https://winvector.github.io/seplyr/articles/MutatePartitioner.html). `extend()` can also alter existing columns, though we note this is not always a relational operation (it can lose row uniqueness).
-   [`project()`](https://winvector.github.io/rquery/reference/project_nse.html). Project is usually *portrayed* as the equivalent to column selection, though the original definition includes aggregation. In our opinion the original relational nature of the operator is best captured by moving `SQL`'s "`GROUP BY`" aggregation functionality.
-   [`natural_join()`](https://winvector.github.io/rquery/reference/natural_join.html). This a specialized relational join operator, using all common columns as an equi-join condition.
-   [`theta_join()`](https://winvector.github.io/rquery/reference/theta_join_nse.html). This is the relational join operator allowing an arbitrary matching predicate.
-   [`select_rows()`](https://winvector.github.io/rquery/reference/theta_join_nse.html). This is Codd's relational row selection. Obviously `select` alone is an over-used and now ambiguous term (for example: it is already used as the "doit" verb in `SQL` and the *column* selector in `dplyr`).
-   [`rename_columns()`](https://winvector.github.io/rquery/reference/rename_columns.html). This operator renames sets of columns.
-   [`set_indicator()`](https://winvector.github.io/rquery/reference/set_indicator.html). This operator produces a new column indicating set membership of a named column.

The primary non-relational (traditional `SQL`) operators are:

-   [`select_columns()`](https://winvector.github.io/rquery/reference/select_columns.html). This allows choice of columns (central to `SQL`), but is not a relational operator as it can damage row-uniqueness.
-   [`orderby()`](https://winvector.github.io/rquery/reference/orderby.html). Row order is not a concept in the relational algebra (and also not maintained in most `SQL` implementations). This operator is only useful when used with its `limit=` option, or as the last step as data comes out of the relation store and is moved to `R` (where row-order is usually maintained).
-   [`map_column_values()`](https://winvector.github.io/rquery/reference/map_column_values.html) re-map values in columns (very useful for re-coding data, currently implemented as a [`sql_node()`](https://winvector.github.io/rquery/reference/sql_node.html)).
-   [`unionall()`](https://winvector.github.io/rquery/reference/unionall.html) concatenate tables.

And `rquery` supports higher-order (written in terms of other operators, both package supplied and user supplied):

-   [`pick_top_k()`](https://winvector.github.io/rquery/reference/pick_top_k.html). Pick top `k` rows per group given a row ordering.
-   [`assign_slice()`](https://winvector.github.io/rquery/reference/assign_slice.html). Conditionally assign sets of rows and columns a scalar value.
-   [`if_else_op()`](https://winvector.github.io/rquery/reference/if_else_op.html). Simulate simultaneous if/else assignments.

`rquery` also has implementation helpers for building both `SQL`-nodes (nodes that are just `SQL` expressions) and non-`SQL`-nodes (nodes that are general functions of their input data values).

-   [`sql_node()`](https://winvector.github.io/rquery/reference/sql_node.html)
-   [`sql_expr_set()`](https://winvector.github.io/rquery/reference/sql_expr_set.html)
-   [`non_sql_node()`](https://winvector.github.io/rquery/reference/non_sql_node.html)
-   [`quantile_node()`](https://winvector.github.io/rquery/reference/quantile_node.html)
-   [`rsummary_node()`](https://winvector.github.io/rquery/reference/rsummary_node.html)

The primary missing relational operators are:

-   Union.
-   Direct set difference, anti-join.
-   Division.

One of the principles of `rquery` is to prefer expressive nodes, and not depend on complicated in-node expressions.

A great benefit of Codd's relational algebra is it gives one concepts to decompose complex data transformations into sequences of simpler transformations.

Some reasons `SQL` seems complicated include:

-   `SQL`'s realization of sequencing as nested function composition.
-   `SQL` uses some relational concepts as steps, others as modifiers and predicates.

A lot of the grace of the Codd theory can be recovered through the usual trick changing function composition notation from `g(f(x))` to `x . f() . g()`. This experiment is asking (and not for the first time): "what if `SQL` were piped (expressed composition as a left to right flow, instead of a right to left nesting)?"

Let's work a non-trivial example: the `dplyr` pipeline from [Let’s Have Some Sympathy For The Part-time R User](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

``` r
library("rquery")
library("wrapr")
use_spark <- FALSE

if(use_spark) {
  my_db <- sparklyr::spark_connect(version='2.2.0', 
                                   master = "local")
  cname <- rq_connection_name(my_db)
  rquery::setDBOption(my_db, 
                      "create_options",
                      "USING PARQUET OPTIONS ('compression'='snappy')")
} else {
  driver <- RPostgreSQL::PostgreSQL()
  my_db <- DBI::dbConnect(driver,
                          host = 'localhost',
                          port = 5432,
                          user = 'johnmount',
                          password = '')
}

dbopts <- rq_connection_tests(my_db)
print(dbopts)
```

    ## $rquery.PostgreSQLConnection.use_DBI_dbListFields
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbRemoveTable
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbExecute
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.create_temporary
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.control_temporary
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.control_rownames
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbExistsTable
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.check_logical_column_types
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.fn_name_map
    ##  mean 
    ## "avg"

``` r
options(dbopts)
print(getDBOption(my_db, "control_rownames"))
```

    ## [1] TRUE

``` r
# copy data in so we have an example
d_local <- build_frame(
   "subjectID", "surveyCategory"     , "assessmentTotal", "irrelevantCol1", "irrelevantCol2" |
   1          , "withdrawal behavior", 5                , "irrel1"        , "irrel2"         |
   1          , "positive re-framing", 2                , "irrel1"        , "irrel2"         |
   2          , "withdrawal behavior", 3                , "irrel1"        , "irrel2"         |
   2          , "positive re-framing", 4                , "irrel1"        , "irrel2"         )
rq_copy_to(my_db, 'd',
            d_local,
            temporary = TRUE, 
            overwrite = TRUE)
```

    ## [1] "table(\"d\"; subjectID, surveyCategory, assessmentTotal, irrelevantCol1, irrelevantCol2)"

``` r
# produce a hande to existing table
d <- db_td(my_db, "d")
```

Note: in examples we use `rq_copy_to()` to create data. This is only for the purpose of having easy portable examples. With big data the data is usually already in the remote database or Spark system. The task is almost always to connect and work with this pre-existing remote data and the method to do this is [`db_td()`](https://winvector.github.io/rquery/reference/db_td.html), which builds a reference to a remote table given the table name. The suggested pattern for working with remote tables is to get inputs via [`db_td()`](https://winvector.github.io/rquery/reference/db_td.html) and land remote results with [`materialze()`](https://winvector.github.io/rquery/reference/materialize.html). To work with local data one can copy data from memory to the database with [`rq_copy_to()`](https://winvector.github.io/rquery/reference/rq_copy_to.html) and bring back results with [`execute()`](https://winvector.github.io/rquery/reference/execute.html) (though be aware operation on remote non-memory data is `rquery`'s primary intent).

First we show the Spark/database version of the original example data:

``` r
class(my_db)
```

    ## [1] "PostgreSQLConnection"
    ## attr(,"package")
    ## [1] "RPostgreSQL"

``` r
print(d)
```

    ## [1] "table(\"d\"; subjectID, surveyCategory, assessmentTotal, irrelevantCol1, irrelevantCol2)"

``` r
d %.>%
  execute(my_db, .) %.>%
  knitr::kable(.)
```

|  subjectID| surveyCategory      |  assessmentTotal| irrelevantCol1 | irrelevantCol2 |
|----------:|:--------------------|----------------:|:---------------|:---------------|
|          1| withdrawal behavior |                5| irrel1         | irrel2         |
|          1| positive re-framing |                2| irrel1         | irrel2         |
|          2| withdrawal behavior |                3| irrel1         | irrel2         |
|          2| positive re-framing |                4| irrel1         | irrel2         |

Now we re-write the original calculation in terms of the `rquery` SQL generating operators.

``` r
scale <- 0.237

dq <- d %.>%
  extend_nse(.,
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

(Note one can also use the named map builder alias `%:=%` if there is concern of aliasing with `data.table`'s definition of `:=`.)

We then generate our result:

``` r
dq %.>%
  execute(my_db, .) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|

We see we have quickly reproduced the original result using the new database operators. This means such a calculation could easily be performed at a "big data" scale (using a database or `Spark`; in this case we would not take the results back, but instead use `CREATE TABLE tname AS` to build a remote materialized view of the results).

The actual `SQL` query that produces the result is, in fact, quite involved:

``` r
cat(to_sql(dq, my_db, source_limit = 1000))
```

    SELECT * FROM (
     SELECT
      "subjectID",
      "diagnosis",
      "probability"
     FROM (
      SELECT
       "probability" AS "probability",
       "subjectID" AS "subjectID",
       "surveyCategory" AS "diagnosis"
      FROM (
       SELECT * FROM (
        SELECT
         "probability",
         "subjectID",
         "surveyCategory",
         row_number ( ) OVER (  PARTITION BY "subjectID" ORDER BY "probability" DESC, "surveyCategory" ) AS "row_number"
        FROM (
         SELECT
          "subjectID",
          "surveyCategory",
          "probability" / sum ( "probability" ) OVER (  PARTITION BY "subjectID" ) AS "probability"
         FROM (
          SELECT
           "subjectID",
           "surveyCategory",
           exp ( "assessmentTotal" * 0.237 )  AS "probability"
          FROM (
           SELECT
            "subjectID",
            "surveyCategory",
            "assessmentTotal"
           FROM
            "d" LIMIT 1000
           ) tsql_49468251362997738093_0000000000
          ) tsql_49468251362997738093_0000000001
         ) tsql_49468251362997738093_0000000002
       ) tsql_49468251362997738093_0000000003
       WHERE "row_number" <= 1
      ) tsql_49468251362997738093_0000000004
     ) tsql_49468251362997738093_0000000005
    ) tsql_49468251362997738093_0000000006 ORDER BY "subjectID"

The query is large, but due to its regular structure it should be very amenable to query optimization.

A feature to notice is: the query was automatically restricted to just columns actually needed from the source table to complete the calculation. This has the possibility of decreasing data volume and greatly speeding up query performance. Our [initial experiments](https://github.com/WinVector/rquery/blob/master/extras/PerfTest%2Emd) show `rquery` narrowed queries to be twice as fast as un-narrowed `dplyr` on a synthetic problem simulating large disk-based queries. We think if we connected directly to `Spark`'s relational operators (avoiding the `SQL` layer) we may be able to achieve even faster performance.

The above optimization is possible because the `rquery` representation is an intelligible tree of nodes, so we can interrogate the tree for facts about the query. For example:

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

The additional record-keeping in the operator nodes allows checking and optimization (such as [query narrowing](http://www.win-vector.com/blog/2017/12/how-to-greatly-speed-up-your-spark-queries/)). The flow itself is represented as follows:

``` r
cat(format(dq))
```

    table("d"; 
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
  op_diagram(.) %.>% 
  DiagrammeR::grViz(.)
```

![](https://github.com/WinVector/rquery/raw/master/tools/pipe_diagram.png)

`rquery` also includes a number of useful utilities (both as nodes and as functions).

``` r
quantile_cols(my_db, "d")
```

    ##   quantile_probability subjectID      surveyCategory assessmentTotal
    ## 1                 0.00         1 positive re-framing               2
    ## 2                 0.25         1 positive re-framing               2
    ## 3                 0.50         1 positive re-framing               3
    ## 4                 0.75         2 withdrawal behavior               4
    ## 5                 1.00         2 withdrawal behavior               5
    ##   irrelevantCol1 irrelevantCol2
    ## 1         irrel1         irrel2
    ## 2         irrel1         irrel2
    ## 3         irrel1         irrel2
    ## 4         irrel1         irrel2
    ## 5         irrel1         irrel2

``` r
rsummary(my_db, "d")
```

    ##            column index     class nrows nna nunique min max mean        sd
    ## 1       subjectID     1   numeric     4   0      NA   1   2  1.5 0.5773503
    ## 2  surveyCategory     2 character     4   0       2  NA  NA   NA        NA
    ## 3 assessmentTotal     3   numeric     4   0      NA   2   5  3.5 1.2909944
    ## 4  irrelevantCol1     4 character     4   0       1  NA  NA   NA        NA
    ## 5  irrelevantCol2     5 character     4   0       1  NA  NA   NA        NA
    ##                lexmin              lexmax
    ## 1                <NA>                <NA>
    ## 2 positive re-framing withdrawal behavior
    ## 3                <NA>                <NA>
    ## 4              irrel1              irrel1
    ## 5              irrel2              irrel2

``` r
dq %.>% 
  quantile_node(.) %.>%
  execute(my_db, .)
```

    ##   quantile_probability subjectID           diagnosis probability
    ## 1                 0.00         1 positive re-framing   0.5589742
    ## 2                 0.25         1 positive re-framing   0.5589742
    ## 3                 0.50         1 positive re-framing   0.5589742
    ## 4                 0.75         2 withdrawal behavior   0.6706221
    ## 5                 1.00         2 withdrawal behavior   0.6706221

``` r
dq %.>% 
  rsummary_node(.) %.>%
  execute(my_db, .)
```

    ##        column index     class nrows nna nunique       min       max
    ## 1   subjectID     1   numeric     2   0      NA 1.0000000 2.0000000
    ## 2   diagnosis     2 character     2   0       2        NA        NA
    ## 3 probability     3   numeric     2   0      NA 0.5589742 0.6706221
    ##        mean         sd              lexmin              lexmax
    ## 1 1.5000000 0.70710678                <NA>                <NA>
    ## 2        NA         NA positive re-framing withdrawal behavior
    ## 3 0.6147982 0.07894697                <NA>                <NA>

We have found most big-data projects either require joining very many tables (something `rquery` join planners help with, please see [here](https://github.com/WinVector/rquery/blob/master/extras/JoinController%2Emd) and [here](https://github.com/WinVector/rquery/blob/master/extras/JoinController%2Emd)) or they require working with wide data-marts (where `rquery` query narrowing helps, please see [here](https://github.com/WinVector/rquery/blob/master/extras/PerfTest%2Emd)).

We can also stand `rquery` up on non-`DBI` sources such as [`SparkR`](https://github.com/WinVector/rquery/blob/master/extras/SparkR%2Emd) and also [`data.table`](https://CRAN.R-project.org/package=data.table). The `data.table` adapter is being developed in the [`rqdatatable`](https://github.com/WinVector/rqdatatable) package, and can be [quite fast](http://www.win-vector.com/blog/2018/06/rqdatatable-rquery-powered-by-data-table/). Notice the examples in this mode all essentially use the same query pipeline, the user can choose where to apply it: in memory (`data.table`), in a `DBI` database (`PostgreSQL`, `Sparklyr`), and with even non-DBI systems (`SparkR`).

See also
========

For deeper dives into specific topics, please see also:

-   <a href="https://github.com/WinVector/rquery/blob/master/extras/JoinController%2Emd">Join Controller</a>
-   <a href="https://github.com/WinVector/rquery/blob/master/extras/DependencySorting%2Emd">Join Dependency Sorting</a>
-   <a href="https://github.com/WinVector/rquery/blob/master/extras/AssigmentPartitioner%2Emd">Assignment Partitioner</a>
-   <a href="https://github.com/WinVector/rquery/blob/master/extras/ExtraDBs%2Emd">DifferentDBs</a>
-   <a href="https://github.com/WinVector/rqdatatable">rqdatatable</a>

To install `rquery` please try `install.packages("rquery")` or try `devtools` as follows.

``` r
# install.packages("devtools")
devtools::install_github("WinVector/rquery")
```
