Ad Hoc Queries
================
John Mount, Win-Vector LLC
2018-05-15

Database Operator Pipelines
===========================

`rquery`'s primary workflow is building re-usable database operator pipelines.

Let's try an example. First let's set up our example database and data.

``` r
library("rquery")
```

    ## Loading required package: wrapr

``` r
db = DBI::dbConnect(RSQLite::SQLite(), 
                    ":memory:")
RSQLite::initExtension(db)

DBI::dbWriteTable(db,
                  'd',
                  data.frame(AUC = 0.6, 
                             R2 = c(0.1, 0.2), 
                             D = NA, z = 2),
                  overwrite = TRUE,
                  temporary = TRUE)
d <- rq_table(db, 'd')
print(d)
```

    ## [1] "table('d')"

``` r
DBI::dbGetQuery(db, to_sql(d, db))
```

    ##   AUC  R2  D z
    ## 1 0.6 0.1 NA 2
    ## 2 0.6 0.2 NA 2

Now we can define a query over this table.

``` r
q <- d %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2)) %.>%
  select_columns(., c("AUC", "R2", "c"))
```

The idea is:

-   The variable `d` is a table model (name of the table and a set of assumed column names) that allows us to reason about an actual database table to specified later.
-   The query `q` is a sequence of operators we can hold, examine, and alter.

We can print the query/operator pipeline:

``` r
cat(format(q))
```

    table('d') %.>%
     select_rows(.,
       R2 > 0.14) %.>%
     extend(.,
      c := sqrt(R2)) %.>%
     select_columns(.,
       AUC, R2, c)

And we can ask questions of it:

``` r
column_names(q)
```

    ## [1] "AUC" "R2"  "c"

``` r
tables_used(q)
```

    ## [1] "d"

``` r
columns_used(q)
```

    ## $d
    ## [1] "AUC" "R2"

And we can convert the operator pipeline to `SQL` which can then be applied to an actual database table.

``` r
sql <- to_sql(q, db)
cat(sql)
```

    SELECT
     `AUC`,
     `R2`,
     `c`
    FROM (
     SELECT
      `AUC`,
      `R2`,
      sqrt ( `R2` )  AS `c`
     FROM (
      SELECT * FROM (
       SELECT
        `d`.`AUC`,
        `d`.`R2`
       FROM
        `d`
      ) tsql_24202580347421524376_0000000000
      WHERE `R2` > 0.14
      ) tsql_24202580347421524376_0000000001
    ) tsql_24202580347421524376_0000000002

``` r
DBI::dbGetQuery(db, sql) %.>%
  knitr::kable(.)
```

|  AUC|   R2|          c|
|----:|----:|----------:|
|  0.6|  0.2|  0.4472136|

Ad Hoc mode
===========

`rquery` also has an "Ad Hoc" mode for interactive analysis.
In this mode things are sped up in that the use can work with in-memory tables and also skip the table modeling step.

Let's first set the global variable `winvector_temp_db_handle` to our database handle so the ad hoc mode knows which database to use to implement the analyses.

``` r
winvector_temp_db_handle <- list(db = db)
```

We can now run operators directly on in-memory `data.frame`s.

``` r
dL <-  data.frame(AUC = 0.6, 
                  R2 = c(0.1, 0.2), 
                  D = NA, z = 2)

dL %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  knitr::kable(.)
```

|  AUC|   R2|    D|    z|
|----:|----:|----:|----:|
|  0.6|  0.2|   NA|    2|

``` r
dL %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2))  %.>%
  select_columns(., c("AUC", "R2", "c")) %.>%
  knitr::kable(.)
```

|  AUC|   R2|          c|
|----:|----:|----------:|
|  0.6|  0.2|  0.4472136|

Using a function wrapper we can also save ad hoc pipelines for later use.

``` r
q2 <- . := {
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2)) %.>%
  select_columns(., c("AUC", "R2", "c"))
}

dL %.>% 
  q2 %.>%
  knitr::kable(.)
```

|  AUC|   R2|          c|
|----:|----:|----------:|
|  0.6|  0.2|  0.4472136|

Or we can use a table model based pipeline directly (without needing additional wrapping). To do this we need to define an S3 function as follows.

``` r
print(rquery:::wrapr_function.relop)
```

    ## function(pipe_left_arg,
    ##                                  pipe_right_arg,
    ##                                  pipe_environment,
    ##                                  pipe_name = NULL) {
    ##   if(!("relop" %in% class(pipe_right_arg))) {
    ##     stop("rquery::wrapr_function.relop expect pipe_right_arg to be of class relop")
    ##   }
    ##   if(is.data.frame(pipe_left_arg)) {
    ##     return(rquery_apply_to_data_frame(pipe_left_arg,
    ##                                       pipe_right_arg,
    ##                                       env = pipe_environment))
    ##   }
    ##   # assume pipe_left_arg is a DB connection, execute and bring back result
    ##   execute(pipe_left_arg, pipe_right_arg)
    ## }
    ## <bytecode: 0x7fc2b1d1d718>
    ## <environment: namespace:rquery>

``` r
needed_columns <- columns_used(q)
print(needed_columns)
```

    ## $d
    ## [1] "AUC" "R2"

``` r
q3 <- table_source(table_name = 'tmp', 
                   columns = needed_columns$d) %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2)) %.>%
  select_columns(., c("AUC", "R2", "c"))

dL %.>% 
  q3 %.>%
  knitr::kable(.)
```

|  AUC|   R2|          c|
|----:|----:|----------:|
|  0.6|  0.2|  0.4472136|

For stored queries we either need the table model (which places a bound on what columns are thought to exist in the table) or a function wrapper (which allows us to use the later to be named table as our future table bound).

We can also use the original pipeline `q`, but only after removing the original backing table (for safety the ad hoc system will not overwrite existing tables).

``` r
DBI::dbExecute(db, "DROP TABLE d")
```

    ## [1] 0

``` r
dL %.>% 
  q %.>%
  knitr::kable(.)
```

|  AUC|   R2|          c|
|----:|----:|----------:|
|  0.6|  0.2|  0.4472136|

Ad Hoc Mode Works
=================

Ad Hoc mode is implemented on top of `wrapr::%.>%` using a few fine points of `R` programming:

-   `S3` deceleration of query nodes, allowing different effects when the data source is another query node or a `data.frame`.
-   `S3` overrides of `as.data.frame()`, `print()` and `head()` to trigger execution of pipelines.
-   [`wrapr`](https://winvector.github.io/wrapr/) pipeline controls to allow `S3` dispatch of pipeline stages.

Basic ad hoc mode
-----------------

The basic version of ad hoc mode is implemented by overriding the `S3` classes `as.data.frame()`, `print()` and `head()` for our `rquery::"relop"` operator trees / pipelines.

Consider our earlier ad hoc pipeline:

``` r
z <- dL %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2))  %.>%
  select_columns(., c("AUC", "R2", "c"))

class(z)
```

    ## [1] "relop_select_columns" "relop"

Notice `z` declares class `"relop"`. This means `z` is a `rquery` operator tree. Formatting it shows that it is starts with "`table+()`" node, meaning the operator tree has a reference to an in-memory `data.frame` bound into it.

``` r
cat(format(z))
```

    table+('rquery_tmp_12279762678601372044_0000000000') %.>%
     select_rows(.,
       R2 > 0.14) %.>%
     extend(.,
      c := sqrt(R2)) %.>%
     select_columns(.,
       AUC, R2, c)

``` r
cat(to_sql(z, db))
```

    SELECT
     `AUC`,
     `R2`,
     `c`
    FROM (
     SELECT
      `AUC`,
      `R2`,
      sqrt ( `R2` )  AS `c`
     FROM (
      SELECT * FROM (
       SELECT
        `rquery_tmp_12279762678601372044_0000000000`.`AUC`,
        `rquery_tmp_12279762678601372044_0000000000`.`R2`
       FROM
        `rquery_tmp_12279762678601372044_0000000000`
      ) tsql_41311662173789195946_0000000000
      WHERE `R2` > 0.14
      ) tsql_41311662173789195946_0000000001
    ) tsql_41311662173789195946_0000000002

The production of `SQL` and execution is triggered if we pass `z` to one of the generic `S3` functions `as.data.frame()` or `print()` (including the possible implicit `print()` implied by `R`'s statement rules):

``` r
print(z)
```

    ##   AUC  R2         c
    ## 1 0.6 0.2 0.4472136

``` r
as.data.frame(z)
```

    ##   AUC  R2         c
    ## 1 0.6 0.2 0.4472136

`knitr::kable()` itself calls `as.data.frame()` at some point, allowing `z` results to formatted by passing to `knitr::kable()`:

``` r
knitr::kable(z)
```

|  AUC|   R2|          c|
|----:|----:|----------:|
|  0.6|  0.2|  0.4472136|

Stored ad hoc pipeline
----------------------

To re-use regular operator trees as ad hoc pipelines we need one more trick: the operator tree object needs to act as if it were a function. As of version `1.2.0` `wrapr` de-references right-hand side names as functions or as surrogate functions through the `S3` method `wrapr_function()` (dispatched by the class of its second or right hand side argument). This gives us the ability to treat an `rquery` operator tree as a data processing pipeline. Results are then produced by overriding the `S3` methods `as.data.frame()` and `print()`.

This is a bit simpler if demonstrated.

``` r
class(q)
```

    ## [1] "relop_select_columns" "relop"

``` r
cat(format(q))
```

    table('d') %.>%
     select_rows(.,
       R2 > 0.14) %.>%
     extend(.,
      c := sqrt(R2)) %.>%
     select_columns(.,
       AUC, R2, c)

``` r
dL %.>% 
  q %.>%
  knitr::kable(.)
```

|  AUC|   R2|          c|
|----:|----:|----------:|
|  0.6|  0.2|  0.4472136|

For an `R` language name `q`, the `wrapr` pipeline operator (`%.>%`) will interpret "`dL %.>% q`" as `q(dL)` (if `q` is a function), or as "`wrapr_function(dL,q)`" (dispatched by the class of `q`). This facility allows objects to declare what sort of function they would like to be treated a in a pipeline.

Cleanup
=======

The ad hoc method defaults to using a transient `RSQLite` database connection.
Our a non-transient `DBI` database connection can be specified by adding one as the "`db`" value in a list bound to the global variable "`winvector_temp_db_handle`" (as we did in this note). If one has done this one can use a more powerful database (such as `PostgeSQL` which has window functions). In this case one should also probably close the `DB` connection or at least break the reference when finished as follows.

``` r
winvector_temp_db_handle <- NULL
DBI::dbDisconnect(db)
```
