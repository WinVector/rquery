Ad Hoc Queries
================
John Mount, Win-Vector LLC
2018-06-05

Database Operator Pipelines
===========================

`rquery`'s primary workflow is building re-usable database operator pipelines.

Let's try an example. First let's set up our example database and data.

``` r
library("rquery")
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
d <- db_td(db, 'd')
print(d)
```

    ## [1] "table('d'; AUC, R2, D, z)"

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

    table('d'; 
      AUC,
      R2,
      D,
      z) %.>%
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
      ) tsql_81185871838786487326_0000000000
      WHERE `R2` > 0.14
      ) tsql_81185871838786487326_0000000001
    ) tsql_81185871838786487326_0000000002

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

Let's first set the global option `rquery.rquery_db_executor` to our database handle so the ad hoc mode knows which database to use to implement the analyses.

``` r
old_o <- options(list("rquery.rquery_db_executor" = list(db = db)))
```

We can now run operators directly on in-memory `data.frame`s.

``` r
dL <- data.frame(AUC = 0.6, 
                 R2 = c(0.1, 0.2), 
                 D = NA, z = 2)

# use data frame to define the pipeline, captures only column structure
ops <- dL %.>%
  select_rows_nse(., R2 > 0.14)

# apply pipeline to any data frame with similar column structure
dL %.>% 
  ops %.>% 
  knitr::kable(.)
```

|  AUC|   R2|    D|    z|
|----:|----:|----:|----:|
|  0.6|  0.2|   NA|    2|

``` r
ops <- dL %.>%
  select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2))  %.>%
  select_columns(., c("AUC", "R2", "c")) 

dL %.>% 
  ops %.>% 
  knitr::kable(.)
```

|  AUC|   R2|          c|
|----:|----:|----------:|
|  0.6|  0.2|  0.4472136|

``` r
# can use pipelines on the fly with
# the %>>% double apply operator.

dL %>>% ( select_rows_nse(., R2 > 0.14) %.>%
  extend_nse(., c = sqrt(R2))  %.>%
  select_columns(., c("AUC", "R2", "c")) )
```

    ##   AUC  R2         c
    ## 1 0.6 0.2 0.4472136

Cleanup
=======

``` r
options(old_o)
DBI::dbDisconnect(db)
```
