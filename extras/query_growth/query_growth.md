query growth
================

[`R`](https://www.r-project.org) users have been enjoying the benefits of [`SQL`](https://en.wikipedia.org/wiki/SQL) query generators for quite some time, most notably using the [`dbplyr`](https://CRAN.R-project.org/package=dbplyr) package. I would like to talk about some features of our own [`rquery`](https://github.com/WinVector/rquery) query generator, concentrating on derived result re-use.

Introduction
------------

`SQL` represents value use by nesting. To use a query result within another query one writes the query to be used inside the query using the values. `R` query generators can also represent value use by nesting, but they also tend to expose a sequential pipe notation where value use is represented by sequencing.

We will demonstrate this with an example.

``` r
library("rquery")

raw_connection <- DBI::dbConnect(RSQLite::SQLite(), 
                                 ":memory:")

db_rquery <- rquery_db_info(
  connection = raw_connection,
  is_dbi = TRUE,
  connection_options = rq_connection_tests(raw_connection))

d <- data.frame(x = -3:3)

d0 <- rq_copy_to(db_rquery, "d", d)
print(d0)
```

    ## [1] "table(`d`; x)"

``` r
ops <- d0 %.>%
  extend(., y = x^2 -1) %.>%
  select_rows(., x > y)

cat(format(ops))
```

    ## table(`d`; 
    ##   x) %.>%
    ##  extend(.,
    ##   y := x ^ 2 - 1) %.>%
    ##  select_rows(.,
    ##    x > y)

Notice how in the `rquery` operation we write operations one after another (using the [dot-arrow pipe notation](https://journal.r-project.org/archive/2018/RJ-2018-042/index.html)), and the operations are similarly printed.

Whereas the generated `SQL` is written in a nested format.

``` r
cat(to_sql(ops, db_rquery))
```

    ## SELECT * FROM (
    ##  SELECT
    ##   `x`,
    ##   POWER ( `x` , 2 ) - 1  AS `y`
    ##  FROM (
    ##   SELECT
    ##    `x`
    ##   FROM
    ##    `d`
    ##   ) tsql_09475918393068567149_0000000000
    ## ) tsql_09475918393068567149_0000000001
    ## WHERE `x` > `y`

Notice the database handle is kept separate from the operators. Furthermore execution is not entangled with operator definition, but is a separate step (performed through `materialze()`, `execute()` or even sending the query to the database via a pipe).

``` r
ops %.>%
  db_rquery %.>%
  knitr::kable(.)
```

|    x|    y|
|----:|----:|
|    0|   -1|
|    1|    0|

Re-using derived values
-----------------------

The piped operator notation runs into trouble if we re-use derived values. `SQL` can re-use tables and views, but can not conveniently re-use queries. So if we use query results more than once the notation expands our [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) specification into a possibly much larger tree.

The following is an artificial example to demonstrate the issue.

``` r
d1 <- natural_join(d0, d0, 
                   by = "x", jointype = "LEFT")
d2 <- natural_join(d1, d1, 
                   by = "x", jointype = "LEFT")
d3 <- natural_join(d2, d2, 
                   by = "x", jointype = "LEFT")

cat(format(d3))
```

    ## table(`d`; 
    ##   x) %.>%
    ##  natural_join(.,
    ##   table(`d`; 
    ##     x),
    ##   j= LEFT, by= x) %.>%
    ##  natural_join(.,
    ##   table(`d`; 
    ##     x) %.>%
    ##    natural_join(.,
    ##     table(`d`; 
    ##       x),
    ##     j= LEFT, by= x),
    ##   j= LEFT, by= x) %.>%
    ##  natural_join(.,
    ##   table(`d`; 
    ##     x) %.>%
    ##    natural_join(.,
    ##     table(`d`; 
    ##       x),
    ##     j= LEFT, by= x) %.>%
    ##    natural_join(.,
    ##     table(`d`; 
    ##       x) %.>%
    ##      natural_join(.,
    ##       table(`d`; 
    ##         x),
    ##       j= LEFT, by= x),
    ##     j= LEFT, by= x),
    ##   j= LEFT, by= x)

Notice the depth 3 expression exploded into tree with 7 joins.

`rquery`'s query diagrammer can help spot and diagnose these issues.

``` r
d3 %.>%
  op_diagram(., merge_tables = TRUE) %.>% 
  DiagrammeR::grViz(.) %.>%
  DiagrammeRsvg::export_svg(.) %.>%
  write(., file="query_growth_diagram.svg")
```

    ## Warning in op_diagram(., merge_tables = TRUE): possible repeated calculation:
    ##  natural_join(.1, .2,  j= LEFT, by= x)

![](query_growth_diagram.svg)

The gold nodes are possibly repeated calculations, and the warning also notes the issue.

The above example may seem unnatural- but there are a number of places where it is natural to re-use intermediate results. One such circumstance is comparing values in groups without the use of window functions (as demonstrated [here](https://github.com/WinVector/rquery/blob/master/db_examples/RSQLite.md)).

The query explosion issue is not unique to [`rquery`](https://CRAN.R-project.org/package=rquery), [`dplyr`](https://CRAN.R-project.org/package=dplyr) has the same issue.

``` r
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
d0_dplyr <- tbl(raw_connection, "d")

d1_dplyr <- left_join(d0_dplyr, d0_dplyr, 
                      by = "x")
d2_dplyr <- left_join(d1_dplyr, d1_dplyr, 
                      by = "x")
d3_dplyr <- left_join(d2_dplyr, d2_dplyr, 
                      by = "x")
dbplyr::remote_query(d3_dplyr)
```

    ## <SQL> SELECT `TBL_LEFT`.`x` AS `x`
    ##   FROM (SELECT `TBL_LEFT`.`x` AS `x`
    ##   FROM (SELECT `TBL_LEFT`.`x` AS `x`
    ##   FROM `d` AS `TBL_LEFT`
    ##   LEFT JOIN `d` AS `TBL_RIGHT`
    ##   ON (`TBL_LEFT`.`x` = `TBL_RIGHT`.`x`)
    ## ) AS `TBL_LEFT`
    ##   LEFT JOIN (SELECT `TBL_LEFT`.`x` AS `x`
    ##   FROM `d` AS `TBL_LEFT`
    ##   LEFT JOIN `d` AS `TBL_RIGHT`
    ##   ON (`TBL_LEFT`.`x` = `TBL_RIGHT`.`x`)
    ## ) AS `TBL_RIGHT`
    ##   ON (`TBL_LEFT`.`x` = `TBL_RIGHT`.`x`)
    ## ) AS `TBL_LEFT`
    ##   LEFT JOIN (SELECT `TBL_LEFT`.`x` AS `x`
    ##   FROM (SELECT `TBL_LEFT`.`x` AS `x`
    ##   FROM `d` AS `TBL_LEFT`
    ##   LEFT JOIN `d` AS `TBL_RIGHT`
    ##   ON (`TBL_LEFT`.`x` = `TBL_RIGHT`.`x`)
    ## ) AS `TBL_LEFT`
    ##   LEFT JOIN (SELECT `TBL_LEFT`.`x` AS `x`
    ##   FROM `d` AS `TBL_LEFT`
    ##   LEFT JOIN `d` AS `TBL_RIGHT`
    ##   ON (`TBL_LEFT`.`x` = `TBL_RIGHT`.`x`)
    ## ) AS `TBL_RIGHT`
    ##   ON (`TBL_LEFT`.`x` = `TBL_RIGHT`.`x`)
    ## ) AS `TBL_RIGHT`
    ##   ON (`TBL_LEFT`.`x` = `TBL_RIGHT`.`x`)

One could hope the query optimizer will detect and eliminate the common sub-expressions, but that is not always going to be the case. In fact sometimes the very size of a query turns off the query optimizer in systems such as `Spark`. It is better to organize your calculation to not emit so many common sub-expressions in the first place. We share [here](https://github.com/WinVector/rquery/blob/master/extras/query_growth/time_dag.md) an example showing explicit value re-use on `Spark` (preventing a crash in `dplyr`, but actually slowing down `rquery`- implying the `Spark` optimizer is very good when it survives). And we also share a variation of the example [here](https://github.com/WinVector/rquery/blob/master/extras/query_growth/time_dag_PostgreSQL.md) using `PostreSQL` where landing intermediate results speeds up both `dplyr` and `rquery` (`dplyr` being faster in this particular example; which is not always the case and may be the case, and here may be due to `rquery`'s `natural_join()` coalesce semantics).

Basic solutions
---------------

`dplyr` can easily overcome this limitation with its `compute()` node, which lands or materializes intermediate results for re-use.

``` r
d1_dplyr <- compute(left_join(d0_dplyr, d0_dplyr, 
                              by = "x"))
d2_dplyr <- compute(left_join(d1_dplyr, d1_dplyr, 
                              by = "x"))
d3_dplyr <- compute(left_join(d2_dplyr, d2_dplyr, 
                              by = "x"))
dbplyr::remote_query(d3_dplyr)
```

    ## <SQL> SELECT *
    ## FROM `stthpcrlgf`

Notice the `dplyr::compute()` results are actually tables (not general queries).

`rquery` can also fix the issue by landing intermediate results, though the table lifetime tracking is intentionally more explicit (and the separation between code and results is also deliberately much more explicit).

``` r
tmps <- wrapr::mk_tmp_name_source()

d1_mat <- materialize(
  db_rquery,
  natural_join(d0, d0, 
               by = "x", jointype = "LEFT"),
  table_name = tmps(), 
  temporary = TRUE, overwrite = TRUE)
d2_mat <- materialize(
  db_rquery,
  natural_join(d1_mat, d1_mat, 
               by = "x", jointype = "LEFT"),
  table_name = tmps(), 
  temporary = TRUE, overwrite = TRUE)
d3_mat <- materialize(
  db_rquery,
  natural_join(d2_mat, d2_mat, 
               by = "x", jointype = "LEFT"),
  table_name = tmps(), 
  temporary = TRUE, overwrite = TRUE)
cat(format(d3_mat))
```

    ## table(`tmpnam_85242167791778851766_0000000002`; 
    ##   x)

More advanced solutions
-----------------------

With a more advanced "relop\_list" notation we can both build the efficient query plan, but also the diagram certifying the lack of redundant stages.

``` r
relop_list <- rquery::make_relop_list(tmps)

d1_ops <- natural_join(d0, d0, 
                       by = "x", jointype = "LEFT") %.>%
  relop_list

d2_ops <- natural_join(d1_ops, d1_ops, 
                       by = "x", jointype = "LEFT") %.>%
  relop_list

d3_ops <- natural_join(d2_ops, d2_ops, 
                       by = "x", jointype = "LEFT") %.>%
  relop_list
```

``` r
relop_list %.>%
  get_relop_list_stages(.) %.>%
  op_diagram(., merge_tables = TRUE) %.>% 
  DiagrammeR::grViz(.) %.>%
  DiagrammeRsvg::export_svg(.) %.>%
  write(., file="query_growth_diagram2.svg")
```

![](query_growth_diagram2.svg)

The `relop_list` collector is introducing and managing intermediate tables. It is simple to materialized inspect the results (either through piping or using `materialize_relop_list_stages()`).

``` r
print(relop_list)
```

    ## $tmpnam_85242167791778851766_0000000003
    ## [1] "table(`d`; x) %.>% natural_join(., table(`d`; x), j= LEFT, by= x)"
    ## 
    ## $tmpnam_85242167791778851766_0000000004
    ## [1] "table(tmpnam_85242167791778851766_0000000003; x) %.>% natural_join(., table(tmpnam_85242167791778851766_0000000003; x), j= LEFT, by= x)"
    ## 
    ## $tmpnam_85242167791778851766_0000000005
    ## [1] "table(tmpnam_85242167791778851766_0000000004; x) %.>% natural_join(., table(tmpnam_85242167791778851766_0000000004; x), j= LEFT, by= x)"

``` r
result <- relop_list %.>% db_rquery
print(result)
```

    ## [1] "table(`tmpnam_85242167791778851766_0000000005`; x)"

``` r
DBI::dbReadTable(raw_connection, result$table_name) %.>%
  knitr::kable(.)
```

|    x|
|----:|
|   -3|
|   -2|
|   -1|
|    0|
|    1|
|    2|
|    3|

Conclusion
----------

[`rquery`](https://github.com/WinVector/rquery) is an advanced `SQL` query generator for `R` that includes a number of tools for managing complex queries. If your queries are taking substantial development time or substantial run time you should definitely consider trying the `rquery` system. For convenience there is also [`data.table`](https://CRAN.R-project.org/package=data.table) based implementation of the `rquery` grammar called [`rqdatatable`](https://github.com/WinVector/rqdatatable) ([which tends to be *much* faster that `dplyr`](http://www.win-vector.com/blog/2018/08/timings-of-a-grouped-rank-filter-task/)).

``` r
# clean up tmps
intermediates <- tmps(dumpList = TRUE)
for(ti in intermediates) {
  rquery::rq_remove_table(db_rquery, ti)
}

DBI::dbDisconnect(raw_connection)
```
