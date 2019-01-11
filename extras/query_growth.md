query growth
================

Query sequences joined to themselves blow up the query complexity exponentially, as each path is re-build on each attempted re-use.

``` r
library("rquery")

db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

d <- data.frame(x = -3:3)
d0 <- rq_copy_to(db, "d", d)

d1 <- natural_join(d0, d0, by = "x", jointype = "LEFT")
d2 <- natural_join(d1, d1, by = "x", jointype = "LEFT")
d3 <- natural_join(d2, d2, by = "x", jointype = "LEFT")
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

``` r
DBI::dbDisconnect(db)
```

This is not unique to [`rquery`](https://CRAN.R-project.org/package=rquery), [`dplyr`](https://CRAN.R-project.org/package=dplyr) has the same issue.

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
db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

d <- data.frame(x = -3:3)
d0 <- dplyr::copy_to(db, d, "d")

d1 <- left_join(d0, d0, by = "x")
d2 <- left_join(d1, d1, by = "x")
d3 <- left_join(d2, d2, by = "x")
dbplyr::remote_query(d3)
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

``` r
DBI::dbDisconnect(db)
```

Largely it is a lack of a convenient way to name and cache the intermediate results in basic `SQL`.

`dplyr` can easily overcome this limitation with it's `compute()` node.

``` r
library("dplyr")

db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

d <- data.frame(x = -3:3)
d0 <- dplyr::copy_to(db, d, "d")

d1 <- compute(left_join(d0, d0, by = "x"))
d2 <- compute(left_join(d1, d1, by = "x"))
d3 <- compute(left_join(d2, d2, by = "x"))
dbplyr::remote_query(d3)
```

    ## <SQL> SELECT *
    ## FROM `gxhhblafgm`

``` r
DBI::dbDisconnect(db)
```

`rquery` can also land intermediate results, though the table lifetime tracking is intentionally more explicit.

``` r
library("rquery")

db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
tmps <- mk_tmp_name_source("ex")

d <- data.frame(x = -3:3)
d0 <- rq_copy_to(db, "d", d)

d1 <- materialize(db,
                  natural_join(d0, d0, by = "x", jointype = "LEFT"),
                  table_name = tmps(), temporary = TRUE, overwrite = TRUE)
d2 <- materialize(db,
                  natural_join(d1, d1, by = "x", jointype = "LEFT"),
                  table_name = tmps(), temporary = TRUE, overwrite = TRUE)
d3 <- materialize(db,
                  natural_join(d2, d2, by = "x", jointype = "LEFT"),
                  table_name = tmps(), temporary = TRUE, overwrite = TRUE)
cat(format(d3))
```

    ## table(`ex_84942077294344486277_0000000002`; 
    ##   x)

``` r
# clean up tmps
intermediates <- tmps(dumpList = TRUE)
for(ti in intermediates) {
  rquery::rq_remove_table(db, ti)
}

DBI::dbDisconnect(db)
```

For a non-trivial example of computation management and value re-use please see [here](https://github.com/WinVector/rquery/blob/master/db_examples/RSQLite.md).
