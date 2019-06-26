rand examples
================

Example of `rand` behaving similarly on local and remote data.

``` r
library("rquery")
library("wrapr")

raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
RSQLite::initExtension(raw_connection)
db <- rquery_db_info(
  connection = raw_connection,
  is_dbi = TRUE,
  connection_options = rq_connection_tests(raw_connection))

t1l <- data.frame(a = c(1, 2), b = c(3, 4))
t1r <- rq_copy_to(db, "t1r", t1l)

ops <- t1r %.>%
  extend(., r := rand())

cat(format(ops))
```

    ## table(`t1r`; 
    ##   a,
    ##   b) %.>%
    ##  extend(.,
    ##   r := rand())

``` r
cat(to_sql(ops, db))
```

    ## SELECT
    ##  `a`,
    ##  `b`,
    ##  ABS ( ( RANDOM ( ) % 268435456 ) / 268435455.0 )  AS `r`
    ## FROM (
    ##  SELECT
    ##   `a`,
    ##   `b`
    ##  FROM
    ##   `t1r`
    ##  ) tsql_15966864495600207824_0000000000

``` r
execute(db, ops) %.>%
  knitr::kable(.)
```

| a | b |         r |
| -: | -: | --------: |
| 1 | 3 | 0.0621963 |
| 2 | 4 | 0.0156907 |

Note `RANDOM()` is not a safe command in `RSQLite` as it is
re-calculated (and takes a new value) each place it is used. Notice `r1
does not equal`r2\`.

``` r
q <- "SELECT r AS r1, r AS r2 FROM (
           SELECT random() AS r FROM (
            SELECT * from ( VALUES(1),(2) )
          ) a
       ) b"
DBI::dbGetQuery(raw_connection, q) %.>%
  knitr::kable(.)
```

|                   r1 |                    r2 |
| -------------------: | --------------------: |
| \-130915452702763056 | \-1888076243036538609 |
|  7344461845789912452 |  \-122832139304135044 |

``` r
DBI::dbDisconnect(raw_connection)
```

We can run the same code on in-memory `data.frame`s using `rqdatatable`.

``` r
library("rqdatatable")

t1l %.>% 
  ops %.>%
  knitr::kable(.)
```

| a | b |         r |
| -: | -: | --------: |
| 1 | 3 | 0.8166875 |
| 2 | 4 | 0.7347532 |
