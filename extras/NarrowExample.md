NarrowExample
================
Win-Vector LLC

<!-- NarrowExample.md is generated from NarrowExample.Rmd. Please edit that file -->
Let's take a quick look at what we are calling query narrowing. For our example let's set up a database connection and copy a small table into the database.

``` r
db <- DBI::dbConnect(RSQLite::SQLite(),
                         ":memory:")

td <- rquery::rq_copy_to(db, 
                         "d", 
                         data.frame(a = 1:3, b = 4:6, c = 7:9),
                         temporary = TRUE,
                         overwrite = TRUE)

print(td)
```

    ## [1] "table(`d`; a, b, c)"

``` r
rquery::rstr(db, td$table_name)
```

    ## table `d` SQLiteConnection 
    ##  nrow: 3 
    ## 'data.frame':    3 obs. of  3 variables:
    ##  $ a: int  1 2 3
    ##  $ b: int  4 5 6
    ##  $ c: int  7 8 9

For our first example we will user [`rquery`](https://github.com/WinVector/rquery) to generate some `SQL`.

``` r
library("rquery")
library("wrapr")

op1 <- td %.>% 
  extend_nse(., e := a + 1)
cat(to_sql(op1, db))
```

    ## SELECT
    ##  `a`,
    ##  `b`,
    ##  `c`,
    ##  `a` + 1  AS `e`
    ## FROM (
    ##  SELECT
    ##   `a`,
    ##   `b`,
    ##   `c`
    ##  FROM
    ##   `d`
    ##  ) tsql_30228218124473308630_0000000000

Notice the above `SQL` has a trivial extra inner select step. `rquery` reserves this `SQL` for extra effects such as query narrowing and it is presumed that such selects are easily removed by downstream query optimizers. The way `rquery` uses this stage is shown as follows. Suppose we later declare we are only going to use the new column "`e`" as our our result.

``` r
op2 <- op1 %.>% 
  select_columns(., "e")

cat(to_sql(op2, db))
```

    ## SELECT
    ##  `e`
    ## FROM (
    ##  SELECT
    ##   `a` + 1  AS `e`
    ##  FROM (
    ##   SELECT
    ##    `a`
    ##   FROM
    ##    `d`
    ##   ) tsql_16807721345884970846_0000000000
    ## ) tsql_16807721345884970846_0000000001

``` r
db %.>% op2
```

    ##   e
    ## 1 2
    ## 2 3
    ## 3 4

`rquery` propagated the columns used all the way to the inner query. This makes the data processing thinner and in fact [often faster](https://github.com/WinVector/rquery/blob/master/extras/NarrowEffectSpark.md) as even with "lazy evaluation" there is significant cost associated with processing the additional columns (and this is not always eliminated by the query optimizers). The narrowing effect can be critical if one caches or stores an intermediate result. `rquery` did introduce some trivial outer `SQL` to represent the outer select step, but we again assume this is the sort of thing that is easy for query optimizers to remove.

In contrast `dplyr` does not back-propagate later constraints to earlier in the query. Notice below how the inner query requests many unused columns.

``` r
library("dplyr")
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:wrapr':
    ## 
    ##     coalesce

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
packageVersion("dplyr")
```

    ## [1] '0.7.6'

``` r
packageVersion("dbplyr")
```

    ## [1] '1.2.2'

``` r
hdl <- dplyr::tbl(db, "d")

hdl %>%
  mutate(., e = a + 1) %>%
  dbplyr::remote_query(.)
```

    ## <SQL> SELECT `a`, `b`, `c`, `a` + 1.0 AS `e`
    ## FROM `d`

``` r
hdl %>%
  mutate(., e = a + 1) %>%
  select(., e) %>%
  dbplyr::remote_query(.)
```

    ## <SQL> SELECT `e`
    ## FROM (SELECT `a`, `b`, `c`, `a` + 1.0 AS `e`
    ## FROM `d`)

Notice `dplyr`/`dbplyr` does not propagate the column narrowing back to earlier expressions.

``` r
DBI::dbDisconnect(db)
```
