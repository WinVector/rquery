SQL Tree Re-Writer
================
John Mount, Win-Vector LLC
2019-01-12

[`rquery`](https://CRAN.R-project.org/package=rquery) `1.3.0` now incorporates a general `SQL` tree-rewriting capability. This is to help adapt `rquery` to different databases.

Let's work an example using [`rqdatatable`](https://CRAN.R-project.org/package=rqdatatable) (`rquery` in memory using [`data.table`](https://CRAN.R-project.org/package=data.table)), [`RPostgreSQL`](https://CRAN.R-project.org/package=RPostgreSQL), and [`RSQLite`](https://CRAN.R-project.org/package=RSQLite).

First lets confirm we have the needed packages installed and attach them.

``` r
library("rquery")

have_rqdatatable <- requireNamespace("rqdatatable", quietly = TRUE) 
if(have_rqdatatable) {
  library("rqdatatable")
}

have_postgresql <- requireNamespace("DBI", quietly = TRUE) && 
  requireNamespace("RPostgreSQL", quietly = TRUE) 
if(have_postgresql) {
  library("RPostgreSQL")
}

have_rsqlite <- requireNamespace("DBI", quietly = TRUE) && 
  requireNamespace("RSQLite", quietly = TRUE) 
if(have_rsqlite) {
  library("RSQLite")
}
```

Now let's set up a simple data example.

``` r
d <- data.frame(x = -3:3)
knitr::kable(d)
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

We define the `rquery` pipe line to compute `x mod 2`.

``` r
ops1 <- local_td(d) %.>%
  extend(., 
         x_mod_2 = x %% 2)

cat(format(ops1))
```

    ## table(d; 
    ##   x) %.>%
    ##  extend(.,
    ##   x_mod_2 := x %% 2)

And `rqdatatable` can execute the pipeline against any table that has the required numeric `x` column.

``` r
d %.>% 
  ops1 %.>% 
  knitr::kable(.)
```

|    x|  x\_mod\_2|
|----:|----------:|
|   -3|          1|
|   -2|          0|
|   -1|          1|
|    0|          0|
|    1|          1|
|    2|          0|
|    3|          1|

In principle applying the same operations to a database table is easy. However `SQL` `mod` unfortunately returns the sign of the argument in its result. This means its range of `x mod 2` can be from `-1` through `1` instead of the more typical `0` through `1` (it breaks much of the mathematical purpose of modulo if we don't have `mod(a-b, k)==0` equivalent to `mod(a)-mod(b)==0`; [`dplyr` runs into the same issue](https://github.com/WinVector/rquery/blob/master/extras/dplyr_modulo.md)).

To work around this we define a larger pipeline that corrects the sign after the modulo operation.

``` r
ops <- local_td(d) %.>%
  extend(., 
         x_mod_2 = x %% 2) %.>%
  extend(., 
         # extra step to work around SQL mod
         # returns sign of argument.
         x_mod_2 = ifelse(x_mod_2 >=0, x_mod_2, x_mod_2 + 2))

cat(format(ops))
```

    ## table(d; 
    ##   x) %.>%
    ##  extend(.,
    ##   x_mod_2 := x %% 2) %.>%
    ##  extend(.,
    ##   x_mod_2 := ifelse(x_mod_2 >= 0, x_mod_2, x_mod_2 + 2))

This pipeline still works in memory.

``` r
d %.>% 
  ops %.>% 
  knitr::kable(.)
```

|    x|  x\_mod\_2|
|----:|----------:|
|   -3|          1|
|   -2|          0|
|   -1|          1|
|    0|          0|
|    1|          1|
|    2|          0|
|    3|          1|

And it works correctly on `PostgreSQL` database tables.

``` r
postgresql_connection <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = 'localhost',
  port = 5432,
  user = 'johnmount',
  password = '')

postgresql_db <- rquery_db_info(
  connection = postgresql_connection,
  is_dbi = TRUE,
  connection_options = rq_connection_tests(postgresql_connection))

postgresql_db
```

    ## [1] "rquery_db_info(PostgreSQLConnection, is_dbi=TRUE, note=\"\")"

``` r
d_postgresql <- rq_copy_to(postgresql_db, "d", d,
                           temporary = TRUE, overwrite = TRUE)

d_postgresql
```

    ## [1] "table(\"d\"; x)"

``` r
rstr(postgresql_db, d_postgresql)
```

    ## table "d" rquery_db_info 
    ##  nrow: 7 
    ## 'data.frame':    7 obs. of  1 variable:
    ##  $ x: int  -3 -2 -1 0 1 2 3

``` r
cat(to_sql(ops, postgresql_db))
```

    ## SELECT
    ##  "x",
    ##  ( CASE WHEN ( "x_mod_2" >= 0 ) THEN ( "x_mod_2" ) WHEN NOT ( "x_mod_2" >= 0 ) THEN ( "x_mod_2" + 2 ) ELSE NULL END )  AS "x_mod_2"
    ## FROM (
    ##  SELECT
    ##   "x",
    ##   MOD ( "x" , 2 )  AS "x_mod_2"
    ##  FROM (
    ##   SELECT
    ##    "x"
    ##   FROM
    ##    "d"
    ##   ) tsql_44021238336041178501_0000000000
    ##  ) tsql_44021238336041178501_0000000001

``` r
ops %.>% 
  postgresql_db %.>%
  knitr::kable(.)
```

|    x|  x\_mod\_2|
|----:|----------:|
|   -3|          1|
|   -2|          0|
|   -1|          1|
|    0|          0|
|    1|          1|
|    2|          0|
|    3|          1|

``` r
DBI::dbDisconnect(postgresql_connection)
```

    ## [1] TRUE

However `SQLite` needs `mod(a,b)` written as `a % b`. So we need to re-write our `SQL` to comply with `SQLite`'s variation when talking to `SQLite`. In `rquery` this is easy: we define a tree-node re-write rule and `rquery`'s `SQL` translator lets this user code visit every node of a `SQL` parse tree during the translation.

The use code looks like this:

``` r
tree_rewriter <- function(x, db_info) {
  if(("pre_sql_sub_expr" %in% class(x)) && 
     (length(x$info$name) == 1) &&
     (x$info$name == "modulo")) {
    lhs <- x$toks[[3]]
    rhs <- x$toks[[5]]
    return(pre_sql_sub_expr(
      list(pre_sql_token("("),
           lhs,
           pre_sql_token("%"),
           rhs,
           pre_sql_token(")")),
      info=list(name = "user_replaced"))
    )
  }
  x
}
```

And using the re-writer is as easy as attaching it to our database handle, which we show below.

``` r
rsqlite_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
rsqlite_db <- rquery_db_info(
  connection = rsqlite_connection,
  is_dbi = TRUE,
  connection_options = rq_connection_tests(rsqlite_connection))

# attach our tree-rewriter to the databse handle.
# this handle now uses this re-writer.
rsqlite_db$tree_rewriter <- tree_rewriter

rsqlite_db
```

    ## [1] "rquery_db_info(SQLiteConnection, is_dbi=TRUE, note=\"\")"

``` r
d_rsqlite <- rq_copy_to(rsqlite_db, "d", d,
                        temporary = TRUE, overwrite = TRUE)

d_rsqlite
```

    ## [1] "table(`d`; x)"

``` r
rstr(rsqlite_db, d_rsqlite)
```

    ## table `d` rquery_db_info 
    ##  nrow: 7 
    ## 'data.frame':    7 obs. of  1 variable:
    ##  $ x: int  -3 -2 -1 0 1 2 3

``` r
cat(to_sql(ops, rsqlite_db))
```

    ## SELECT
    ##  `x`,
    ##  ( CASE WHEN ( `x_mod_2` >= 0 ) THEN ( `x_mod_2` ) WHEN NOT ( `x_mod_2` >= 0 ) THEN ( `x_mod_2` + 2 ) ELSE NULL END )  AS `x_mod_2`
    ## FROM (
    ##  SELECT
    ##   `x`,
    ##   ( `x` % 2 )  AS `x_mod_2`
    ##  FROM (
    ##   SELECT
    ##    `x`
    ##   FROM
    ##    `d`
    ##   ) tsql_33601897649263872580_0000000000
    ##  ) tsql_33601897649263872580_0000000001

``` r
ops %.>% 
  rsqlite_db %.>%
  knitr::kable(.)
```

|    x|  x\_mod\_2|
|----:|----------:|
|   -3|          1|
|   -2|          0|
|   -1|          1|
|    0|          0|
|    1|          1|
|    2|          0|
|    3|          1|

We learn how to write the transform by using the function `str_pre_sql_sub_expr()` which prints out node details, telling us where to get the arguments for our new `SQL` code. We can dump all the nodes the `SQL` translator visits (to see what pattern we would have to re-write) as follows. The `"pre_sql_sub_expr" %in% class(x)` is merely restricting the printing to non-leaf nodes (the ones likely to hold the references to the values we need).

``` r
rsqlite_db$tree_rewriter <- function(x, db_info) {
  if("pre_sql_sub_expr" %in% class(x)) {
    print(str_pre_sql_sub_expr(x))
  }
  x
}
untranslated_sql <- to_sql(ops, rsqlite_db)
```

    ## [1] "{:_ifelse 1_{(} 2_{CASE} 3_{WHEN} 4_{(} 5_{{:_inline_op 1_{\"x_mod_2\"} 2_{>=} 3_{0} :}} 6_{)} 7_{THEN} 8_{(} 9_{\"x_mod_2\"} 10_{)} 11_{WHEN} 12_{NOT (} 13_{{:_inline_op 1_{\"x_mod_2\"} 2_{>=} 3_{0} :}} 14_{)} 15_{THEN} 16_{(} 17_{{:_inline_op 1_{\"x_mod_2\"} 2_{+} 3_{2} :}} 18_{)} 19_{ELSE} 20_{NULL} 21_{END} 22_{)} :}"
    ## [1] "{:_inline_op 1_{\"x_mod_2\"} 2_{>=} 3_{0} :}"
    ## [1] "{:_inline_op 1_{\"x_mod_2\"} 2_{>=} 3_{0} :}"
    ## [1] "{:_inline_op 1_{\"x_mod_2\"} 2_{+} 3_{2} :}"
    ## [1] "{:_modulo 1_{MOD} 2_{(} 3_{\"x\"} 4_{,} 5_{2} 6_{)} :}"

From the above we see that the node named "modulo" (name found after the first colon) is the one we need to re-write, and that it has the values we need as terms `3` and `5` (where we find the values `x` and `2`).

In addition to expression-tree re-writing procedures `rquery` also allows a per-handle replacement of any `to_sql()` method via the `db_methods` map found on the `rquery_db_info` database handle. Yet another form of adaption can be found here, where we [by-hand replace a window function with a join](https://github.com/WinVector/rquery/blob/master/db_examples/MonetDBLite.md) when using `MonetDBLite` (as while `MonetDBLite` defines window functions, it doesn't define a windowed version of `sum()`; also check out the operator diagram showing the two uses of the same table arriving at a join).

And that is a brief introduction to adapting `rquery` to new databases.

``` r
DBI::dbDisconnect(rsqlite_connection)
```
