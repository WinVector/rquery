RPostgres date conversion example
================

Converting dates in a Postgres database.

Unlike `dplyr`, `rquery` does not depend on a hybrid-eval and also does
not have translations for as many `R`-functions. So one has to use the
Postgres date conversion which is `to_date(COLUMN, FORMAT)`.

``` r
library(rqdatatable)
```

    ## Loading required package: rquery

``` r
library(rquery)
library(wrapr)
library(RPostgres)

raw_connection <- DBI::dbConnect(RPostgres::Postgres(),
                                  host = 'localhost',
                                  port = 5432,
                                  user = 'johnmount',
                                  password = '')

dbopts <- rq_connection_tests(raw_connection)
db <- rquery_db_info(connection = raw_connection,
                     is_dbi = TRUE,
                     connection_options = dbopts)

print(db)
```

    ## [1] "rquery_db_info(PqConnection, is_dbi=TRUE, note=\"\")"

``` r
# local table
df <- wrapr::build_frame(
  "id"  , "date"       |
    "a" , "2019-01-12" |
    "b" , "2019-02-21" |
    "c" , "2019-02-11" )

# remote table
db_testdate <- rq_copy_to(db, "testdate",
           df, overwrite = TRUE)

# R idiom local op
df %.>%
  extend(., date := as.Date(date))
```

    ##    id       date
    ## 1:  a 2019-01-12
    ## 2:  b 2019-02-21
    ## 3:  c 2019-02-11

``` r
# SQL idiom remote op
db_result <- db_testdate %.>%
  extend(. , date := to_date(date, "YYYY-MM-DD")) %.>%
  materialize(db, .)
DBI::dbReadTable(db$connection, db_result$table_name) %.>%
  str(.)
```

    ## 'data.frame':    3 obs. of  2 variables:
    ##  $ id  : chr  "a" "b" "c"
    ##  $ date: Date, format: "2019-01-12" "2019-02-21" ...

``` r
# translated remote op

ops <- db_testdate %.>%
   extend(., date := as.Date(date))

cat(to_sql(ops, db))
```

    ## SELECT
    ##  "id",
    ##  to_date ( "date" , 'YYYY-MM-DD' )  AS "date"
    ## FROM (
    ##  SELECT
    ##   "id",
    ##   "date"
    ##  FROM
    ##   "testdate"
    ##  ) tsql_90684675899976646004_0000000000

``` r
execute(db, ops)  %.>%
  str(.)
```

    ## 'data.frame':    3 obs. of  2 variables:
    ##  $ id  : chr  "a" "b" "c"
    ##  $ date: Date, format: "2019-01-12" "2019-02-21" ...

``` r
rquery::rq_function_mappings(db) %.>%
  knitr::kable(.)
```

| fn\_name | sql\_mapping                     | simple\_name\_mapping |
| :------- | :------------------------------- | :-------------------- |
| mean     | avg                              | TRUE                  |
| as.Date  | to\_date ( .(3) , ‘YYYY-MM-DD’ ) | FALSE                 |

``` r
DBI::dbDisconnect(raw_connection)
```

More on what re-mappings `rquery` provides can be found
[here](https://winvector.github.io/rquery/articles/R_mapping.html).
