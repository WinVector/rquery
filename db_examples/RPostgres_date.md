RPostgres date conversion example
================

Converting dates in a Postgres database.

Unlike `dplyr`, `rquery` does not depend on a hybrid-eval and also does
not have translations for as many `R`-functions. So one has to use the
Postgres date conversion which is `to_date(COLUMN, FORMAT)`.

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

df <- wrapr::build_frame(
  "id"  , "date"       |
    "a" , "2019-01-12" |
    "b" , "2019-02-21" |
    "c" , "2019-02-11" )

db_testdate <- rq_copy_to(db, "testdate",
           df, overwrite = TRUE)

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
DBI::dbDisconnect(raw_connection)
```
