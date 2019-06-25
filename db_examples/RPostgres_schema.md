RPostgres schema
================

Working with a schema. Package is not quite ready for schemas yet as we
are not yet qualifying table names enough places.

``` r
library("rquery")
library("wrapr")
packageVersion("rquery")
```

    ## [1] '1.3.5'

``` r
raw_connection <- DBI::dbConnect(RPostgres::Postgres(),
                                 host = 'localhost',
                                 port = 5432,
                                 user = 'johnmount',
                                 password = '')


dbopts <- rq_connection_tests(raw_connection)
db <- rquery_db_info(connection = raw_connection,
                     is_dbi = TRUE,
                     connection_options = dbopts)

DBI::dbExecute(raw_connection, "DROP SCHEMA IF EXISTS test_schema CASCADE")
```

    ## [1] 0

``` r
DBI::dbExecute(raw_connection, "CREATE SCHEMA test_schema")
```

    ## [1] 0

``` r
rq_copy_to(db, "test_table", data.frame(x = 1), 
           qualifiers = c(schema = "test_schema"), temporary = FALSE)
```

    ## [1] "table(\"test_schema\".\"test_table\"; x)"

``` r
table_handle <- db_td(db, "test_table", qualifiers = c(schema = "test_schema"))

print(table_handle)
```

    ## [1] "table(\"test_schema\".\"test_table\"; x)"

``` r
execute(db, table_handle)
```

    ##   x
    ## 1 1

``` r
DBI::dbExecute(raw_connection, "DROP SCHEMA IF EXISTS test_schema CASCADE")
```

    ## [1] 0

``` r
DBI::dbDisconnect(raw_connection)
```
