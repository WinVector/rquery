RPostgres schema
================

Working with a schema. Package is not quite ready for schemas yet as we
are not yet qualifying table names enough places.

``` r
library("rquery")
library("wrapr")
packageVersion("rquery")
```

    ## [1] '1.3.4'

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
DBI::dbExecute(raw_connection, "CREATE TABLE test_schema.test_table (email varchar)")
```

    ## [1] 0

``` r
DBI::dbExecute(raw_connection, "INSERT INTO test_schema.test_table (email) VALUES ('j@example.com')")
```

    ## [1] 1

``` r
DBI::dbGetQuery(raw_connection, "SELECT * FROM test_schema.test_table")
```

    ##           email
    ## 1 j@example.com

``` r
table_handle <- db_td(db, "test_table", qualifiers = c(schema = "test_schema"))

execute(db, table_handle)
```

    ##           email
    ## 1 j@example.com

``` r
DBI::dbExecute(raw_connection, "DROP SCHEMA IF EXISTS test_schema CASCADE")
```

    ## [1] 0

``` r
DBI::dbDisconnect(raw_connection)
```
