DBI Tests
================
John Mount, Win-Vector LLC
5/15/2018

[`rquery`](https://github.com/WinVector/rquery) `0.4.2` now includes tests that report which [`DBI`](https://CRAN.R-project.org/package=DBI) methods appear to be correclty implemented for a given database connection. `rquery` also supplies a number of work-around methods that use these determinations.

Here are the tests on a few popular database connections.

[`RSQLite`](https://CRAN.R-project.org/package=RSQLite)
-------------------------------------------------------

``` r
db <- DBI::dbConnect(RSQLite::SQLite(), 
                     ":memory:")

rquery::rq_connection_tests(db)
```

    ## $rquery.SQLiteConnection.use_DBI_dbListFields
    ## [1] TRUE
    ## 
    ## $rquery.SQLiteConnection.use_DBI_dbRemoveTable
    ## [1] TRUE
    ## 
    ## $rquery.SQLiteConnection.use_DBI_dbExecute
    ## [1] TRUE
    ## 
    ## $rquery.SQLiteConnection.create_temporary
    ## [1] FALSE
    ## 
    ## $rquery.SQLiteConnection.control_temporary
    ## [1] TRUE
    ## 
    ## $rquery.SQLiteConnection.control_rownames
    ## [1] TRUE
    ## 
    ## $rquery.SQLiteConnection.use_DBI_dbExistsTable
    ## [1] TRUE
    ## 
    ## $rquery.SQLiteConnection.check_logical_column_types
    ## [1] TRUE

``` r
DBI::dbDisconnect(db)
```

[`RPostgreSQL`](https://CRAN.R-project.org/package=RPostgreSQL)
---------------------------------------------------------------

``` r
db <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')

rquery::rq_connection_tests(db)
```

    ## $rquery.PostgreSQLConnection.use_DBI_dbListFields
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbRemoveTable
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbExecute
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.create_temporary
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.control_temporary
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.control_rownames
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbExistsTable
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.check_logical_column_types
    ## [1] FALSE

``` r
DBI::dbDisconnect(db)
```

    ## [1] TRUE

[`RPostgres`](https://CRAN.R-project.org/package=RPostgres)
-----------------------------------------------------------

``` r
db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')

rquery::rq_connection_tests(db)
```

    ## $rquery.PqConnection.use_DBI_dbListFields
    ## [1] TRUE
    ## 
    ## $rquery.PqConnection.use_DBI_dbRemoveTable
    ## [1] TRUE
    ## 
    ## $rquery.PqConnection.use_DBI_dbExecute
    ## [1] TRUE
    ## 
    ## $rquery.PqConnection.create_temporary
    ## [1] FALSE
    ## 
    ## $rquery.PqConnection.control_temporary
    ## [1] TRUE
    ## 
    ## $rquery.PqConnection.control_rownames
    ## [1] TRUE
    ## 
    ## $rquery.PqConnection.use_DBI_dbExistsTable
    ## [1] TRUE
    ## 
    ## $rquery.PqConnection.check_logical_column_types
    ## [1] FALSE

``` r
DBI::dbDisconnect(db)
```

[`sparklyr`](https://CRAN.R-project.org/package=sparklyr)
---------------------------------------------------------

``` r
db <- sparklyr::spark_connect(version='2.2.0', 
                              master = "local")

rquery::rq_connection_tests(db)
```

    ## $rquery.DBIConnection_spark_connection_spark_shell_connection.use_DBI_dbListFields
    ## [1] FALSE
    ## 
    ## $rquery.DBIConnection_spark_connection_spark_shell_connection.use_DBI_dbRemoveTable
    ## [1] FALSE
    ## 
    ## $rquery.DBIConnection_spark_connection_spark_shell_connection.use_DBI_dbExecute
    ## [1] TRUE
    ## 
    ## $rquery.DBIConnection_spark_connection_spark_shell_connection.create_temporary
    ## [1] FALSE
    ## 
    ## $rquery.DBIConnection_spark_connection_spark_shell_connection.control_temporary
    ## [1] TRUE
    ## 
    ## $rquery.DBIConnection_spark_connection_spark_shell_connection.control_rownames
    ## [1] FALSE
    ## 
    ## $rquery.DBIConnection_spark_connection_spark_shell_connection.use_DBI_dbExistsTable
    ## [1] TRUE
    ## 
    ## $rquery.DBIConnection_spark_connection_spark_shell_connection.check_logical_column_types
    ## [1] FALSE

``` r
sparklyr::spark_disconnect(db)
```
