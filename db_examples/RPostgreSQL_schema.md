RPostgreSQL schema
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
raw_connection <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
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

print(table_handle)
```

    ## [1] "table(\"test_schema\".\"test_table\"; email)"

``` r
execute(db, table_handle)
```

    ##           email
    ## 1 j@example.com

``` r
## Doesn't work yet. as DBI::dbWriteTable() doesn't seem to use schema in this case
#rq_copy_to(db, "test_table", data.frame(x = 1), 
#           qualifiers = c(schema = "test_schema"), temporary = FALSE)

# exhibit schema issue
# try with qualifiers
DBI::dbWriteTable(raw_connection, "schema_example_1", data.frame(x = 1), 
                  qualifiers = c(schema = "test_schema"),
                  overwrite = TRUE)
```

    ## [1] TRUE

``` r
# try with unquoted name
DBI::dbWriteTable(raw_connection, "test_schema.schema_example_2", data.frame(x = 1), 
                  overwrite = TRUE)
```

    ## [1] TRUE

``` r
# try with quoted name
DBI::dbWriteTable(raw_connection, '"test_schema"."schema_example_3"', data.frame(x = 1), 
                  overwrite = TRUE)
```

    ## [1] TRUE

``` r
# try with quoted name
DBI::dbWriteTable(raw_connection, "'test_schema'.'schema_example_4'", data.frame(x = 1), 
                  overwrite = TRUE)
```

    ## [1] TRUE

``` r
# look for tables, notice all in public schema
tab <- DBI::dbGetQuery(raw_connection, "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME LIKE '%schema_example_%'")
knitr::kable(tab)
```

| table\_catalog | table\_schema | table\_name                         | table\_type | self\_referencing\_column\_name | reference\_generation | user\_defined\_type\_catalog | user\_defined\_type\_schema | user\_defined\_type\_name | is\_insertable\_into | is\_typed | commit\_action |
| :------------- | :------------ | :---------------------------------- | :---------- | :------------------------------ | :-------------------- | :--------------------------- | :-------------------------- | :------------------------ | :------------------- | :-------- | :------------- |
| johnmount      | public        | schema\_example\_1                  | BASE TABLE  | NA                              | NA                    | NA                           | NA                          | NA                        | YES                  | NO        | NA             |
| johnmount      | public        | test\_schema.schema\_example\_2     | BASE TABLE  | NA                              | NA                    | NA                           | NA                          | NA                        | YES                  | NO        | NA             |
| johnmount      | public        | “test\_schema”.“schema\_example\_3” | BASE TABLE  | NA                              | NA                    | NA                           | NA                          | NA                        | YES                  | NO        | NA             |
| johnmount      | public        | ‘test\_schema’.’schema\_example\_4’ | BASE TABLE  | NA                              | NA                    | NA                           | NA                          | NA                        | YES                  | NO        | NA             |

``` r
DBI::dbExecute(raw_connection, "DROP SCHEMA IF EXISTS test_schema CASCADE")
```

    ## [1] 0

``` r
DBI::dbDisconnect(raw_connection)
```

    ## [1] TRUE
