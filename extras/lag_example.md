Lag Example
================

``` r
# From: https://community.rstudio.com/t/dplyr-mutate-gives-na-values/19170/2
library("rquery")

dat <- wrapr::build_frame(
    "purchase_date", "product" |
    "2017-12-17"   , "apple"   |
    "2017-12-22"   , "banana"  |
    "2017-12-21"   , "banana"  |
    "2017-12-21"   , "carrot"  |
    "2017-11-29"   , "banana"  |
    "2017-12-18"   , "carrot"  |
    "2017-12-05"   , "apple"   |
    "2017-12-20"   , "banana"  |
    "2017-12-19"   , "carrot"  )

raw_connection <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                                 host = 'localhost',
                                 port = 5432,
                                 user = 'johnmount',
                                 password = '')

dbopts <- rquery::rq_connection_tests(raw_connection)
db <- rquery::rquery_db_info(connection = raw_connection,
                             is_dbi = TRUE,
                             connection_options = dbopts)

rh <- rquery::rq_copy_to(db, "rh", dat,
                         overwrite = TRUE, temporary = TRUE)


ops <- rh %.>%
  extend(., partitionby = "product", orderby = "purchase_date",
         z = LAG(purchase_date, 1, NULL)) %.>%
  orderby(., c("product", "purchase_date"))
cat(format(ops))
```

    ## table("rh"; 
    ##   purchase_date,
    ##   product) %.>%
    ##  extend(.,
    ##   z := LAG(purchase_date, 1, NULL),
    ##   p= product,
    ##   o= "purchase_date") %.>%
    ##  orderby(., product, purchase_date)

``` r
ops %.>%
  to_sql(., db) %.>%
  cat(.)
```

    ## SELECT * FROM (
    ##  SELECT
    ##   "purchase_date",
    ##   "product",
    ##   LAG ( "purchase_date" , 1 , NULL ) OVER (  PARTITION BY "product" ORDER BY "purchase_date" ) AS "z"
    ##  FROM (
    ##   SELECT
    ##    "purchase_date",
    ##    "product"
    ##   FROM
    ##    "rh"
    ##   ) tsql_53227794457970594125_0000000000
    ## ) tsql_53227794457970594125_0000000001 ORDER BY "product", "purchase_date"

``` r
DBI::dbGetQuery(raw_connection, to_sql(ops, db))
```

    ##   purchase_date product          z
    ## 1    2017-12-05   apple       <NA>
    ## 2    2017-12-17   apple 2017-12-05
    ## 3    2017-11-29  banana       <NA>
    ## 4    2017-12-20  banana 2017-11-29
    ## 5    2017-12-21  banana 2017-12-20
    ## 6    2017-12-22  banana 2017-12-21
    ## 7    2017-12-18  carrot       <NA>
    ## 8    2017-12-19  carrot 2017-12-18
    ## 9    2017-12-21  carrot 2017-12-19

``` r
DBI::dbDisconnect(raw_connection)
```

    ## [1] TRUE

`data.table` methodology here: <https://stackoverflow.com/questions/26291988/how-to-create-a-lag-variable-within-each-group>
