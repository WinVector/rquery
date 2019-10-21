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

rh <- rquery::rq_copy_to(db, "dat", dat,
                         overwrite = TRUE, temporary = TRUE)


ops <- rh %.>%
  extend(., partitionby = "product", orderby = "purchase_date",
         z = LAG(purchase_date, 1, NULL)) %.>%
  orderby(., c("product", "purchase_date"))
cat(format(ops))
```

    ## mk_td("dat", c(
    ##   "purchase_date",
    ##   "product")) %.>%
    ##  extend(.,
    ##   z := LAG(purchase_date, 1, NULL),
    ##   partitionby = c('product'),
    ##   orderby = c('purchase_date'),
    ##   reverse = c()) %.>%
    ##  order_rows(.,
    ##   c('product', 'purchase_date'),
    ##   reverse = c(),
    ##   limit = NULL)

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
    ##    "dat"
    ##   ) tsql_54253887843810751466_0000000000
    ## ) tsql_54253887843810751466_0000000001 ORDER BY "product", "purchase_date"

``` r
#DBI::dbGetQuery(raw_connection, to_sql(ops, db))

db %.>% ops
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
ops %.>% db
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
library("rqdatatable")
library("wrapr")



ops2 <- local_td(dat) %.>%
  orderby(., c("product", "purchase_date")) %.>%
  rqdatatable::rq_ufn(
    .,
    wrapr::srcfn(
      '.[, z := c(NA, purchase_date[-.N]), by="product"][]'),
    f_db =  function(db, incoming_table_name, outgoing_table_name, nd)  {
      if("rquery_db_info" %in% class(db)) {
        con <- db$connection
      } else {
        con <- db
      }
      DBI::dbExecute(
        con,
        paste(
          "CREATE TABLE ", outgoing_table_name, " AS ",
          "SELECT *, ",
          'LAG ( "purchase_date" , 1 , NULL ) OVER (  PARTITION BY "product" ORDER BY "purchase_date" ) AS "z"',
          " FROM ",
          incoming_table_name
        ))
    },
    columns_produced = c(colnames(dat), "z"))

as.data.table(dat) %.>% ops2
```

    ##    purchase_date product          z
    ## 1:    2017-12-05   apple       <NA>
    ## 2:    2017-12-17   apple 2017-12-05
    ## 3:    2017-11-29  banana       <NA>
    ## 4:    2017-12-20  banana 2017-11-29
    ## 5:    2017-12-21  banana 2017-12-20
    ## 6:    2017-12-22  banana 2017-12-21
    ## 7:    2017-12-18  carrot       <NA>
    ## 8:    2017-12-19  carrot 2017-12-18
    ## 9:    2017-12-21  carrot 2017-12-19

``` r
db %.>% ops2
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

`data.table` methodology from here:
<https://stackoverflow.com/questions/26291988/how-to-create-a-lag-variable-within-each-group>
