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
    ##  as.Date ( "date" )  AS "date"
    ## FROM (
    ##  SELECT
    ##   "id",
    ##   "date"
    ##  FROM
    ##   "testdate"
    ##  ) tsql_60889200356789734695_0000000000

``` r
# as.Date() not going to work without a translation

# define user specified translation
expr_map <- list("as.Date" = list( # call is 1:as.Date 2:( 3:date_col 4:)
    pre_sql_fn("to_date"),
    pre_sql_token("("),
    3,  # the date column
    pre_sql_token(","),
    pre_sql_string("YYYY-MM-DD"),
    pre_sql_token(")")
  )
)
print(expr_map)
```

    ## $as.Date
    ## $as.Date[[1]]
    ## [1] "to_date"
    ## 
    ## $as.Date[[2]]
    ## [1] "("
    ## 
    ## $as.Date[[3]]
    ## [1] 3
    ## 
    ## $as.Date[[4]]
    ## [1] ","
    ## 
    ## $as.Date[[5]]
    ## [1] "'YYYY-MM-DD'"
    ## 
    ## $as.Date[[6]]
    ## [1] ")"

``` r
db$tree_rewriter <- function(x, db_info) {
  if("pre_sql_sub_expr" %in% class(x)) {
    # first recurse
    for(i in seq_len(length(x$toks))) {
      x$toks[[i]] <- Recall(x$toks[[i]], db_info)
    }
    # now look for special cases
    if(("pre_sql_token" %in% class(x$toks[[1]])) &&
       (x$toks[[1]]$token_type == "function_name")) {
      key <- x$toks[[1]][["value"]]
      replacement <- expr_map[[key]]
      if(!is.null(replacement)) {
        x_translated <- x
        x_translated$toks <- replacement
        for(i in seq_len(length(replacement))) {
          if(is.numeric(replacement[[i]])) {
            x_translated$toks[[i]] <- x$toks[[replacement[[i]]]]
          }
        }
        return(x_translated)
      }
    }
  }
  x
}


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
    ##  ) tsql_37167747422159635647_0000000000

``` r
execute(db, ops)  %.>%
  str(.)
```

    ## 'data.frame':    3 obs. of  2 variables:
    ##  $ id  : chr  "a" "b" "c"
    ##  $ date: Date, format: "2019-01-12" "2019-02-21" ...

``` r
DBI::dbDisconnect(raw_connection)
```
