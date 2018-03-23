Simple Task
================
John Mount, Win-Vector LLC
3/22/2018

Simple tasks related to [R Tip: Break up Function Nesting for Legibility](http://www.win-vector.com/blog/2018/03/r-tip-break-up-function-nesting-for-legibility/). Most remote data systems start and end with data remote, so we are materializing tables when showing database timings.

``` r
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("microbenchmark")
library("ggplot2")
library("data.table")
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
library("rquery")
```

    ## Loading required package: wrapr

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:data.table':
    ## 
    ##     :=

``` r
# PostgreSQL dbExistsTable() does not work
options(list(rquery.use_DBI_dbExistsTable = FALSE))

db <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')

nrows <- 100000
```

Simple problem (subset rows and columns).

``` r
mtcarsb <- mtcars[rep(seq_len(nrow(mtcars)), nrows), ,]
print(dim(mtcarsb))
```

    ## [1] 3200000      11

``` r
mtcarsd <- as.data.table(mtcarsb)

mtcarsdb <- dbi_copy_to(db, "mtcarsdb", mtcarsb,
                        overwrite = TRUE,
                        temporary = TRUE)
# DBI::dbGetQuery(db, "CREATE INDEX mtcarsdb_cyl ON mtcarsdb (cyl)")
rquery_sql <- mtcarsdb %.>% 
  select_rows_nse(., cyl == 8) %.>% 
  select_columns(., qc(mpg, cyl, wt)) %.>%
  to_sql(., db)

cat(rquery_sql)
```

    ## SELECT
    ##  "mpg",
    ##  "cyl",
    ##  "wt"
    ## FROM (
    ##  SELECT * FROM (
    ##   SELECT
    ##    "mtcarsdb"."mpg",
    ##    "mtcarsdb"."cyl",
    ##    "mtcarsdb"."wt"
    ##   FROM
    ##    "mtcarsdb"
    ##  ) tsql_93912208524734195079_0000000000
    ##  WHERE "cyl" = 8
    ## ) tsql_93912208524734195079_0000000001

``` r
DBI::dbGetQuery(db, paste("EXPLAIN", rquery_sql))
```

    ##                                                                      QUERY PLAN
    ## 1                          Gather  (cost=1000.00..58315.16 rows=16000 width=24)
    ## 2                                                            Workers Planned: 4
    ## 3   ->  Parallel Seq Scan on mtcarsdb  (cost=0.00..55715.16 rows=4000 width=24)
    ## 4                                         Filter: (cyl = '8'::double precision)

``` r
mtcarst <- dplyr::tbl(db, "mtcarsdb")

dplyr_sql <- mtcarst %>%
  filter(cyl == 8) %>%
  select(mpg, cyl, wt) %>%
  dbplyr::remote_query(.)

cat(dplyr_sql)
```

    ## SELECT "mpg", "cyl", "wt"
    ## FROM "mtcarsdb"
    ## WHERE ("cyl" = 8.0)

``` r
DBI::dbGetQuery(db, paste("EXPLAIN", dplyr_sql))
```

    ##                                                                      QUERY PLAN
    ## 1                          Gather  (cost=1000.00..58315.16 rows=16000 width=24)
    ## 2                                                            Workers Planned: 4
    ## 3   ->  Parallel Seq Scan on mtcarsdb  (cost=0.00..55715.16 rows=4000 width=24)
    ## 4                                         Filter: (cyl = '8'::double precision)

``` r
timings <- microbenchmark(
  base_stepped = {
    . <- mtcarsb
    . <- subset(., cyl == 8)
    . <- .[, c("mpg", "cyl", "wt")]
    nrow(.)
  },
  base_nested = {
    nrow(mtcarsb[mtcarsb$cyl == 8, c("mpg", "cyl", "wt")])
  },
  dplyr = {
    mtcarsb                %>%
      filter(cyl == 8)     %>%
      select(mpg, cyl, wt) %>%
      nrow
  },
  dplyr_database = {
    res <- mtcarst         %>%
      filter(cyl == 8)     %>%
      select(mpg, cyl, wt) %>%
      compute()
    as.numeric(as.data.frame(tally(res))[[1]][[1]])
  },
  data.table_nested = {
    nrow(mtcarsd[cyl==8, c("mpg", "cyl", "wt")])
  },
  data.table_stepped = {
   mtcarsd[cyl==8,        ][,
    c("mpg", "cyl", "wt") ][, 
    .N                    ]
  },
  rquery_database = {
    res <- mtcarsdb                       %.>% 
      select_rows_nse(., cyl == 8)        %.>% 
      select_columns(., qc(mpg, cyl, wt)) %.>%
      materialize(db, ., 
                  table_name = "restab",
                  overwrite = TRUE,
                  temporary = TRUE)
     dbi_nrow(db, res$table_name)
  }
)

print(timings)
```

    ## Unit: milliseconds
    ##                expr        min         lq      mean     median         uq
    ##        base_stepped  390.82470  535.07365  621.2703  573.94872  660.98708
    ##         base_nested  189.88128  220.92643  308.1530  238.72063  390.38276
    ##               dplyr   69.98686  118.14594  190.5346  129.23745  249.22599
    ##      dplyr_database 1494.24581 1563.01820 2030.9475 1727.84260 2067.37017
    ##   data.table_nested   31.68172   40.63368   74.8987   50.00722   58.15187
    ##  data.table_stepped   95.33918  145.20882  223.9996  204.64586  286.91458
    ##     rquery_database 1216.06152 1437.59025 1980.8697 1655.72319 2595.96311
    ##        max neval
    ##  2060.7125   100
    ##   742.4877   100
    ##   651.8981   100
    ##  4441.4148   100
    ##   506.7049   100
    ##   731.2549   100
    ##  3774.4753   100

``` r
autoplot(timings)
```

![](SimpleTask_files/figure-markdown_github/unnamed-chunk-2-1.png)

Want to add the query to query folding feature that `dbplyr`'s optimizer has.

``` r
DBI::dbDisconnect(db)
```

    ## [1] TRUE
