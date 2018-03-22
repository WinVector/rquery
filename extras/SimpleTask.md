Simple Task
================
John Mount, Win-Vector LLC
3/22/2018

Simple tasks related to [R Tip: Break up Function Nesting for Legibility](http://www.win-vector.com/blog/2018/03/r-tip-break-up-function-nesting-for-legibility/).

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
mtcarsb <- mtcars[rep(seq_len(nrow(mtcars)), 100000), ,]
print(nrow(mtcarsb))
```

    ## [1] 3200000

``` r
mtcarsd <- as.data.table(mtcarsb)

db <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')

mtcarsdb <- dbi_copy_to(db, "mtcarsdb", mtcarsb,
                        overwrite = TRUE,
                        temporary = TRUE)
# DBI::dbGetQuery(db, "CREATE INDEX mtcarsdb_cyl ON mtcarsdb (cyl)")
mtcarsdb %.>% 
  select_rows_nse(., cyl == 8) %.>% 
  select_columns(., qc(mpg, cyl, wt)) %.>%
  sql_node(., "n" := "COUNT(1)", 
           orig_columns = FALSE) %.>%
  to_sql(., db) %.>%
  DBI::dbGetQuery(db, paste("EXPLAIN", .))
```

    ##                                                                                 QUERY PLAN
    ## 1                             Finalize Aggregate  (cost=56725.58..56725.59 rows=1 width=8)
    ## 2                                     ->  Gather  (cost=56725.16..56725.57 rows=4 width=8)
    ## 3                                                                       Workers Planned: 4
    ## 4                          ->  Partial Aggregate  (cost=55725.16..55725.17 rows=1 width=8)
    ## 5               ->  Parallel Seq Scan on mtcarsdb  (cost=0.00..55715.16 rows=4000 width=0)
    ## 6                                                    Filter: (cyl = '8'::double precision)

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
  data.table_nested = {
    nrow(mtcarsd[cyl==8, c("mpg", "cyl", "wt")])
  },
  data.table_stepped = {
   mtcarsd[cyl==8,        ][,
    c("mpg", "cyl", "wt") ][, 
    .N                    ]
  },
  rquery_database = {
    res <- mtcarsdb %.>% 
      select_rows_nse(., cyl == 8) %.>% 
      select_columns(., qc(mpg, cyl, wt)) %.>%
      sql_node(., "n" := "COUNT(1)", 
               orig_columns = FALSE) %.>%
      execute(db, .)
    as.numeric(res[[1]][[1]])
  }
)

print(timings)
```

    ## Unit: milliseconds
    ##                expr       min         lq       mean     median         uq
    ##        base_stepped 954.72239 1097.44422 1242.94071 1179.27912 1300.91262
    ##         base_nested 748.62681  799.64760  929.75215  916.99677 1004.47287
    ##               dplyr  70.68412  115.19450  194.54565  132.10756  272.04900
    ##   data.table_nested  31.53406   39.06388   81.29942   45.09571   57.20796
    ##  data.table_stepped 103.57988  144.79269  246.15669  264.79787  297.08095
    ##     rquery_database 463.71270  475.78697  508.33065  480.03343  511.98086
    ##        max neval
    ##  2150.6464   100
    ##  1363.6790   100
    ##   607.8737   100
    ##   675.2933   100
    ##   646.3215   100
    ##   920.7828   100

``` r
autoplot(timings)
```

![](SimpleTask_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
DBI::dbDisconnect(db)
```

    ## [1] TRUE
