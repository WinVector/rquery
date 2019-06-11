Expression Factoring
================

A quick example of expression factoring in `dplyr` and `rquery`.

We set up our local and remote tables.

``` r
db <- DBI::dbConnect(RSQLite::SQLite(), 
                     ":memory:")

d_local <- data.frame(x = 1)

DBI::dbWriteTable(db, "d_remote", d_local)
```

We try the same calculation with `dplyr` in `R` and in the database.

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
packageVersion("dplyr")
```

    ## [1] '0.8.1'

``` r
packageVersion("dbplyr")
```

    ## [1] '1.4.1'

``` r
d_local %>% 
  mutate(y = 1, y = y + 1, y = y + 1)
```

    ##   x y
    ## 1 1 3

``` r
dplyr_remote <- dplyr::tbl(db, "d_remote")

dplyr_remote %>% 
  mutate(y = 1, y = y + 1, y = y + 1)
```

    ## # Source:   lazy query [?? x 2]
    ## # Database: sqlite 3.22.0 [:memory:]
    ##       x     y
    ##   <dbl> <dbl>
    ## 1     1     2

``` r
# notice y does not equal 3 in this case
```

We try the same calculation with `rquery` in `R` and in the database.

``` r
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
library("rquery")

d_local %.>% 
  extend(., 
         y = 1, y = y + 1, y = y + 1)
```

    ##    x y
    ## 1: 1 3

``` r
rquery_remote <- rquery::db_td(db, "d_remote")

rquery_remote %.>% 
  extend(., 
         y = 1, y = y + 1, y = y + 1) %.>%
  execute(db, .)
```

    ##   x y
    ## 1 1 3

And we clean up after.

``` r
DBI::dbDisconnect(db)
```
