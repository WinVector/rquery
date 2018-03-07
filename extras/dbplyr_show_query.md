dbply\_query
================
Win-Vector LLC
3/7/2018

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
my_db <- DBI::dbConnect(RSQLite::SQLite(),
                        ":memory:")
dLocal <- data.frame(x = c(1,2))
dRemote <- dplyr::copy_to(my_db, dLocal, "dRemote")


dRemote %>% mutate(x = x + 1) %>% compute() %>% show_query()
```

    ## <SQL>
    ## SELECT *
    ## FROM `lzjauewxal`

``` r
dRemote %>% mutate(x = x + 1) %>% compute() %>% dbplyr::remote_query()
```

    ## <SQL> SELECT *
    ## FROM `gnslkbvyse`

``` r
DBI::dbDisconnect(my_db)
```
