dplyr modulo
================

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
postgresql_connection <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = 'localhost',
  port = 5432,
  user = 'johnmount',
  password = '')

d <- data.frame(x = -3:3)
d_db <- dplyr::copy_to(postgresql_connection, d, "d")

d %>%
  mutate(x_mod_2 = x %%2)
```

    ##    x x_mod_2
    ## 1 -3       1
    ## 2 -2       0
    ## 3 -1       1
    ## 4  0       0
    ## 5  1       1
    ## 6  2       0
    ## 7  3       1

``` r
d_db %>%
  mutate(x_mod_2 = x %%2)
```

    ## # Source:   lazy query [?? x 2]
    ## # Database: postgres 10.4.0 [johnmount@localhost:5432/johnmount]
    ##       x x_mod_2
    ##   <int>   <dbl>
    ## 1    -3      -1
    ## 2    -2       0
    ## 3    -1      -1
    ## 4     0       0
    ## 5     1       1
    ## 6     2       0
    ## 7     3       1

``` r
DBI::dbDisconnect(postgresql_connection)
```

    ## [1] TRUE
