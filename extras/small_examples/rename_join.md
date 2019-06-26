rename join example
================

Example of automatic insertion of a rename node on a join.

``` r
library("rquery")
library("wrapr")

raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
RSQLite::initExtension(raw_connection)
db <- rquery_db_info(
  connection = raw_connection,
  is_dbi = TRUE,
  connection_options = rq_connection_tests(raw_connection))

t1l <- data.frame(a = c(1, 2), b = c(3, 4))
t1r <- rq_copy_to(db, "t1r", t1l)
t2l <- data.frame(c = c(2, 1), d = c(6, 5))
t2r <- rq_copy_to(db, "t2r", t2l)

ops <- natural_join(t1r, t2r, by = c("a" = "c"))

cat(format(ops))
```

    ## table(`t1r`; 
    ##   a,
    ##   b) %.>%
    ##  natural_join(.,
    ##   table(`t2r`; 
    ##     c,
    ##     d) %.>%
    ##    rename(.,
    ##     c('a' = 'c')),
    ##   j= INNER, by= a)

``` r
# cat(to_sql(ops, db))

execute(db, ops) %.>%
  knitr::kable(.)
```

| a | b | d |
| -: | -: | -: |
| 1 | 3 | 5 |
| 2 | 4 | 6 |

``` r
DBI::dbDisconnect(raw_connection)

library("rqdatatable")

natural_join(t1l, t2l, by = c("a" = "c"))
```

    ##    a b d
    ## 1: 1 3 5
    ## 2: 2 4 6

``` r
ex_data_table(ops,
              tables = list("t1r" = t1l, "t2r" = t2l)) %.>%
  knitr::kable(.)
```

| a | b | d |
| -: | -: | -: |
| 1 | 3 | 5 |
| 2 | 4 | 6 |

``` r
list("t1r" = t1l, "t2r" = t2l) %.>% 
  ops %.>%
  knitr::kable(.)
```

| a | b | d |
| -: | -: | -: |
| 1 | 3 | 5 |
| 2 | 4 | 6 |
