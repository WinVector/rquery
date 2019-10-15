JoinArrow
================

Converting a join to a an arrow ([`R`
version](https://github.com/WinVector/rquery/blob/master/Examples/Arrow/JoinArrow.md),
[`Python`
version](https://github.com/WinVector/data_algebra/blob/master/Examples/Arrow/JoinArrow.md)).

``` r
library(wrapr)
library(rquery)
library(rqdatatable)
```

``` r
d1 <- wrapr::build_frame(
   "key"  , "x" |
     "a"  , 1   |
     "b"  , 2   )

knitr::kable(d1)
```

| key | x |
| :-- | -: |
| a   | 1 |
| b   | 2 |

``` r
table_1_description <- local_td(d1, name='d1')

table_1_description
```

    ## [1] "mk_td(\"d1\", c( \"key\", \"x\"))"

``` r
d2 <- wrapr::build_frame(
   "key"  , "y" |
     "b"  , 3   |
     "c"  , 4   )

knitr::kable(d2)
```

| key | y |
| :-- | -: |
| b   | 3 |
| c   | 4 |

``` r
table_2_description <- local_td(d2, name='d2')

table_2_description
```

    ## [1] "mk_td(\"d2\", c( \"key\", \"y\"))"

``` r
ops <- table_1_description %.>%
    natural_join(.,
        b=table_2_description, 
        by='key',
        jointype='FULL')
```

``` r
arrow_1 = arrow(ops, free_table_key = table_1_description$table_name)

print(arrow_1)
```

    ## [c('key', 'x') ->
    ##  c('key', 'x', 'y')]

``` r
arrow_2 = arrow(ops, free_table_key = table_2_description$table_name)

print(arrow_2)
```

    ## [c('key', 'y') ->
    ##  c('key', 'x', 'y')]

``` r
res <- ex_data_table(
  arrow_1$pipeline,
  tables = list(
    d1 = d1,
    d2 = d2))

knitr::kable(res)
```

| key |  x |  y |
| :-- | -: | -: |
| a   |  1 | NA |
| b   |  2 |  3 |
| c   | NA |  4 |
