Arrow
================

``` r
library(wrapr)
library(rquery)
library(rqdatatable)

d <- data.frame(
    'g' = c(1, 2, 2, 3, 3, 3),
    'x' = c(1, 4, 5, 7, 8, 9),
    'v' = c(10, 40, 50, 70, 80, 90)
)

knitr::kable(d)
```

| g | x |  v |
| -: | -: | -: |
| 1 | 1 | 10 |
| 2 | 4 | 40 |
| 2 | 5 | 50 |
| 3 | 7 | 70 |
| 3 | 8 | 80 |
| 3 | 9 | 90 |

``` r
table_description <- local_td(d)

d['irrelevant_column'] <- 1

id_ops_a = table_description %.>%
    project(., groupby='g') %.>%
    extend(.,
        ngroup := row_number())

id_ops_b = table_description %.>%
    natural_join(., b=id_ops_a, by='g', jointype='LEFT')

a1 = arrow(id_ops_b)
print(a1)
```

    ## [c('g', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'ngroup')]

``` r
print(a1, verbose = TRUE)
```

    ## [c('g', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'ngroup')](
    ## mk_td("d", c(
    ##   "g",
    ##   "x",
    ##   "v")) %.>%
    ##  natural_join(.,
    ##   mk_td("d", c(
    ##     "g",
    ##     "x",
    ##     "v")) %.>%
    ##    project(., ,
    ##     groupby = c('g')) %.>%
    ##    extend(.,
    ##     ngroup := row_number()),
    ##   jointype = "LEFT", by = c('g'))
    ## )

``` r
shift <- data.table::shift

ordered_ops = mk_td('d2', colnames(id_ops_b)) %.>%
    extend(., 
        row_number := row_number(),
        v_shift := shift(v),
    order_by='x',
    partitionby='g')

a2 = arrow(ordered_ops)
print(a2)
```

    ## [c('g', 'x', 'v', 'ngroup') ->
    ##  c('g', 'x', 'v', 'ngroup', 'row_number', 'v_shift', 'order_by')]

``` r
print(a2, verbose = TRUE)
```

    ## [c('g', 'x', 'v', 'ngroup') ->
    ##  c('g', 'x', 'v', 'ngroup', 'row_number', 'v_shift', 'order_by')](
    ## mk_td("d2", c(
    ##   "g",
    ##   "x",
    ##   "v",
    ##   "ngroup")) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   v_shift := shift(v),
    ##   order_by := "x",
    ##   partitionby = c('g'),
    ##   orderby = c(),
    ##   reverse = c())
    ## )

``` r
unordered_ops = mk_td('d3', colnames(ordered_ops)) %.>%
    extend(.,
        size := n(),
        max_v := max(v),
        min_v := min(v),
        sum_v := sum(v),
        mean_v := mean(v),
    partitionby='g')


a3 = arrow(unordered_ops)
print(a3)
```

    ## [c('g', 'x', 'v', 'ngroup', 'row_number', 'v_shift', 'order_by') ->
    ##  c('g', 'x', 'v', 'ngroup', 'row_number', 'v_shift', 'order_by', 'size', 'max_v', 'min_v', 'sum_v', 'mean_v')]

``` r
print(a3, verbose = TRUE)
```

    ## [c('g', 'x', 'v', 'ngroup', 'row_number', 'v_shift', 'order_by') ->
    ##  c('g', 'x', 'v', 'ngroup', 'row_number', 'v_shift', 'order_by', 'size', 'max_v', 'min_v', 'sum_v', 'mean_v')](
    ## mk_td("d3", c(
    ##   "g",
    ##   "x",
    ##   "v",
    ##   "ngroup",
    ##   "row_number",
    ##   "v_shift",
    ##   "order_by")) %.>%
    ##  extend(.,
    ##   size := n(),
    ##   max_v := max(v),
    ##   min_v := min(v),
    ##   sum_v := sum(v),
    ##   mean_v := mean(v),
    ##   partitionby = c('g'),
    ##   orderby = c(),
    ##   reverse = c())
    ## )

``` r
d %.>% 
  id_ops_b %.>% 
  ordered_ops %.>% 
  unordered_ops %.>%
  knitr::kable(.)
```

| g | x |  v | ngroup | row\_number | v\_shift | order\_by | size | max\_v | min\_v | sum\_v | mean\_v |
| -: | -: | -: | -----: | ----------: | -------: | :-------- | ---: | -----: | -----: | -----: | ------: |
| 1 | 1 | 10 |      1 |           1 |       NA | x         |    1 |     10 |     10 |     10 |      10 |
| 2 | 4 | 40 |      2 |           1 |       NA | x         |    2 |     50 |     40 |     90 |      45 |
| 2 | 5 | 50 |      2 |           2 |       40 | x         |    2 |     50 |     40 |     90 |      45 |
| 3 | 7 | 70 |      3 |           1 |       NA | x         |    3 |     90 |     70 |    240 |      80 |
| 3 | 8 | 80 |      3 |           2 |       70 | x         |    3 |     90 |     70 |    240 |      80 |
| 3 | 9 | 90 |      3 |           3 |       80 | x         |    3 |     90 |     70 |    240 |      80 |

``` r
d %.>% 
  .(id_ops_b %.>% 
  ordered_ops %.>% 
  unordered_ops) %.>%
  knitr::kable(.)
```

| g |  v | x | ngroup | row\_number | v\_shift | order\_by | size | max\_v | min\_v | sum\_v | mean\_v |
| -: | -: | -: | -----: | ----------: | -------: | :-------- | ---: | -----: | -----: | -----: | ------: |
| 1 | 10 | 1 |      1 |           1 |       NA | x         |    1 |     10 |     10 |     10 |      10 |
| 2 | 40 | 4 |      2 |           1 |       NA | x         |    2 |     50 |     40 |     90 |      45 |
| 2 | 50 | 5 |      2 |           2 |       40 | x         |    2 |     50 |     40 |     90 |      45 |
| 3 | 70 | 7 |      3 |           1 |       NA | x         |    3 |     90 |     70 |    240 |      80 |
| 3 | 80 | 8 |      3 |           2 |       70 | x         |    3 |     90 |     70 |    240 |      80 |
| 3 | 90 | 9 |      3 |           3 |       80 | x         |    3 |     90 |     70 |    240 |      80 |

``` r
id_ops_b %.>% 
  ordered_ops %.>% 
  unordered_ops %.>% 
  format %.>%
  cat(.)
```

    ## mk_td("d", c(
    ##   "g",
    ##   "x",
    ##   "v")) %.>%
    ##  natural_join(.,
    ##   mk_td("d", c(
    ##     "g",
    ##     "x",
    ##     "v")) %.>%
    ##    project(., ,
    ##     groupby = c('g')) %.>%
    ##    extend(.,
    ##     ngroup := row_number()),
    ##   jointype = "LEFT", by = c('g')) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   v_shift := shift(v),
    ##   order_by := "x",
    ##   partitionby = c('g'),
    ##   orderby = c(),
    ##   reverse = c()) %.>%
    ##  extend(.,
    ##   size := n(),
    ##   max_v := max(v),
    ##   min_v := min(v),
    ##   sum_v := sum(v),
    ##   mean_v := mean(v),
    ##   partitionby = c('g'),
    ##   orderby = c(),
    ##   reverse = c())

``` r
a1 %.>% a2 %.>% a3
```

    ## [c('g', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'ngroup', 'row_number', 'v_shift', 'order_by', 'size', 'max_v', 'min_v', 'sum_v', 'mean_v')]

``` r
d %.>% a1 %.>% a2 %.>% a3 %.>% knitr::kable(.)
```

| g | x |  v | ngroup | row\_number | v\_shift | order\_by | size | max\_v | min\_v | sum\_v | mean\_v |
| -: | -: | -: | -----: | ----------: | -------: | :-------- | ---: | -----: | -----: | -----: | ------: |
| 1 | 1 | 10 |      1 |           1 |       NA | x         |    1 |     10 |     10 |     10 |      10 |
| 2 | 4 | 40 |      2 |           1 |       NA | x         |    2 |     50 |     40 |     90 |      45 |
| 2 | 5 | 50 |      2 |           2 |       40 | x         |    2 |     50 |     40 |     90 |      45 |
| 3 | 7 | 70 |      3 |           1 |       NA | x         |    3 |     90 |     70 |    240 |      80 |
| 3 | 8 | 80 |      3 |           2 |       70 | x         |    3 |     90 |     70 |    240 |      80 |
| 3 | 9 | 90 |      3 |           3 |       80 | x         |    3 |     90 |     70 |    240 |      80 |

``` r
d %.>% .(a1 %.>% a2 %.>% a3) %.>% knitr::kable(.)
```

| g |  v | x | ngroup | row\_number | v\_shift | order\_by | size | max\_v | min\_v | sum\_v | mean\_v |
| -: | -: | -: | -----: | ----------: | -------: | :-------- | ---: | -----: | -----: | -----: | ------: |
| 1 | 10 | 1 |      1 |           1 |       NA | x         |    1 |     10 |     10 |     10 |      10 |
| 2 | 40 | 4 |      2 |           1 |       NA | x         |    2 |     50 |     40 |     90 |      45 |
| 2 | 50 | 5 |      2 |           2 |       40 | x         |    2 |     50 |     40 |     90 |      45 |
| 3 | 70 | 7 |      3 |           1 |       NA | x         |    3 |     90 |     70 |    240 |      80 |
| 3 | 80 | 8 |      3 |           2 |       70 | x         |    3 |     90 |     70 |    240 |      80 |
| 3 | 90 | 9 |      3 |           3 |       80 | x         |    3 |     90 |     70 |    240 |      80 |
