Arrow
================

Example of data transforms as categorical arrows ([`R`
version](https://github.com/WinVector/rquery/blob/master/Examples/Arrow/Arrow.md)
[`Python`
version](https://github.com/WinVector/data_algebra/blob/master/Examples/Arrow/Arrow.ipynb)).

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

id_ops_a = table_description %.>%
    project(., groupby='g') %.>%
    extend(.,
        ngroup := row_number())

id_ops_b = table_description %.>%
    natural_join(., b=id_ops_a, by='g', jointype='LEFT') %.>%
    extend(., irrelevant_column := 1)

a1 = arrow(id_ops_b)
print(a1)
```

    ## [c('g', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'ngroup', 'irrelevant_column')]

``` r
print(a1, verbose = TRUE)
```

    ## [c('g', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'ngroup', 'irrelevant_column')](
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
    ##   irrelevant_column := 1)
    ## )

``` r
shift <- data.table::shift

ordered_ops = mk_td('d2', setdiff(colnames(id_ops_b), 'irrelevant_column')) %.>%
    extend(., 
        row_number := row_number(),
        v_shift := shift(v),
    orderby='x',
    partitionby='g')

a2 = arrow(ordered_ops)
print(a2)
```

    ## [c('g', 'x', 'v', 'ngroup') ->
    ##  c('g', 'x', 'v', 'ngroup', 'row_number', 'v_shift')]

``` r
print(a2, verbose = TRUE)
```

    ## [c('g', 'x', 'v', 'ngroup') ->
    ##  c('g', 'x', 'v', 'ngroup', 'row_number', 'v_shift')](
    ## mk_td("d2", c(
    ##   "g",
    ##   "x",
    ##   "v",
    ##   "ngroup")) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   v_shift := shift(v),
    ##   partitionby = c('g'),
    ##   orderby = c('x'),
    ##   reverse = c())
    ## )

``` r
a1 %.>% a2
```

    ## Error in compose_arrows(pipe_left_arg, pipe_right_arg, strict = TRUE): unexpected columns: irrelevant_column

``` r
ordered_ops = mk_td('d2', colnames(id_ops_b)) %.>%
    extend(., 
        row_number := row_number(),
        v_shift := shift(v),
    orderby='x',
    partitionby='g')

a2 = arrow(ordered_ops)
print(a2)
```

    ## [c('g', 'x', 'v', 'ngroup', 'irrelevant_column') ->
    ##  c('g', 'x', 'v', 'ngroup', 'irrelevant_column', 'row_number', 'v_shift')]

``` r
a1 %.>% a2
```

    ## [c('g', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'ngroup', 'irrelevant_column', 'row_number', 'v_shift')]

``` r
print(
  a1 %.>% a2,
  verbose = TRUE)
```

    ## [c('g', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'ngroup', 'irrelevant_column', 'row_number', 'v_shift')](
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
    ##   irrelevant_column := 1) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   v_shift := shift(v),
    ##   partitionby = c('g'),
    ##   orderby = c('x'),
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

    ## [c('g', 'x', 'v', 'ngroup', 'irrelevant_column', 'row_number', 'v_shift') ->
    ##  c('g', 'x', 'v', 'ngroup', 'irrelevant_column', 'row_number', 'v_shift', 'size', 'max_v', 'min_v', 'sum_v', 'mean_v')]

``` r
print(a3, verbose = TRUE)
```

    ## [c('g', 'x', 'v', 'ngroup', 'irrelevant_column', 'row_number', 'v_shift') ->
    ##  c('g', 'x', 'v', 'ngroup', 'irrelevant_column', 'row_number', 'v_shift', 'size', 'max_v', 'min_v', 'sum_v', 'mean_v')](
    ## mk_td("d3", c(
    ##   "g",
    ##   "x",
    ##   "v",
    ##   "ngroup",
    ##   "irrelevant_column",
    ##   "row_number",
    ##   "v_shift")) %.>%
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

| g | x |  v | ngroup | irrelevant\_column | row\_number | v\_shift | size | max\_v | min\_v | sum\_v | mean\_v |
| -: | -: | -: | -----: | -----------------: | ----------: | -------: | ---: | -----: | -----: | -----: | ------: |
| 1 | 1 | 10 |      1 |                  1 |           1 |       NA |    1 |     10 |     10 |     10 |      10 |
| 2 | 4 | 40 |      2 |                  1 |           1 |       NA |    2 |     50 |     40 |     90 |      45 |
| 2 | 5 | 50 |      2 |                  1 |           2 |       40 |    2 |     50 |     40 |     90 |      45 |
| 3 | 7 | 70 |      3 |                  1 |           1 |       NA |    3 |     90 |     70 |    240 |      80 |
| 3 | 8 | 80 |      3 |                  1 |           2 |       70 |    3 |     90 |     70 |    240 |      80 |
| 3 | 9 | 90 |      3 |                  1 |           3 |       80 |    3 |     90 |     70 |    240 |      80 |

``` r
d %.>% 
  .(id_ops_b %.>% 
  ordered_ops %.>% 
  unordered_ops) %.>%
  knitr::kable(.)
```

| g |  v | x | ngroup | irrelevant\_column | row\_number | v\_shift | size | max\_v | min\_v | sum\_v | mean\_v |
| -: | -: | -: | -----: | -----------------: | ----------: | -------: | ---: | -----: | -----: | -----: | ------: |
| 1 | 10 | 1 |      1 |                  1 |           1 |       NA |    1 |     10 |     10 |     10 |      10 |
| 2 | 40 | 4 |      2 |                  1 |           1 |       NA |    2 |     50 |     40 |     90 |      45 |
| 2 | 50 | 5 |      2 |                  1 |           2 |       40 |    2 |     50 |     40 |     90 |      45 |
| 3 | 70 | 7 |      3 |                  1 |           1 |       NA |    3 |     90 |     70 |    240 |      80 |
| 3 | 80 | 8 |      3 |                  1 |           2 |       70 |    3 |     90 |     70 |    240 |      80 |
| 3 | 90 | 9 |      3 |                  1 |           3 |       80 |    3 |     90 |     70 |    240 |      80 |

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
    ##   irrelevant_column := 1) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   v_shift := shift(v),
    ##   partitionby = c('g'),
    ##   orderby = c('x'),
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
    ##  c('g', 'x', 'v', 'ngroup', 'irrelevant_column', 'row_number', 'v_shift', 'size', 'max_v', 'min_v', 'sum_v', 'mean_v')]

``` r
d %.>% a1 %.>% a2 %.>% a3 %.>% knitr::kable(.)
```

| g | x |  v | ngroup | irrelevant\_column | row\_number | v\_shift | size | max\_v | min\_v | sum\_v | mean\_v |
| -: | -: | -: | -----: | -----------------: | ----------: | -------: | ---: | -----: | -----: | -----: | ------: |
| 1 | 1 | 10 |      1 |                  1 |           1 |       NA |    1 |     10 |     10 |     10 |      10 |
| 2 | 4 | 40 |      2 |                  1 |           1 |       NA |    2 |     50 |     40 |     90 |      45 |
| 2 | 5 | 50 |      2 |                  1 |           2 |       40 |    2 |     50 |     40 |     90 |      45 |
| 3 | 7 | 70 |      3 |                  1 |           1 |       NA |    3 |     90 |     70 |    240 |      80 |
| 3 | 8 | 80 |      3 |                  1 |           2 |       70 |    3 |     90 |     70 |    240 |      80 |
| 3 | 9 | 90 |      3 |                  1 |           3 |       80 |    3 |     90 |     70 |    240 |      80 |

``` r
d %.>% .(a1 %.>% a2 %.>% a3) %.>% knitr::kable(.)
```

| g |  v | x | ngroup | irrelevant\_column | row\_number | v\_shift | size | max\_v | min\_v | sum\_v | mean\_v |
| -: | -: | -: | -----: | -----------------: | ----------: | -------: | ---: | -----: | -----: | -----: | ------: |
| 1 | 10 | 1 |      1 |                  1 |           1 |       NA |    1 |     10 |     10 |     10 |      10 |
| 2 | 40 | 4 |      2 |                  1 |           1 |       NA |    2 |     50 |     40 |     90 |      45 |
| 2 | 50 | 5 |      2 |                  1 |           2 |       40 |    2 |     50 |     40 |     90 |      45 |
| 3 | 70 | 7 |      3 |                  1 |           1 |       NA |    3 |     90 |     70 |    240 |      80 |
| 3 | 80 | 8 |      3 |                  1 |           2 |       70 |    3 |     90 |     70 |    240 |      80 |
| 3 | 90 | 9 |      3 |                  1 |           3 |       80 |    3 |     90 |     70 |    240 |      80 |
