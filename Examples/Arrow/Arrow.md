Arrow
================

Example of data transforms as categorical arrows ([`R`
version](https://github.com/WinVector/rquery/blob/master/Examples/Arrow/Arrow.md)
[`Python`
version](https://github.com/WinVector/rquery/blob/master/Examples/Arrow/Arrow.md)).

(For ideas on applying category theory to science and data, please see
David I Spivak, *Category Theory for the Sciences*, MIT Press, 2014.)

The [R `rquery` package](https://github.com/WinVector/rquery/) supplies
a number of operators for working with tabular data. The operators are
picked in reference to [Codd’s relational
algebra](https://en.wikipedia.org/wiki/Relational_algebra), though (as
with [`SQL`](https://en.wikipedia.org/wiki/SQL)) we do not insist on
table rows being unique. Many of the operations are simple: selecting
rows, selecting columns, joining tables. Two of the operations stand
out: projecting or aggregating rows, and extending tables with new
derived columns.

An interesting point is: while the `rquery` operators are fairly
generic: the operator pipelines that map a single table to a single
table form a category over a nice set of objects.

The objects of this category can be either of:

  - Sets of column names.
  - Maps of column names to column types (schema-like objects).

I will take a liberty and call these objects (with or without types)
“single table schemas.”

Our setup is easiest to explain with an example. Let’s work an example
in `Python`.

First we import our packages and instantiate an example data frame.

``` r
library(wrapr)
library(rquery)
library(rqdatatable)

d <- data.frame(
    'g' = c('a', 'b', 'b', 'c', 'c', 'c'),
    'x' = c(1, 4, 5, 7, 8, 9),
    'v' = c(10, 40, 50, 70, 80, 90),
    'i' = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
)

knitr::kable(d)
```

| g | x |  v | i     |
| :- | -: | -: | :---- |
| a | 1 | 10 | TRUE  |
| b | 4 | 40 | TRUE  |
| b | 5 | 50 | FALSE |
| c | 7 | 70 | FALSE |
| c | 8 | 80 | FALSE |
| c | 9 | 90 | FALSE |

`rquery` operator pipelines are designed to transform data. For example
we can define the following operator pipeline which is designed count
how many different values there are for `g`, and assign a unique integer
id to each group.

``` r
table_description <- local_td(d)

id_ops_a = table_description %.>%
    project(., groupby='g') %.>%
    extend(.,
        ngroup := row_number())
```

The pipeline is saved in the variable `id_ops_a` which can then be
applied to our data as follows.

``` r
d %.>% 
  id_ops_a %.>% 
  knitr::kable(.)
```

| g | ngroup |
| :- | -----: |
| a |      1 |
| b |      2 |
| c |      3 |

The pipelines are designed for composition in addition to application to
data. For example we can use the `id_ops_a` pipeline as part of a larger
pipeline as follows.

``` r
id_ops_b = table_description %.>%
    natural_join(., b=id_ops_a, by='g', jointype='LEFT')
```

This pipeline specifies joining the integer group ids back into the
original table as follows.

``` r
d %.>% 
  id_ops_b %.>% 
  knitr::kable(.)
```

| g | i     |  v | x | ngroup |
| :- | :---- | -: | -: | -----: |
| a | TRUE  | 10 | 1 |      1 |
| b | TRUE  | 40 | 4 |      2 |
| b | FALSE | 50 | 5 |      2 |
| c | FALSE | 70 | 7 |      3 |
| c | FALSE | 80 | 8 |      3 |
| c | FALSE | 90 | 9 |      3 |

Notice the `ngroup` column is a function of the `g` column in this
result.

I am now ready to state my big point. These pipelines have documented
pre and post conditions: what set of columns (and optionally types) they
expect on their input, and what set of columns (optionally types) the
pipeline produces.

``` r
# needs
columns_used(id_ops_b)
```

    ## $d
    ## [1] "g" "i" "v" "x"

``` r
# produces
column_names(id_ops_b)
```

    ## [1] "g"      "x"      "v"      "i"      "ngroup"

This is where we seem to have nice opportunity to use category theory to
manage our pre-and post conditions. Let’s wrap this pipeline into a
convenience class to make the categorical connection easier to see.

``` r
a1 = arrow(id_ops_b)
print(a1)
```

    ## [c('g', 'i', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup')]

`a1` is a categorical theory arrow, it has the usual domain (arrow base,
or incoming object), and co-domain (arrow head, or outgoing object) in a
category of single-table schemas.

``` r
# dom or domain
a1$incoming_columns
```

    ## [1] "g" "i" "v" "x"

``` r
# cod or co-domain
a1$outgoing_columns
```

    ## [1] "g"      "x"      "v"      "i"      "ngroup"

These are what are presented in the succinct presentation of the arrow.

``` r
print(a1)
```

    ## [c('g', 'i', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup')]

The arrow has a more detailed presentation, which is the realization of
the operator pipeline.

``` r
print(a1, verbose = TRUE)
```

    ## [c('g', 'i', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup')](
    ## mk_td("d", c(
    ##   "g",
    ##   "x",
    ##   "v",
    ##   "i")) %.>%
    ##  natural_join(.,
    ##   mk_td("d", c(
    ##     "g",
    ##     "x",
    ##     "v",
    ##     "i")) %.>%
    ##    project(., ,
    ##     groupby = c('g')) %.>%
    ##    extend(.,
    ##     ngroup := row_number()),
    ##   jointype = "LEFT", by = c('g'))
    ## )

Arrows can be composed or applied by using the notation `d %.>% a1`.
Note: we are not thinking of `%.>%` itself as an arrow, but as a symbol
for composition of arrows.

``` r
d %.>%
  a1 %.>%
  knitr::kable(.)
```

| g | i     |  v | x | ngroup |
| :- | :---- | -: | -: | -----: |
| a | TRUE  | 10 | 1 |      1 |
| b | TRUE  | 40 | 4 |      2 |
| b | FALSE | 50 | 5 |      2 |
| c | FALSE | 70 | 7 |      3 |
| c | FALSE | 80 | 8 |      3 |
| c | FALSE | 90 | 9 |      3 |

Up until now we have been showing how we work to obey the category
theory axioms. From here on we look at what does category theory do for
us. What it does is check correct composition and ensure full
associativity of operations.

As is typical in category theory, there can be more than one arrow from
a given object to given object. For example the following is a different
arrow with the same start and end.

``` r
a1b <- arrow(
  table_description %.>%
    extend(.,
           ngroup := 0)
)

print(a1b)
```

    ## [c('g', 'x', 'v', 'i') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup')]

However, the `a1b` arrow represents a different operation:

``` r
d %.>%
  a1b %.>%
  knitr::kable(.)
```

| g | x |  v | i     | ngroup |
| :- | -: | -: | :---- | -----: |
| a | 1 | 10 | TRUE  |      0 |
| b | 4 | 40 | TRUE  |      0 |
| b | 5 | 50 | FALSE |      0 |
| c | 7 | 70 | FALSE |      0 |
| c | 8 | 80 | FALSE |      0 |
| c | 9 | 90 | FALSE |      0 |

``` r
print(a1, verbose = TRUE)
```

    ## [c('g', 'i', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup')](
    ## mk_td("d", c(
    ##   "g",
    ##   "x",
    ##   "v",
    ##   "i")) %.>%
    ##  natural_join(.,
    ##   mk_td("d", c(
    ##     "g",
    ##     "x",
    ##     "v",
    ##     "i")) %.>%
    ##    project(., ,
    ##     groupby = c('g')) %.>%
    ##    extend(.,
    ##     ngroup := row_number()),
    ##   jointype = "LEFT", by = c('g'))
    ## )

The arrows compose exactly when the pre-conditions meet the post
conditions.

Here are two examples of violating the pre and post conditions. The
point is, the categorical conditions enforce the checking for us. We
can’t compose arrows that don’t match domain and range. Up until now
we have been setting things up to make the categorical machinery work,
now this machinery will work for us and make the job of managing complex
data transformations easier.

``` r
shift <- data.table::shift

# too small
ordered_ops = mk_td('d2', c("g", "x", "v", "ngroup")) %.>%
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
a1 %.>% a2
```

    ## Error in compose_arrows(pipe_left_arg, pipe_right_arg, strict = TRUE): unexpected columns: i

``` r
# too big
ordered_ops = mk_td('d2', c("g", "x", "v", "i", "ngroup", "q")) %.>%
    extend(., 
        row_number := row_number(),
        v_shift := shift(v),
    orderby='x',
    partitionby='g')

a2 = arrow(ordered_ops)
print(a2)
```

    ## [c('g', 'x', 'v', 'i', 'ngroup', 'q') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup', 'q', 'row_number', 'v_shift')]

``` r
a1 %.>% a2
```

    ## Error in compose_arrows(pipe_left_arg, pipe_right_arg, strict = TRUE): missing required columns: c('q')

The point is: we will never see the above exceptions when we compose
arrows that match on pre and post conditions (which in category theory
are the only arrows you are allowed to compose).

When the pre and post conditions are met the arrows compose in a fully
associative manner.

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

    ## [c('g', 'x', 'v', 'i', 'ngroup') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup', 'row_number', 'v_shift')]

``` r
a1 %.>% a2
```

    ## [c('g', 'i', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup', 'row_number', 'v_shift')]

``` r
print(
  a1 %.>% a2,
  verbose = TRUE)
```

    ## [c('g', 'i', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup', 'row_number', 'v_shift')](
    ## mk_td("d", c(
    ##   "g",
    ##   "x",
    ##   "v",
    ##   "i")) %.>%
    ##  natural_join(.,
    ##   mk_td("d", c(
    ##     "g",
    ##     "x",
    ##     "v",
    ##     "i")) %.>%
    ##    project(., ,
    ##     groupby = c('g')) %.>%
    ##    extend(.,
    ##     ngroup := row_number()),
    ##   jointype = "LEFT", by = c('g')) %.>%
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
        mean_v := mean(v),
    partitionby='g')


a3 = arrow(unordered_ops)
print(a3)
```

    ## [c('g', 'x', 'v', 'i', 'ngroup', 'row_number', 'v_shift') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup', 'row_number', 'v_shift', 'mean_v')]

``` r
a1 %.>% a2 %.>% a3
```

    ## [c('g', 'i', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup', 'row_number', 'v_shift', 'mean_v')]

``` r
ops1 <- (a1 %.>% a2) %.>% a3
ops1
```

    ## [c('g', 'i', 'v', 'x') ->
    ##  c('g', 'x', 'v', 'i', 'ngroup', 'row_number', 'v_shift', 'mean_v')]

``` r
ops2 <- a1 %.>% .(a2 %.>% a3)
```

All three compositions are in fact the same arrow.

``` r
d %.>%
  ops1 %.>% 
  knitr::kable(.)
```

| g | i     |  v | x | ngroup | row\_number | v\_shift | mean\_v |
| :- | :---- | -: | -: | -----: | ----------: | -------: | ------: |
| a | TRUE  | 10 | 1 |      1 |           1 |       NA |      10 |
| b | TRUE  | 40 | 4 |      2 |           1 |       NA |      45 |
| b | FALSE | 50 | 5 |      2 |           2 |       40 |      45 |
| c | FALSE | 70 | 7 |      3 |           1 |       NA |      80 |
| c | FALSE | 80 | 8 |      3 |           2 |       70 |      80 |
| c | FALSE | 90 | 9 |      3 |           3 |       80 |      80 |

``` r
d %.>%
  ops2 %.>% 
  knitr::kable(.)
```

| g | i     |  v | x | ngroup | row\_number | v\_shift | mean\_v |
| :- | :---- | -: | -: | -----: | ----------: | -------: | ------: |
| a | TRUE  | 10 | 1 |      1 |           1 |       NA |      10 |
| b | TRUE  | 40 | 4 |      2 |           1 |       NA |      45 |
| b | FALSE | 50 | 5 |      2 |           2 |       40 |      45 |
| c | FALSE | 70 | 7 |      3 |           1 |       NA |      80 |
| c | FALSE | 80 | 8 |      3 |           2 |       70 |      80 |
| c | FALSE | 90 | 9 |      3 |           3 |       80 |      80 |

The combination operator `%.>%` is fully associative over the
combination of data and arrows.

The underlying `rquery` steps compute and check very similar pre and
post conditions, the arrow class is just making this look more
explicitly like arrows moving through objects in category.

The data arrows operate over three different value domains:

  - their own arrow space (i.e. composition)
  - data frames (transforming data)
  - single table schemas (transforming single table schemas)

I think we have that if we consider the arrows operating on data and the
arrows operating on single table schemas we have a faithful embedding
(in the sense of Saunders Mac Lane *Categories for the Working
Mathematician, 2nd Edition*, Springer, 1997, page 15) from data to
single table schemas.
