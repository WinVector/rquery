rquery Modes
================

[`rqdatatable`](https://github.com/WinVector/rqdatatable)/[`rquery`](https://github.com/WinVector/rquery)
is designed to have a number of different modes of use. The primary
intended one the considered mode of building up a pipelines from a
description of the tables to be acted on.

As our example, lets start with the following example data.

``` r
d <- data.frame(
  x = c(1, 2, 3, 4, 5, 6),
  y = c(2, 2, 2, 3, 7, 10),
  g = c('a', 'a', 'a', 'b', 'b' ,'b'),
  stringsAsFactors = FALSE
)

knitr::kable(d)
```

| x |  y | g |
| -: | -: | :- |
| 1 |  2 | a |
| 2 |  2 | a |
| 3 |  2 | a |
| 4 |  3 | b |
| 5 |  7 | b |
| 6 | 10 | b |

For our task: let’s find a row with the largest ratio of ‘y’ to ‘x’, per
group ‘g’.

The `rquery` concept is to break this into small sub-goals and steps:

  - Find the ratio of ‘y’ to ‘x’.
  - Rank the rows by this ratio.
  - Mark our chosen rows.

In the standard `rquery` practice we build up our processing pipeline to
follow our above plan. The translation involves some familiarity with
the `rquery` steps, including the row-numbering command
[`row_number()`](https://github.com/WinVector/rquery/blob/master/Examples/WindowFunctions/WindowFunctions.md).

``` r
library(rquery)

ops <- local_td(d) %.>%  # Describe table for later operations
  extend(.,       # add a new column
         ratio := y / x) %.>%
  extend(.,       # rank the rows by group and order
         simple_rank := row_number(),
         partitionby = 'g',
         orderby = 'ratio',
         reverse = 'ratio') %.>%
  extend(.,       # mark the rows we want
         choice := simple_rank == 1)
```

The `ops` operator pipeline can than be used to process data.

``` r
d %.>%
  ops %.>%
  knitr::kable(.)
```

| x |  y | g |     ratio | simple\_rank | choice |
| -: | -: | :- | --------: | -----------: | :----- |
| 1 |  2 | a | 2.0000000 |            1 | TRUE   |
| 2 |  2 | a | 1.0000000 |            2 | FALSE  |
| 3 |  2 | a | 0.6666667 |            3 | FALSE  |
| 6 | 10 | b | 1.6666667 |            1 | TRUE   |
| 5 |  7 | b | 1.4000000 |            2 | FALSE  |
| 4 |  3 | b | 0.7500000 |            3 | FALSE  |

Another point is: this form documents check-able (and enforceable) pre
and post conditions on the calculation. For example such a calculation
documents what columns are required by the calculation, and which ones
are produced.

``` r
# columns produced
column_names(ops)
```

    ## [1] "x"           "y"           "g"           "ratio"       "simple_rank"
    ## [6] "choice"

``` r
# columns used
columns_used(ops)
```

    ## $d
    ## [1] "x" "y" "g"

We can in fact make these conditions the explicit basis of [an
interpretation of these data transforms as category theory
arrows](https://github.com/WinVector/rquery/blob/master/Examples/Arrow/Arrow.md).

``` r
arrow(ops)
```

    ## [
    ##  'd':
    ##  c('x', 'y', 'g')
    ##    ->
    ##  c('x', 'y', 'g', 'ratio', 'simple_rank', 'choice')
    ## ]

Another way to use `rquery`/`rqdatatable` is in “immediate mode”, where
we send the data from pipeline stage to pipeline state.

``` r
d %.>%
  extend(.,       # add a new column
         ratio := y / x) %.>%
  extend(.,       # rank the rows by group and order
         simple_rank := row_number(),
         partitionby = 'g',
         orderby = 'ratio',
         reverse = 'ratio') %.>%
  extend(.,       # mark the rows we want
         choice := simple_rank == 1) %.>%
  knitr::kable(.)
```

| x |  y | g |     ratio | simple\_rank | choice |
| -: | -: | :- | --------: | -----------: | :----- |
| 1 |  2 | a | 2.0000000 |            1 | TRUE   |
| 2 |  2 | a | 1.0000000 |            2 | FALSE  |
| 3 |  2 | a | 0.6666667 |            3 | FALSE  |
| 6 | 10 | b | 1.6666667 |            1 | TRUE   |
| 5 |  7 | b | 1.4000000 |            2 | FALSE  |
| 4 |  3 | b | 0.7500000 |            3 | FALSE  |

Immediate mode skips the `local_td()` step of building a specification
for the data to later come, and runs directly on the data. The advantage
of this mode is: it is quick for the user. A disadvantage of this mode
is: the pipeline is not left for re-use, and there are possibly
expensive data conversions (from `data.frame` to `data.table`) at each
operator stage.

``` r
d %.>%
  wrap %.>%       # wrap data in a description
  extend(.,       # add a new column
         ratio := y / x) %.>%
  extend(.,       # rank the rows by group and order
         simple_rank := row_number(),
         partitionby = 'g',
         orderby = 'ratio',
         reverse = 'ratio') %.>%
  extend(.,       # mark the rows we want
         choice := simple_rank == 1) %.>%
  ex %.>%         # signal construction done, and execute
  knitr::kable(.)
```

| x |  y | g |     ratio | simple\_rank | choice |
| -: | -: | :- | --------: | -----------: | :----- |
| 1 |  2 | a | 2.0000000 |            1 | TRUE   |
| 2 |  2 | a | 1.0000000 |            2 | FALSE  |
| 3 |  2 | a | 0.6666667 |            3 | FALSE  |
| 6 | 10 | b | 1.6666667 |            1 | TRUE   |
| 5 |  7 | b | 1.4000000 |            2 | FALSE  |
| 4 |  3 | b | 0.7500000 |            3 | FALSE  |

The difference is: we use the `wrap` to build a special operator
collecting (and checking) pipeline, and then later `ex` to say we are
done specifying steps and to apply the operations to the data. Prior to
the `ex` step the operator pipeline is available as a field called
`underlying` and the set of wrapped `data.frame`s is available as a
field called `data_map`.

The wrapping of data as a different kind of `rquery` pipeline is an
example of using the [“decorator
pattern”](https://en.wikipedia.org/wiki/Decorator_pattern) (which can
be considered as an object oriented variation of the functional monad
pattern,
[ref](https://en.wikipedia.org/wiki/Monad_\(functional_programming\))).
However, these are technical considerations that are the package
developer’s problem- not problems for the package users. Think of these
terms as examples of things developers worry about so users don’t have
to worry about them.

`rquery` operator are designed to check pre and post-conditions early.
This can be specialized into [interpreting `rquery` pipelines as
category theory
arrows](https://github.com/WinVector/rquery/blob/master/Examples/Arrow/Arrow.md).
More commonly a less controlled form of operator abstraction/composition
based on
[lambda-abstraction](https://en.wikipedia.org/wiki/Lambda_calculus),
[lazy evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation) is used
in `R` packages.

That would look something like the following.

``` r
# collect operatios as a function
f <- wrapr::locum() %.>% # locum stands in for data to to supplied later
  extend(.,       # add a new column
         ratio := y / x) %.>%
  extend(.,       # rank the rows by group and order
         simple_rank := row_number(),
         partitionby = 'g',
         orderby = 'ratio',
         reverse = 'ratio') %.>%
  extend(.,       # mark the rows we want
         choice := simple_rank == 1) 

# show what we have
cat(format(f))
```

    ## locum() %.>%
    ##    extend(., `:=`(ratio, y/x)) %.>%
    ##    extend(., `:=`(simple_rank, row_number()), partitionby = "g", 
    ##      orderby = "ratio", reverse = "ratio") %.>%
    ##    extend(., `:=`(choice, simple_rank == 1))

``` r
# use the operations on data
d %.>%
  f %.>%
  knitr::kable(.)
```

| x |  y | g |     ratio | simple\_rank | choice |
| -: | -: | :- | --------: | -----------: | :----- |
| 1 |  2 | a | 2.0000000 |            1 | TRUE   |
| 2 |  2 | a | 1.0000000 |            2 | FALSE  |
| 3 |  2 | a | 0.6666667 |            3 | FALSE  |
| 6 | 10 | b | 1.6666667 |            1 | TRUE   |
| 5 |  7 | b | 1.4000000 |            2 | FALSE  |
| 4 |  3 | b | 0.7500000 |            3 | FALSE  |

The above works. However it isn’t desirable when we have other tools
available, as very little is checked early. Our theory is:
lambda-abstraction/lazy-evaluation tools are common as they are less
work for package developers. That is unfortunate as such abstractions do
a lot less for the end-user.

``` r
set.seed(2019)
n_rows <- 1000000
d_large <- data.frame(
  x = rnorm(n = n_rows),
  y = rnorm(n = n_rows),
  g = sample(paste0('v_', seq_len(n_rows/10)), 
             size = n_rows, 
             replace = TRUE),
  stringsAsFactors = FALSE
)

f_compiled <- function(dat) {
  dat %.>% ops  # use pre-compiled pipeline
}

f_immediate <- function(dat) {
  dat %.>%
    extend(.,       # add a new column
           ratio := y / x) %.>%
    extend(.,       # rank the rows by group and order
           simple_rank := row_number(),
           partitionby = 'g',
           orderby = 'ratio',
           reverse = 'ratio') %.>%
    extend(.,       # mark the rows we want
           choice := simple_rank == 1)
}

f_wrapped <- function(dat) {
  dat %.>%
    wrap %.>%       # wrap data in a description
    extend(.,       # add a new column
           ratio := y / x) %.>%
    extend(.,       # rank the rows by group and order
           simple_rank := row_number(),
           partitionby = 'g',
           orderby = 'ratio',
           reverse = 'ratio') %.>%
    extend(.,       # mark the rows we want
           choice := simple_rank == 1) %.>%
    ex              # signal construction done, and execute
}
```

And we can also time `data.table` itself (without the translation
overhead, though we are adding in the time to convert the `data.frame`).

``` r
library(data.table)

f_data_table = function(dat) {
  dat <- data.table(dat)
  dat[ , ratio := y / x
       ][order(-ratio) , simple_rank := 1:.N, by = list(g)
          ][ , choice := simple_rank == 1]
}

f_data_table(d) %.>%
  knitr::kable(.)
```

| x |  y | g |     ratio | simple\_rank | choice |
| -: | -: | :- | --------: | -----------: | :----- |
| 1 |  2 | a | 2.0000000 |            1 | TRUE   |
| 2 |  2 | a | 1.0000000 |            2 | FALSE  |
| 3 |  2 | a | 0.6666667 |            3 | FALSE  |
| 4 |  3 | b | 0.7500000 |            3 | FALSE  |
| 5 |  7 | b | 1.4000000 |            2 | FALSE  |
| 6 | 10 | b | 1.6666667 |            1 | TRUE   |

``` r
library(microbenchmark)

timings <- microbenchmark(
  rquery_compiled = f_compiled(d_large),
  rquery_immediate = f_immediate(d_large),
  rquery_wrapped = f_wrapped(d_large),
  data.table = f_data_table(d_large),
  times = 10L
)

print(timings)
```

    ## Unit: milliseconds
    ##              expr      min       lq      mean   median        uq       max
    ##   rquery_compiled 616.6470 641.2185  691.7500 665.4975  746.9114  818.6258
    ##  rquery_immediate 810.5923 922.7914 1036.9230 964.8026 1137.7349 1466.3925
    ##    rquery_wrapped 643.4293 686.7460  763.3003 745.0626  820.7949  878.3681
    ##        data.table 484.3947 523.5561  567.5336 545.8624  614.2712  696.6354
    ##  neval
    ##     10
    ##     10
    ##     10
    ##     10

Notice, the speed differences are usually not that large for short
pipelines. Then intent is: pipeline construction and data conversion
steps should be cheap compared to the actual processing steps.

For those interested: we add `dplyr` and `dtplyr` to the timing
comparisons
[here](https://github.com/WinVector/rquery/blob/master/Examples/Modes/Modes_dplyr.md).
