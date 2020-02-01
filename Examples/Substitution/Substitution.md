Substitution
================

[`rquery`](https://github.com/WinVector/rquery) allows [substitution of
variable values for column/variable
names](https://cran.r-project.org/web/packages/rquery/vignettes/rquery_substitution.html)
in several places. This is particularly useful in the
[`extend()`](https://winvector.github.io/rquery/reference/extend.html)
and
[`select_rows()`](https://winvector.github.io/rquery/reference/select_rows.html)
operations.

The substitution is performed using `bquote` “`.()`” notation. The
values being substituted must be of class `name`, to specify they are
refering to column names (and not string constants).

Here is an example.

``` r
d <- data.frame(
  x = c(1, 1, 2),
  y = c(5, 4, 3),
  z = c(6, 7, 8)
)

knitr::kable(d)
```

| x | y | z |
| -: | -: | -: |
| 1 | 5 | 6 |
| 1 | 4 | 7 |
| 2 | 3 | 8 |

``` r
library("rquery")
```

    ## Loading required package: wrapr

``` r
condition_variable <- as.name('x')
new_value_variable <- as.name('y')
old_value_variable <- as.name('z')

ops <- local_td(d) %.>%
  select_rows(.,
              .(condition_variable) == 1) %.>%
  extend(.,
         .(new_value_variable) := .(old_value_variable) + 1)

cat(format(ops))
```

    ## mk_td("d", c(
    ##   "x",
    ##   "y",
    ##   "z")) %.>%
    ##  select_rows(.,
    ##    x == 1) %.>%
    ##  extend(.,
    ##   y := z + 1)

And we can execute the operations as follows.

``` r
d %.>% 
  ops %.>%
  knitr::kable(.)
```

| x | y | z |
| -: | -: | -: |
| 1 | 7 | 6 |
| 1 | 8 | 7 |

This also works in the `wrap`/`ex` pattern.

``` r
d %.>%
  wrap %.>%
  select_rows(.,
              .(condition_variable) == 1) %.>%
  extend(.,
         .(new_value_variable) := .(old_value_variable) + 1) %.>%
  ex %.>%
  knitr::kable(.)
```

| x | y | z |
| -: | -: | -: |
| 1 | 7 | 6 |
| 1 | 8 | 7 |

And the method can also be used with project.

``` r
d %.>%
  wrap %.>%
  select_rows(.,
              .(condition_variable) == 1) %.>%
  project(.,
         .(new_value_variable) := max(.(old_value_variable))) %.>%
  ex %.>%
  knitr::kable(.)
```

| y |
| -: |
| 7 |

We want to avoid using strings instead of names, as strings don’t always
give us the expected result. `rquery` tries to work around and catch
many of these cases. For example:

``` r
condition_name <- 'x'

ops2 <- local_td(d) %.>%
  select_rows(.,
              .(condition_name) == 1)
```

    ## Warning in warn_about_filter_conditions(parsed): rquery::select_rows: expression
    ## "x" == 1 refers to no columns (so is a constant)

``` r
cat(format(ops2))
```

    ## mk_td("d", c(
    ##   "x",
    ##   "y",
    ##   "z")) %.>%
    ##  select_rows(.,
    ##    "x" == 1)

New to `rquery` `1.4.3` we have a new “`-` to strip off quotes” feature.
What this is: `.(-v)` converts `v` from a character type to a name.
Meaning we don’t have to use the `as.name()` calls if we don’t want to.

``` r
condition_name <- 'x'

ops3 <- local_td(d) %.>%
  select_rows(.,
              .(-condition_name) == 1)

cat(format(ops3))
```

    ## mk_td("d", c(
    ##   "x",
    ##   "y",
    ##   "z")) %.>%
    ##  select_rows(.,
    ##    x == 1)
