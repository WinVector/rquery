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

``` r
d %.>% 
  ops %.>%
  knitr::kable(.)
```

| x | y | z |
| -: | -: | -: |
| 1 | 7 | 6 |
| 1 | 8 | 7 |
