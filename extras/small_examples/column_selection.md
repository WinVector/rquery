column selection
================

`dplyr` is inconsistent as to which column is selected unless one uses
extra notation such as `!!`, `{{}}`, `.data[[]]`, and so on. Of course
if using a name or string directly are not the “correct” notation, why
are they allowed? Notice how different columns are selected in each
example, depending on the columns present in the `data.frame`. The issue
is `dplyr` does not commit to an unambiguous interpretation of the basic
notation (only the more complicated, longer notations have reliable
semantics).

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
y = "x"

data.frame(x = 1) %>%
  select(y)
```

    ##   x
    ## 1 1

``` r
data.frame(x = 1, y = 2) %>%
  select(y)
```

    ##   y
    ## 1 2

`dplyr` notations that are unambiguous include:

``` r
data.frame(x = 1) %>%
  select({{y}})
```

    ##   x
    ## 1 1

``` r
data.frame(x = 1, y = 2) %>%
  select
```

    ## data frame with 0 columns and 1 row

``` r
data.frame(x = 1) %>%
  select(!!y)
```

    ##   x
    ## 1 1

``` r
data.frame(x = 1, y = 2) %>%
  select(!!y)
```

    ##   x
    ## 1 1

``` r
data.frame(x = 1) %>%
  select(!!rlang::enquo(y))
```

    ##   x
    ## 1 1

``` r
data.frame(x = 1, y = 2) %>%
  select(!!rlang::enquo(y))
```

    ##   x
    ## 1 1

``` r
data.frame(x = 1) %>%
  select(.data[[y]])
```

    ##   x
    ## 1 1

``` r
data.frame(x = 1, y = 2) %>%
  select(.data[[y]])
```

    ##   x
    ## 1 1

But other notations don’t work (`.data` is apparently a mapping from
column names to column indices, and not in fact a reference to the
incoming `data.frame`).

``` r
data.frame(x = 1) %>%
  select(.data[y])
```

    ## `.data[y]` must evaluate to column positions or names, not a list

``` r
data.frame(x = 1, y = 2) %>%
  select(.data[y])
```

    ## `.data[y]` must evaluate to column positions or names, not a list

`R` itself does not have this problem. Notice how the column named by
`y` (which turns out to be `x`) is reliably chosen in all cases. In `[]`
and `[[]]` notations columns are always values (not taken from code or
variable names; and `$` always take from code and not from values).

``` r
y = "x"

data.frame(x = 1)[y]
```

    ##   x
    ## 1 1

``` r
data.frame(x = 1, y = 2)[y]
```

    ##   x
    ## 1 1

`rqdatable` also has reliable column selection semantics, columns are
always values (not taken from code or variable names).

``` r
library("rqdatatable")
```

    ## Loading required package: rquery

``` r
y = "x"

data.frame(x = 1) %.>% 
  select_columns(., y)
```

    ##    x
    ## 1: 1

``` r
data.frame(x = 1, y = 2) %.>% 
  select_columns(., y)
```

    ##    x
    ## 1: 1
