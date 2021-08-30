Simplifiation
================

``` r
library("rquery")
```

    ## Loading required package: wrapr

``` r
library("rqdatatable")
```

Roughly we consider `extend()` safe to fold, and `select_rows()` unsafe
(as this may eliminate guards). So we combine `extend()` steps, but not
`select_rows()` steos.

``` r
d2 <- data.frame(
    col1 = c(0, 1),
    col2 = c(1, 0)
)


ops2 <- local_td(d2) %.>%
  extend(., x := 1) %.>%
  extend(., x := 2) %.>%
  select_rows(., col2 > 0) %.>%
  select_rows(., col1 / col2 > 0)

ops2
```

    ## [1] "mk_td(\"d2\", c( \"col1\", \"col2\")) %.>% extend(., x := 2) %.>% select_rows(., col2 > 0) %.>% select_rows(., col1 / col2 > 0)"

``` r
d2 %.>% ops2
```

    ## [1] col1 col2 x   
    ## <0 rows> (or 0-length row.names)
