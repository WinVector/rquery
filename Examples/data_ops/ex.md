ex Example
================

``` r
library(rqdatatable)
```

    ## Loading required package: rquery

``` r
d <- data.frame(x = 1:3, y = 4:6)
d %.>%
  wrap(.) %.>%
  extend(., z := x + y) %.>%
  ex(.) %.>%
  knitr::kable(.)
```

| x | y | z |
| -: | -: | -: |
| 1 | 4 | 5 |
| 2 | 5 | 7 |
| 3 | 6 | 9 |

``` r
d %.>%
  knitr::kable(.)
```

| x | y |
| -: | -: |
| 1 | 4 |
| 2 | 5 |
| 3 | 6 |
