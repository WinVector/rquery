Summary Example
================
John Mount, Win-Vector LLC
3/13/2018

``` r
library("rquery")
```

    ## Loading required package: wrapr

    ## Loading required package: DBI

``` r
# driver <- RPostgreSQL::PostgreSQL()
driver <- RPostgres::Postgres()
```

    ## Warning: multiple methods tables found for 'dbQuoteLiteral'

``` r
db <- DBI::dbConnect(driver,
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')

dbi_copy_to(db, "d",
            data.frame(v = NA_real_,
                       w = 1,
                       x = c(rev(1:10), NA, NA),
                       y = c(NA, NA, sin(1:10)),
                       z = rev(letters[1:12]),
                       stringsAsFactors = FALSE),
            temporary = TRUE,
            overwrite = TRUE)
```

    ## [1] "table('d')"

``` r
optree <- dbi_table(db, "d") %.>%
  quantile_node(.)

execute(db, optree) %.>%
  knitr::kable(.)
```

|  probs| v   |    w|    x|          y| z   |
|------:|:----|----:|----:|----------:|:----|
|   0.00| NA  |    1|    1|  -0.958924| a   |
|   0.25| NA  |    1|    3|  -0.544021| c   |
|   0.50| NA  |    1|    5|   0.141120| f   |
|   0.75| NA  |    1|    8|   0.841471| i   |
|   1.00| NA  |    1|   10|   0.989358| l   |

``` r
optrees <- dbi_table(db, "d") %.>%
  rsummary_node(., quartiles = TRUE)

execute(db, optrees) %.>%
  knitr::kable(.)
```

| column |  index| class     |  nrows|  nna|  nunique|        min|        max|      mean|        sd| lexmin | lexmax |         Q1|   median|        Q3|
|:-------|------:|:----------|------:|----:|--------:|----------:|----------:|---------:|---------:|:-------|:-------|----------:|--------:|---------:|
| v      |      1| numeric   |     12|   12|        0|         NA|         NA|        NA|        NA| NA     | NA     |         NA|       NA|        NA|
| w      |      2| numeric   |     12|    0|       NA|   1.000000|   1.000000|  1.000000|  0.000000| NA     | NA     |   1.000000|  1.00000|  1.000000|
| x      |      3| integer   |     12|    2|       NA|   1.000000|  10.000000|  5.500000|  3.027650| NA     | NA     |   3.000000|  5.00000|  8.000000|
| y      |      4| numeric   |     12|    2|       NA|  -0.958924|   0.989358|  0.141119|  0.730471| NA     | NA     |  -0.544021|  0.14112|  0.841471|
| z      |      5| character |     12|    0|       12|         NA|         NA|        NA|        NA| a      | l      |         NA|       NA|        NA|

``` r
DBI::dbDisconnect(db)
```
