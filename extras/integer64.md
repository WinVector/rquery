Take Care When Using the RPostgres Package
================
Win-Vector LLC
3/15/2018

Take care when using the new [`RPostgres`](https://CRAN.R-project.org/package=RPostgres) database connection package. It by default returns some types that code developed against other database drivers may not expect and may not be able to defend against.

Tryint the new package
----------------------

We can try the newer [`RPostgres`](https://CRAN.R-project.org/package=RPostgres) as a drop-in replacement for the usual [`RPostgreSQL`](https://CRAN.R-project.org/package=RPostgreSQL).

That starts out okay. We can connect to the database and and pull a summary about remote data to `R`.

``` r
db <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = 'localhost',
  port = 5432,
  user = 'johnmount',
  password = '')
```

    ## Warning: multiple methods tables found for 'dbQuoteLiteral'

``` r
d <- DBI::dbGetQuery(
  db, 
  "SELECT COUNT(1) FROM pg_catalog.pg_tables")
print(d)
```

    ##   count
    ## 1   177

``` r
ntables <- d$count[[1]]
print(ntables)
```

    ## integer64
    ## [1] 177

The result at first looks okay.

``` r
class(ntables)
```

    ## [1] "integer64"

``` r
typeof(ntables)
```

    ## [1] "double"

``` r
ntables + 1L
```

    ## integer64
    ## [1] 178

``` r
ntables + 1
```

    ## integer64
    ## [1] 178

``` r
is.numeric(ntables)
```

    ## [1] TRUE

But it is only okay, until it is not.

``` r
pmax(1L, ntables)
```

    ## [1] 8.744962e-322

``` r
pmin(1L, ntables)
```

    ## [1] 1

``` r
ifelse(TRUE, ntables, ntables)
```

    ## [1] 8.744962e-322

``` r
for(ni in ntables) {
  print(ni)
}
```

    ## [1] 8.744962e-322

``` r
unclass(ntables)
```

    ## [1] 8.744962e-322

If you or a package perform any one of the above calculations, your result is now corrupt and wrong. It is quite likely any code written before December 2017 ([`RPostgres`'s first `CRAN` distribution](https://cran.rstudio.com/src/contrib/Archive/RPostgres/)) would not have been written with the `RPostgres` "`integer64` for all of my friends" design decision in mind.

Also note, `RPostgres` does not currently appear to write `integer64` back to the database.

``` r
DBI::dbWriteTable(db, "d", d, 
                  temporary = TRUE, 
                  overwrite = TRUE)
DBI::dbGetQuery(db, "
  SELECT 
     column_name, 
     data_type, 
     numeric_precision, 
     numeric_precision_radix,
     udt_name
  FROM 
     information_schema.columns 
  WHERE 
     table_name = 'd'
  ")
```

    ##   column_name data_type numeric_precision numeric_precision_radix udt_name
    ## 1       count      real                24                       2   float4

``` r
DBI::dbDisconnect(db)
```

The work-around
---------------

The work-around is: add the argument `bigint = "numeric"` to your `dbConnect()` call. This is mentioned in the [manual](https://cran.r-project.org/web/packages/RPostgres/RPostgres.pdf), but not the default and not called out in the package description or [`README`](https://cran.r-project.org/web/packages/RPostgres/README.html).
