QTiming
================
Win-Vector LLC
1/10/2018

Let's time [`rquery`](https://winvector.github.io/rquery/), [`dplyr`](https://CRAN.R-project.org/package=dplyr), and [`data.table`](https://CRAN.R-project.org/package=data.table) on a non-trivial example.

These timings are on a late 2014 Mac Mini with 8GB of RAM running OSX 10.12.6, R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree", and the current (2018-01-07) CRAN versions of all packages (except `rquery`, which is not yet up on CRAN). We are getting database services from PostgreSQL version `9.6.1` in a docker container.

First let's load our packages, establish a database connection, and declare an [`rquery` ad hoc execution service](https://winvector.github.io/rquery/articles/AdHocQueries.html) (the "`winvector_temp_db_handle`").

``` r
library("data.table")  # load first so we can overwrite := with rquery
library("rquery")
```

    ## Loading required package: wrapr

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:data.table':
    ## 
    ##     :=

    ## Loading required package: cdata

``` r
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("microbenchmark")
library("ggplot2")
source("cscan.R")
source("fns.R")

db <- NULL
db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'postgres',
                     password = 'pg')
if(!is.null(db)) {
  winvector_temp_db_handle <- list(db = db)
  print(db)
  DBI::dbGetQuery(db, "SELECT version()", stringsAsFactors = FALSE)
}
```

    ## <PqConnection> postgres@localhost:5432

    ##                                                                                    version
    ## 1 PostgreSQL 9.6.1 on x86_64-pc-linux-gnu, compiled by gcc (Debian 4.9.2-10) 4.9.2, 64-bit

``` r
packageVersion("rquery")
```

    ## [1] '0.2.0'

``` r
packageVersion("dplyr")
```

    ## [1] '0.7.4'

``` r
packageVersion("dbplyr")
```

    ## [1] '1.2.0'

``` r
packageVersion("DBI")
```

    ## [1] '0.7'

``` r
packageVersion("data.table")
```

    ## [1] '1.10.4.3'

``` r
packageVersion("RPostgres")
```

    ## [1] '1.0.4'

``` r
R.Version()
```

    ## $platform
    ## [1] "x86_64-apple-darwin15.6.0"
    ## 
    ## $arch
    ## [1] "x86_64"
    ## 
    ## $os
    ## [1] "darwin15.6.0"
    ## 
    ## $system
    ## [1] "x86_64, darwin15.6.0"
    ## 
    ## $status
    ## [1] ""
    ## 
    ## $major
    ## [1] "3"
    ## 
    ## $minor
    ## [1] "4.3"
    ## 
    ## $year
    ## [1] "2017"
    ## 
    ## $month
    ## [1] "11"
    ## 
    ## $day
    ## [1] "30"
    ## 
    ## $`svn rev`
    ## [1] "73796"
    ## 
    ## $language
    ## [1] "R"
    ## 
    ## $version.string
    ## [1] "R version 3.4.3 (2017-11-30)"
    ## 
    ## $nickname
    ## [1] "Kite-Eating Tree"

We now build and extended version of the example from [Let’s Have Some Sympathy For The Part-time R User](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

``` r
nrep <- 10

dLocal <- mkData(nrep)
head(dLocal)
```

    ##   subjectID      surveyCategory assessmentTotal
    ## 1        s1 withdrawal behavior               5
    ## 2        s1 positive re-framing               8
    ## 3       s10 withdrawal behavior               8
    ## 4       s10 positive re-framing               6
    ## 5        s2 withdrawal behavior               5
    ## 6        s2 positive re-framing               0

``` r
dR <- NULL
dTbl <- NULL

if(!is.null(db)) {
  dR <- rquery::dbi_copy_to(db, 'dR',
                            dLocal,
                            temporary = TRUE, 
                            overwrite = TRUE)
  dTbl <- dplyr::tbl(db, dR$table_name)
  
  cdata::qlook(db, dR$table_name)
  
  dplyr::glimpse(dTbl)
}
```

    ## table "dR" PqConnection 
    ##  nrow: 20 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  3 variables:
    ##  $ subjectID      : chr  "s1" "s1" "s10" "s10" ...
    ##  $ surveyCategory : chr  "withdrawal behavior" "positive re-framing" "withdrawal behavior" "positive re-framing" ...
    ##  $ assessmentTotal: num  5 8 8 6 5 0 0 4 7 1
    ## Observations: NA
    ## Variables: 3
    ## $ subjectID       <chr> "s1", "s1", "s10", "s10", "s2", "s2", "s3", "s...
    ## $ surveyCategory  <chr> "withdrawal behavior", "positive re-framing", ...
    ## $ assessmentTotal <dbl> 5, 8, 8, 6, 5, 0, 0, 4, 7, 1, 1, 0, 8, 8, 4, 4...

Now we declare our operation pipelines, both on local (in-memory `data.frame`) and remote (already in a database) data.

``` r
scale <- 0.237

base_R_row_calculation <- function() {
  base_r_calculate_rows(dLocal)
}

base_R_sequential_calculation <- function() {
  base_r_calculate_sequenced(dLocal)
}

base_R_cframe_calculation <- function() {
  base_r_calculate_cframe(dLocal)
}


base_R_tabular_calculation <- function() {
  base_r_calculate_tabular(dLocal)
}

rquery_local <- function() {
  dLocal %.>% 
    rquery_pipeline(.) %.>%
    as.data.frame(., stringsAsFactors = FALSE) # force execution
}

rquery_database_pull <- function() {
  dR %.>% 
    rquery_pipeline(.) %.>% 
    to_sql(., db) %.>% 
    DBI::dbGetQuery(db, ., stringsAsFactors = FALSE) %.>%
    as.data.frame(., stringsAsFactors = FALSE) # shouldn't be needed
}

rquery_database_land <- function() {
  tabName <- "rquery_tmpx"
  sqlc <- dR %.>% 
    rquery_pipeline(.) %.>% 
    to_sql(., db)
  DBI::dbExecute(db, paste("CREATE TABLE", tabName, "AS", sqlc))
  DBI::dbExecute(db, paste("DROP TABLE", tabName))
  NULL
}

rquery_database_count <- function() {
  dR %.>% 
    rquery_pipeline(.) %.>% 
    sql_node(., "n" := "COUNT(1)") %.>% 
    to_sql(., db) %.>% 
    DBI::dbGetQuery(db, ., stringsAsFactors = FALSE) %.>%
    as.data.frame(., stringsAsFactors = FALSE) # shouldn't be needed
}


dplyr_local <- function() {
  dLocal %>% 
    dplyr_pipeline
}

dplyr_local_no_grouped_filter <- function() {
  dLocal %>% 
    dplyr_pipeline2
}

dplyr_tbl <- function() {
  dLocal %>%
    as_tibble %>%
    dplyr_pipeline
}

dplyr_round_trip <- function() {
  dTmp <- dplyr::copy_to(db, dLocal, "dplyr_tmp",
                         # overwrite = TRUE,
                         temporary = TRUE
  )
  res <- dTmp %>% 
    dplyr_pipeline %>%
    collect()
  dplyr::db_drop_table(db, "dplyr_tmp")
  res
}

dplyr_database_pull <- function() {
  dTbl %>% 
    dplyr_pipeline %>%
    collect()
}

dplyr_database_land <- function() {
  tabName = "dplyr_ctmpx"
  dTbl %>% 
    dplyr_pipeline %>%
    compute(name = tabName)
  dplyr::db_drop_table(db, table = tabName)
  NULL
}

dplyr_database_count <- function() {
  dTbl %>% 
    dplyr_pipeline %>%
    tally() %>%
    collect()
}
```

Let's inspect the functions.

``` r
check <- base_R_sequential_calculation()
head(check)
```

    ##   subjectID           diagnosis probability
    ## 1        s1 positive re-framing   0.6706221
    ## 2       s10 withdrawal behavior   0.6163301
    ## 3        s2 withdrawal behavior   0.7658456
    ## 4        s3 positive re-framing   0.7207128
    ## 5        s4 withdrawal behavior   0.8056518
    ## 6        s5 withdrawal behavior   0.5589742

``` r
if(!equiv_res(check, base_R_cframe_calculation())) {
  stop("mismatch")
}

if(!equiv_res(check, base_R_row_calculation())) {
  stop("mismatch")
}

if(!equiv_res(check, base_R_tabular_calculation())) {
  stop("mismatch")
}

if(!equiv_res(check, dplyr_local())) {
  stop("mismatch")
}

if(!equiv_res(check, dplyr_tbl())) {
  stop("mismatch")
}

if(!equiv_res(check, dplyr_local_no_grouped_filter())) {
  stop("mismatch")
}

if(!equiv_res(check, data.table_local())) {
  stop("mismatch")
}

if(!is.null(db)) {
  head(rquery_local())
  
  rquery_database_land()
  
  head(rquery_database_pull())
  
  rquery_database_count()
  
  dplyr_database_land()
  
  head(dplyr_database_pull())
  
  dplyr_database_count()
  
  head(dplyr_round_trip())
}
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 s1        positive re-framing       0.671
    ## 2 s10       withdrawal behavior       0.616
    ## 3 s2        withdrawal behavior       0.766
    ## 4 s3        positive re-framing       0.721
    ## 5 s4        withdrawal behavior       0.806
    ## 6 s5        withdrawal behavior       0.559

Now let's measure the speeds with `microbenchmark`.

``` r
timings <- NULL


expressions <- list(
    # "rquery in memory" = bquote({ nrow(rquery_local())}),
    # "rquery from db to memory" =  bquote({nrow(rquery_database_pull())}),
    # "rquery database count" =  bquote({rquery_database_count()}),
    # "rquery database land" =  bquote({rquery_database_land()}),
    # "dplyr in memory" =  bquote({nrow(dplyr_local())}),
    # "dplyr tbl in memory" =  bquote({nrow(dplyr_tbl())}),
    "dplyr in memory no grouped filter" =  bquote({nrow(dplyr_local_no_grouped_filter())}),
    # "dplyr from memory to db and back" =  bquote({nrow(dplyr_round_trip())}),
    # "dplyr from db to memory" =  bquote({nrow(dplyr_database_pull())}),
    # "dplyr database count" =  bquote({dplyr_database_count()}),
    # "dplyr database land" =  bquote({dplyr_database_land()}),
    "data.table in memory" =  bquote({nrow(data.table_local())}),
    # "base R row calculation" =  bquote({nrow(base_R_row_calculation())}),
    "base R tabular calculation" =  bquote({nrow(base_R_tabular_calculation())}),
    # "base R sequential calculation" =  bquote({nrow(base_R_sequential_calculation())}),
    "base R cframe calculation" =  bquote({nrow(base_R_cframe_calculation())})
)

if(!is.null(db)) {
  expressions <- 
    c(expressions,
      list(
        "rquery from memory to db and back" = bquote({ nrow(rquery_local())}),
        # "rquery from db to memory" =  bquote({nrow(rquery_database_pull())}),
        "rquery database count" =  bquote({rquery_database_count()}),
        "rquery database land" =  bquote({rquery_database_land()}),
        # "dplyr in memory" =  bquote({nrow(dplyr_local())}),
        # "dplyr tbl in memory" =  bquote({nrow(dplyr_tbl())}),
        "dplyr from memory to db and back" =  bquote({nrow(dplyr_round_trip())}),
        # "dplyr from db to memory" =  bquote({nrow(dplyr_database_pull())}),
        "dplyr database count" =  bquote({dplyr_database_count()}),
        "dplyr database land" =  bquote({dplyr_database_land()})
      ))
}

prune <- FALSE

for(nrep in c(1,
              10, 
              100, 
              1000,
              10000,
              100000, 
              1000000)) {
  print(nrep)
  dLocal <- mkData(nrep)
  dR <- NULL
  dTbl <- NULL
  if(!is.null(db)) {
    dR <- rquery::dbi_copy_to(db, 'dR',
                              dLocal,
                              temporary = TRUE, 
                              overwrite = TRUE)
    dTbl <- dplyr::tbl(db, dR$table_name)
  }
  tm <- microbenchmark(
    list = expressions,
    times = 5L
  )
  print(tm)
  print(autoplot(tm))
  tmi <- as.data.frame(tm, stringsAsFactors = FALSE)
  tmi$data_size <- nrow(dLocal)
  timings <- rbind(timings, tmi)
  if(prune) {
    baddies <- unique(tmi$expr[tmi$time > 10*1e+9])
    for(bi in baddies) {
      expressions[[bi]] <- NULL
    }
    if(length(expressions)<=0) {
      break
    }
  }
}
```

    ## [1] 1
    ## Unit: microseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter  18604.266  19110.376  20974.588
    ##               data.table in memory   2060.398   2077.499   3551.220
    ##         base R tabular calculation   2206.870   2493.420   3037.395
    ##          base R cframe calculation    804.223    826.674   1260.620
    ##  rquery from memory to db and back  34938.135  35259.591  37456.006
    ##              rquery database count  19879.596  20907.581  22204.841
    ##               rquery database land  27672.659  29803.167  31687.987
    ##   dplyr from memory to db and back 131293.165 133815.728 149384.897
    ##               dplyr database count 120688.426 122534.945 124312.252
    ##                dplyr database land 153760.277 164176.872 183496.367
    ##      median         uq        max neval
    ##   20851.466  21131.775  25175.056     5
    ##    2108.786   2489.462   9019.957     5
    ##    2744.533   3590.913   4151.238     5
    ##    1301.593   1454.119   1916.489     5
    ##   37492.824  38274.070  41315.408     5
    ##   21034.611  23796.268  25406.148     5
    ##   30045.298  32904.975  38013.834     5
    ##  138536.765 154642.791 188636.035     5
    ##  123099.852 123720.405 131517.634     5
    ##  174869.846 205426.187 219248.654     5

![](QTiming_files/figure-markdown_github/timings-1.png)

    ## [1] 10
    ## Unit: microseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter  18498.798  20298.166  24463.777
    ##               data.table in memory   2020.115   2092.674   2245.385
    ##         base R tabular calculation   2766.977   2773.072   2924.768
    ##          base R cframe calculation    809.380    905.822   1041.052
    ##  rquery from memory to db and back  38622.784  40659.025  48520.804
    ##              rquery database count  20552.020  24508.929  27516.412
    ##               rquery database land  27854.254  28669.708  35773.414
    ##   dplyr from memory to db and back 130097.716 134385.614 167191.396
    ##               dplyr database count 124005.371 149161.631 150896.841
    ##                dplyr database land 152755.392 170968.877 194796.395
    ##      median         uq        max neval
    ##   23773.248  27646.428  32102.246     5
    ##    2156.889   2334.537   2622.709     5
    ##    2776.116   2834.526   3473.150     5
    ##    1048.368   1206.161   1235.527     5
    ##   42539.683  55218.608  65563.922     5
    ##   24662.668  33545.354  34313.091     5
    ##   33761.465  37617.238  50964.406     5
    ##  175929.845 194867.476 200676.328     5
    ##  152190.104 154744.900 174382.199     5
    ##  193630.977 224626.106 232000.621     5

![](QTiming_files/figure-markdown_github/timings-2.png)

    ## [1] 100
    ## Unit: microseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter  21946.413  22405.506  24800.577
    ##               data.table in memory   2061.562   2278.797   2285.475
    ##         base R tabular calculation   3930.858   3953.243   4114.171
    ##          base R cframe calculation    926.499    962.150   1112.649
    ##  rquery from memory to db and back  37735.858  38395.138  41264.175
    ##              rquery database count  22600.274  23411.051  28650.843
    ##               rquery database land  29597.937  29933.004  31502.550
    ##   dplyr from memory to db and back 138918.852 139824.239 150320.280
    ##               dplyr database count 122739.930 124228.090 132979.918
    ##                dplyr database land 154191.195 162983.379 176809.352
    ##      median         uq        max neval
    ##   22606.867  24329.195  32714.902     5
    ##    2300.356   2372.106   2414.556     5
    ##    4016.240   4158.512   4512.004     5
    ##    1026.448   1208.693   1439.455     5
    ##   39530.097  42166.659  48493.124     5
    ##   29498.902  32858.658  34885.329     5
    ##   30426.525  30971.114  36584.170     5
    ##  143877.325 153472.194 175508.788     5
    ##  128299.269 129588.733 160043.566     5
    ##  176056.116 184940.542 205875.528     5

![](QTiming_files/figure-markdown_github/timings-3.png)

    ## [1] 1000
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter  52.473963  53.990990  63.028670
    ##               data.table in memory   4.455679   4.507801   5.483580
    ##         base R tabular calculation  17.599765  17.913393  19.124216
    ##          base R cframe calculation   1.154692   1.211258   1.258236
    ##  rquery from memory to db and back  46.544463  47.237076  51.689153
    ##              rquery database count  27.113162  28.647527  33.635749
    ##               rquery database land  34.889043  35.336711  37.266456
    ##   dplyr from memory to db and back 142.808719 144.400584 164.001254
    ##               dplyr database count 129.880032 131.432918 132.510677
    ##                dplyr database land 153.132841 153.498812 171.558014
    ##      median         uq        max neval
    ##   56.979042  70.296466  81.402891     5
    ##    5.018678   6.572011   6.863733     5
    ##   18.700409  19.640466  21.767046     5
    ##    1.256009   1.312208   1.357014     5
    ##   48.091848  51.998220  64.574159     5
    ##   33.869665  38.679622  39.868768     5
    ##   38.079601  38.609171  39.417754     5
    ##  171.087333 174.662941 187.046694     5
    ##  131.576545 133.086278 136.577613     5
    ##  154.349510 156.112850 240.696058     5

![](QTiming_files/figure-markdown_github/timings-4.png)

    ## [1] 10000
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter 445.479793 462.128216 461.558608
    ##               data.table in memory  32.679948  33.285725  37.555572
    ##         base R tabular calculation 203.182324 208.605384 274.796484
    ##          base R cframe calculation   6.214073   6.384175   7.663596
    ##  rquery from memory to db and back 158.081700 158.686633 167.098485
    ##              rquery database count  84.006778  84.235042  89.692751
    ##               rquery database land  94.663270 100.040548 130.978910
    ##   dplyr from memory to db and back 310.333586 325.436052 347.078224
    ##               dplyr database count 236.267981 237.502695 269.441125
    ##                dplyr database land 266.223856 293.120698 307.784142
    ##      median         uq       max neval
    ##  463.005306 464.042453 473.13727     5
    ##   36.148643  41.206449  44.45710     5
    ##  296.723821 330.933172 334.53772     5
    ##    6.443698   6.904465  12.37157     5
    ##  159.267349 177.548843 181.90790     5
    ##   84.656834  85.680394 109.88471     5
    ##  101.944803 145.969241 212.27669     5
    ##  343.316783 357.967376 398.33733     5
    ##  258.436019 266.648524 348.35040     5
    ##  302.511094 314.292650 362.77241     5

![](QTiming_files/figure-markdown_github/timings-5.png)

    ## [1] 1e+05
    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 5017.5726 5133.4360 5339.9600 5221.5242
    ##               data.table in memory  331.0423  341.3509  433.3445  385.4363
    ##         base R tabular calculation 3152.4825 3231.6479 3681.9147 3289.0593
    ##          base R cframe calculation  111.1425  117.2846  142.7184  117.5100
    ##  rquery from memory to db and back 2974.7296 3101.3483 3390.2958 3183.3395
    ##              rquery database count 2397.9234 2422.7249 3000.0021 3017.8357
    ##               rquery database land 2421.4490 2503.7378 2667.8342 2581.6713
    ##   dplyr from memory to db and back 3356.7295 3367.4212 3522.3745 3373.7240
    ##               dplyr database count 2657.5513 2831.2473 3277.9244 3203.9260
    ##                dplyr database land 2734.8701 2773.8153 2925.0462 2780.1310
    ##         uq       max neval
    ##  5248.7433 6078.5240     5
    ##   439.9731  668.9198     5
    ##  4312.3620 4424.0219     5
    ##   125.9056  241.7493     5
    ##  3629.2971 4062.7644     5
    ##  3326.9069 3834.6198     5
    ##  2588.7821 3243.5308     5
    ##  3440.3051 4073.6929     5
    ##  3717.2611 3979.6364     5
    ##  3086.2025 3250.2121     5

![](QTiming_files/figure-markdown_github/timings-6.png)

    ## [1] 1e+06
    ## Unit: seconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 61.138666 61.157309 62.968696 61.223800
    ##               data.table in memory  2.747111  3.106111  3.171730  3.273438
    ##         base R tabular calculation 34.492814 35.178702 35.153437 35.323243
    ##          base R cframe calculation  1.197866  1.516489  1.545648  1.549475
    ##  rquery from memory to db and back 33.293289 33.547535 35.079603 33.573165
    ##              rquery database count 27.397834 27.549569 28.880384 27.926856
    ##               rquery database land 27.785351 28.102473 28.487276 28.206404
    ##   dplyr from memory to db and back 39.158903 39.312264 39.422830 39.467064
    ##               dplyr database count 32.490253 32.592843 33.100726 32.781197
    ##                dplyr database land 32.860777 33.252879 33.746528 33.957826
    ##         uq       max neval
    ##  65.277189 66.046517     5
    ##   3.350313  3.381679     5
    ##  35.373962 35.398462     5
    ##   1.573302  1.891110     5
    ##  33.736385 41.247639     5
    ##  27.950838 33.576823     5
    ##  28.706067 29.636084     5
    ##  39.540772 39.635145     5
    ##  33.608900 34.030437     5
    ##  34.130398 34.530762     5

![](QTiming_files/figure-markdown_github/timings-7.png)

``` r
saveRDS(timings, "qtimings.RDS")
```

``` r
sessionInfo()
```

    ## R version 3.4.3 (2017-11-30)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Sierra 10.12.6
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] bindrcpp_0.2         ggplot2_2.2.1        microbenchmark_1.4-3
    ## [4] dplyr_0.7.4          rquery_0.2.0         cdata_0.5.1         
    ## [7] wrapr_1.1.1          data.table_1.10.4-3 
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.14.2   dbplyr_1.2.0     pillar_1.0.1     compiler_3.4.3  
    ##  [5] plyr_1.8.4       bindr_0.1        tools_3.4.3      RPostgres_1.0-4 
    ##  [9] digest_0.6.13    bit_1.1-12       evaluate_0.10.1  tibble_1.4.1    
    ## [13] gtable_0.2.0     pkgconfig_2.0.1  rlang_0.1.6      cli_1.0.0       
    ## [17] DBI_0.7          yaml_2.1.16      withr_2.1.1      stringr_1.2.0   
    ## [21] knitr_1.18       hms_0.4.0        tidyselect_0.2.3 rprojroot_1.3-2 
    ## [25] bit64_0.9-7      grid_3.4.3       glue_1.2.0       R6_2.2.2        
    ## [29] rmarkdown_1.8    purrr_0.2.4      blob_1.1.0       magrittr_1.5    
    ## [33] backports_1.1.2  scales_0.5.0     htmltools_0.3.6  assertthat_0.2.0
    ## [37] colorspace_1.3-2 utf8_1.1.3       stringi_1.1.6    lazyeval_0.2.1  
    ## [41] munsell_0.4.3    crayon_1.3.4

``` r
winvector_temp_db_handle <- NULL
if(!is.null(db)) {
  DBI::dbDisconnect(db)
  db <- NULL
}
```
