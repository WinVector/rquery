QTiming3
================
Win-Vector LLC
1/13/2018

Let's time [`rquery`](https://winvector.github.io/rquery/), [`dplyr`](https://CRAN.R-project.org/package=dplyr), and [`data.table`](https://CRAN.R-project.org/package=data.table) on a non-trivial example.

These timings are on an Amazon EC2 c5.4xlarge 16 vcpu 32GB RAM, 128GB block storage, Ubuntu Server 16.04 LTS (HVM).

First let's load our packages, establish a database connection, and declare an [`rquery` ad hoc execution service](https://winvector.github.io/rquery/articles/AdHocQueries.html) (the "`winvector_temp_db_handle`").

``` r
library("rquery")
```

    ## Loading required package: wrapr

    ## Loading required package: cdata

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
library("microbenchmark")
library("ggplot2")
source("cscan.R")
source("fns.R")

db <- NULL
db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'ruser',
                     password = 'ruser')
if(!is.null(db)) {
  winvector_temp_db_handle <- list(db = db)
  print(db)
  DBI::dbGetQuery(db, "SELECT version()", stringsAsFactors = FALSE)
}
```

    ## <PqConnection> ruser@localhost:5432

    ##                                                                                                            version
    ## 1 PostgreSQL 9.5.10 on x86_64-pc-linux-gnu, compiled by gcc (Ubuntu 5.4.0-6ubuntu1~16.04.4) 5.4.0 20160609, 64-bit

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
    ## [1] "x86_64-pc-linux-gnu"
    ## 
    ## $arch
    ## [1] "x86_64"
    ## 
    ## $os
    ## [1] "linux-gnu"
    ## 
    ## $system
    ## [1] "x86_64, linux-gnu"
    ## 
    ## $status
    ## [1] ""
    ## 
    ## $major
    ## [1] "3"
    ## 
    ## $minor
    ## [1] "2.3"
    ## 
    ## $year
    ## [1] "2015"
    ## 
    ## $month
    ## [1] "12"
    ## 
    ## $day
    ## [1] "10"
    ## 
    ## $`svn rev`
    ## [1] "69752"
    ## 
    ## $language
    ## [1] "R"
    ## 
    ## $version.string
    ## [1] "R version 3.2.3 (2015-12-10)"
    ## 
    ## $nickname
    ## [1] "Wooden Christmas-Tree"

We now build and extended version of the example from [Let’s Have Some Sympathy For The Part-time R User](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

``` r
nrep <- 10

dLocal <- mkData(nrep)
head(dLocal)
```

    ##   subjectID      surveyCategory assessmentTotal
    ## 1        s1 withdrawal behavior               7
    ## 2        s1 positive re-framing               1
    ## 3       s10 withdrawal behavior               7
    ## 4       s10 positive re-framing               3
    ## 5        s2 withdrawal behavior               0
    ## 6        s2 positive re-framing               2

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

    ## Warning: replacing previous import by 'rlang::enquo' when loading 'dbplyr'

    ## Warning: replacing previous import by 'rlang::quo' when loading 'dbplyr'

    ## Warning: replacing previous import by 'rlang::quos' when loading 'dbplyr'

    ## Warning: replacing previous import by 'rlang::quo_name' when loading
    ## 'dbplyr'

    ## table "dR" PqConnection 
    ##  nrow: 20 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  3 variables:
    ##  $ subjectID      : chr  "s1" "s1" "s10" "s10" ...
    ##  $ surveyCategory : chr  "withdrawal behavior" "positive re-framing" "withdrawal behavior" "positive re-framing" ...
    ##  $ assessmentTotal: num  7 1 7 3 0 2 9 1 3 8
    ## Observations: NA
    ## Variables: 3
    ## $ subjectID       <chr> "s1", "s1", "s10", "s10", "s2", "s2", "s3", "s...
    ## $ surveyCategory  <chr> "withdrawal behavior", "positive re-framing", ...
    ## $ assessmentTotal <dbl> 7, 1, 7, 3, 0, 2, 9, 1, 3, 8, 8, 0, 6, 9, 8, 1...

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
    ## 1        s1 withdrawal behavior   0.8056518
    ## 2       s10 withdrawal behavior   0.7207128
    ## 3        s2 positive re-framing   0.6163301
    ## 4        s3 withdrawal behavior   0.8694381
    ## 5        s4 positive re-framing   0.7658456
    ## 6        s5 withdrawal behavior   0.8694381

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
  # haven't worked on data.table tie breaking yet
  #stop("mismatch")
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
    ## 1 s1        withdrawal behavior       0.806
    ## 2 s10       withdrawal behavior       0.721
    ## 3 s2        positive re-framing       0.616
    ## 4 s3        withdrawal behavior       0.869
    ## 5 s4        positive re-framing       0.766
    ## 6 s5        withdrawal behavior       0.869

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
    "base R sequential calculation" =  bquote({nrow(base_R_sequential_calculation())}),
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
    ##                               expr        min         lq        mean
    ##  dplyr in memory no grouped filter  14919.794  16643.680  16505.5182
    ##               data.table in memory   2220.430   2310.583   2414.1946
    ##         base R tabular calculation   1974.774   2068.691   2113.3290
    ##      base R sequential calculation    673.452    689.799    729.5538
    ##          base R cframe calculation    562.561    570.416    608.6124
    ##  rquery from memory to db and back  26497.124  27702.526  28106.8860
    ##              rquery database count  17898.933  19714.732  19857.3174
    ##               rquery database land  27320.331  27355.301  27728.3996
    ##   dplyr from memory to db and back 108524.224 109462.540 111382.1920
    ##               dplyr database count 110414.790 111373.114 113225.6906
    ##                dplyr database land 125595.791 127317.767 128307.1956
    ##      median         uq        max neval
    ##   16665.241  16915.641  17383.235     5
    ##    2386.725   2464.256   2688.979     5
    ##    2106.032   2154.585   2262.563     5
    ##     746.418    758.131    779.969     5
    ##     571.243    646.200    692.642     5
    ##   28580.877  28862.517  28891.386     5
    ##   20284.732  20617.304  20770.886     5
    ##   27361.836  27671.148  28933.382     5
    ##  109768.180 114509.224 114646.792     5
    ##  113138.389 113633.019 117569.141     5
    ##  128808.984 129839.509 129973.927     5

![](QTiming3_files/figure-markdown_github/timings-1.png)

    ## [1] 10
    ## Unit: microseconds
    ##                               expr        min         lq        mean
    ##  dplyr in memory no grouped filter  16420.675  16593.048  18400.5070
    ##               data.table in memory   2833.826   2835.220   2932.0316
    ##         base R tabular calculation   2298.396   2425.995   2466.0360
    ##      base R sequential calculation    786.436    835.417    838.3962
    ##          base R cframe calculation    633.483    637.047    657.6866
    ##  rquery from memory to db and back  29463.002  29872.595  30805.1248
    ##              rquery database count  19133.098  20201.553  20923.5576
    ##               rquery database land  26742.925  28387.844  30086.8864
    ##   dplyr from memory to db and back 118479.723 118940.331 121407.9406
    ##               dplyr database count 118568.712 119506.020 123585.3676
    ##                dplyr database land 138400.484 138745.675 141819.1232
    ##      median         uq        max neval
    ##   18949.394  19850.321  20189.097     5
    ##    2853.650   3039.165   3098.297     5
    ##    2474.938   2515.565   2615.286     5
    ##     844.869    846.116    879.143     5
    ##     654.185    668.590    695.128     5
    ##   29912.274  31936.941  32840.812     5
    ##   20520.219  22086.054  22676.864     5
    ##   28676.400  28713.769  37913.494     5
    ##  122370.900 122496.180 124752.569     5
    ##  124688.934 126876.612 128286.560     5
    ##  139787.695 144239.770 147921.992     5

![](QTiming3_files/figure-markdown_github/timings-2.png)

    ## [1] 100
    ## Unit: microseconds
    ##                               expr        min         lq        mean
    ##  dplyr in memory no grouped filter  18073.306  18451.382  20218.6570
    ##               data.table in memory   4354.474   4564.098   4652.7636
    ##         base R tabular calculation   3333.128   3493.308   3586.1372
    ##      base R sequential calculation    791.136    867.436    896.2096
    ##          base R cframe calculation    623.525    702.986    700.9184
    ##  rquery from memory to db and back  28527.712  29694.450  30789.6628
    ##              rquery database count  21334.021  21985.211  22485.0316
    ##               rquery database land  27256.713  27599.084  28216.3726
    ##   dplyr from memory to db and back 121076.380 124278.987 124969.6364
    ##               dplyr database count 122768.739 123634.603 124730.8520
    ##                dplyr database land 138751.846 138848.373 141184.5488
    ##      median         uq        max neval
    ##   21241.644  21331.565  21995.388     5
    ##    4577.232   4883.388   4884.626     5
    ##    3525.960   3705.709   3872.581     5
    ##     937.679    937.984    946.813     5
    ##     703.420    729.838    744.823     5
    ##   30305.067  32458.124  32962.961     5
    ##   22251.857  23035.283  23818.786     5
    ##   28018.134  29070.530  29137.402     5
    ##  125425.407 125810.196 128257.212     5
    ##  124851.603 125719.184 126680.131     5
    ##  139267.903 143437.639 145616.983     5

![](QTiming3_files/figure-markdown_github/timings-3.png)

    ## [1] 1000
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter  40.569960  41.740071  43.683324
    ##               data.table in memory  21.320034  22.064059  23.434768
    ##         base R tabular calculation  12.338918  13.754944  14.120674
    ##      base R sequential calculation   1.513159   1.678584   1.720877
    ##          base R cframe calculation   1.327390   1.349103   1.382964
    ##  rquery from memory to db and back  38.373062  39.198296  59.911435
    ##              rquery database count  23.867159  24.529366  25.893482
    ##               rquery database land  32.537667  33.950576  36.103993
    ##   dplyr from memory to db and back 129.001820 131.162942 139.720157
    ##               dplyr database count 131.348376 134.644298 138.936032
    ##                dplyr database land 144.663646 157.888635 159.509233
    ##      median         uq        max neval
    ##   42.569503  45.367514  48.169572     5
    ##   22.660041  23.137450  27.992258     5
    ##   14.011730  14.391393  16.106387     5
    ##    1.726648   1.837266   1.848727     5
    ##    1.358937   1.399255   1.480136     5
    ##   42.004061  42.401129 137.580626     5
    ##   24.624985  26.963480  29.482420     5
    ##   34.130745  39.546414  40.354563     5
    ##  131.620742 151.256695 155.558586     5
    ##  141.678855 143.204211 143.804420     5
    ##  159.416323 163.357441 172.220119     5

![](QTiming3_files/figure-markdown_github/timings-4.png)

    ## [1] 10000
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter 256.465431 257.444848 260.744735
    ##               data.table in memory 182.177043 183.906715 187.066329
    ##         base R tabular calculation 127.359266 130.332398 158.811335
    ##      base R sequential calculation  12.827291  13.150738  13.851750
    ##          base R cframe calculation   7.855752   9.192164   9.872924
    ##  rquery from memory to db and back 115.778987 116.608576 116.913628
    ##              rquery database count  63.764691  64.259320  65.576305
    ##               rquery database land  76.966643  77.645038  81.557048
    ##   dplyr from memory to db and back 235.385601 237.401861 240.061989
    ##               dplyr database count 174.345200 181.468843 181.664235
    ##                dplyr database land 208.837252 210.060864 210.923624
    ##      median        uq       max neval
    ##  260.804824 263.70363 265.30494     5
    ##  185.434052 190.03725 193.77658     5
    ##  131.281632 198.17053 206.91285     5
    ##   13.957632  14.34775  14.97534     5
    ##    9.520451  11.32124  11.47501     5
    ##  116.788069 117.54099 117.85151     5
    ##   65.584429  67.04636  67.22672     5
    ##   81.962718  82.75151  88.45933     5
    ##  238.188394 241.01114 248.32295     5
    ##  182.905855 183.89199 185.70928     5
    ##  210.910076 212.21544 212.59449     5

![](QTiming3_files/figure-markdown_github/timings-5.png)

    ## [1] 1e+05
    ## Unit: milliseconds
    ##                               expr        min         lq      mean
    ##  dplyr in memory no grouped filter 2879.14798 2920.53859 3003.2275
    ##               data.table in memory 1971.27455 1975.51057 2000.4651
    ##         base R tabular calculation 1984.89104 2031.93807 2157.3666
    ##      base R sequential calculation  179.71942  183.04959  233.0356
    ##          base R cframe calculation   93.43596   95.40505  143.2274
    ##  rquery from memory to db and back 1598.50345 1598.97737 1623.3120
    ##              rquery database count 1175.44348 1181.35413 1186.5448
    ##               rquery database land 1234.30287 1237.12849 1260.3615
    ##   dplyr from memory to db and back 2113.30692 2115.53313 2137.2131
    ##               dplyr database count 1619.63292 1622.76326 1634.4709
    ##                dplyr database land 1694.17816 1708.75105 1736.1157
    ##     median        uq       max neval
    ##  2965.4724 3001.2682 3249.7102     5
    ##  2007.4036 2014.8945 2033.2421     5
    ##  2089.2053 2104.6431 2576.1557     5
    ##   225.4960  277.9607  298.9521     5
    ##   145.9071  187.1560  194.2326     5
    ##  1602.0145 1615.4108 1701.6538     5
    ##  1182.8001 1188.4573 1204.6688     5
    ##  1257.5454 1286.2811 1286.5493     5
    ##  2140.1551 2140.6551 2176.4152     5
    ##  1625.0999 1639.6180 1665.2404     5
    ##  1711.8927 1716.4594 1849.2972     5

![](QTiming3_files/figure-markdown_github/timings-6.png)

    ## [1] 1e+06
    ## Unit: seconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 33.929307 34.261962 34.734730 34.269795
    ##               data.table in memory 19.237717 19.276145 19.393562 19.403852
    ##         base R tabular calculation 23.248868 23.260405 23.740916 23.378683
    ##      base R sequential calculation  2.348706  2.517009  2.505155  2.519522
    ##          base R cframe calculation  1.162829  1.217960  1.330591  1.402444
    ##  rquery from memory to db and back 17.424365 17.451762 17.478221 17.463684
    ##              rquery database count 13.104036 13.110209 13.136386 13.122121
    ##               rquery database land 13.573986 13.577371 13.678281 13.617569
    ##   dplyr from memory to db and back 23.027015 23.240760 23.706608 23.500951
    ##               dplyr database count 18.665066 18.694470 18.836521 18.740129
    ##                dplyr database land 19.156754 19.162376 19.202321 19.203780
    ##         uq       max neval
    ##  34.708069 36.504516     5
    ##  19.429334 19.620763     5
    ##  24.145061 24.671560     5
    ##   2.567034  2.573504     5
    ##   1.410289  1.459434     5
    ##  17.486227 17.565068     5
    ##  13.125642 13.219922     5
    ##  13.768933 13.853544     5
    ##  24.285898 24.478416     5
    ##  18.786367 19.296570     5
    ##  19.241818 19.246877     5

![](QTiming3_files/figure-markdown_github/timings-7.png)

``` r
saveRDS(timings, "qtimings3.RDS")
```

``` r
sessionInfo()
```

    ## R version 3.2.3 (2015-12-10)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 16.04.3 LTS
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] bindrcpp_0.2         ggplot2_2.2.1        microbenchmark_1.4-3
    ## [4] dplyr_0.7.4          rquery_0.2.0         cdata_0.5.1         
    ## [7] wrapr_1.1.0         
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.14        dbplyr_1.2.0        pillar_1.0.1       
    ##  [4] plyr_1.8.4          bindr_0.1           tools_3.2.3        
    ##  [7] RPostgres_1.0-4     digest_0.6.13       bit_1.1-12         
    ## [10] evaluate_0.10.1     tibble_1.4.1        gtable_0.2.0       
    ## [13] pkgconfig_2.0.1     rlang_0.1.6         cli_1.0.0          
    ## [16] DBI_0.7             yaml_2.1.16         withr_2.1.1        
    ## [19] stringr_1.2.0       knitr_1.18          hms_0.4.0          
    ## [22] tidyselect_0.2.3    rprojroot_1.3-2     bit64_0.9-7        
    ## [25] grid_3.2.3          data.table_1.10.4-3 glue_1.2.0         
    ## [28] R6_2.2.2            rmarkdown_1.8       purrr_0.2.4        
    ## [31] blob_1.1.0          magrittr_1.5        backports_1.1.2    
    ## [34] scales_0.5.0        htmltools_0.3.6     assertthat_0.2.0   
    ## [37] colorspace_1.3-2    utf8_1.1.3          stringi_1.1.6      
    ## [40] lazyeval_0.2.1      munsell_0.4.3       crayon_1.3.4

``` r
winvector_temp_db_handle <- NULL
if(!is.null(db)) {
  DBI::dbDisconnect(db)
  db <- NULL
}
```
