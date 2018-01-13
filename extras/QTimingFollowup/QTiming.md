QTiming
================
Win-Vector LLC
1/10/2018

Let's time [`rquery`](https://winvector.github.io/rquery/), [`dplyr`](https://CRAN.R-project.org/package=dplyr), and [`data.table`](https://CRAN.R-project.org/package=data.table) on a non-trivial example.

These timings are on a late 2014 Mac Mini with 8GB of RAM running OSX 10.12.6, R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree", and the current (2018-01-07) CRAN versions of all packages (except `rquery`, which is not yet up on CRAN). We are getting database services from PostgreSQL version `9.6.1` in a docker container.

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
    ## 1        s1 withdrawal behavior               1
    ## 2        s1 positive re-framing               1
    ## 3       s10 withdrawal behavior               7
    ## 4       s10 positive re-framing               6
    ## 5        s2 withdrawal behavior               6
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
    ##  $ assessmentTotal: num  1 1 7 6 6 0 0 8 5 4
    ## Observations: NA
    ## Variables: 3
    ## $ subjectID       <chr> "s1", "s1", "s10", "s10", "s2", "s2", "s3", "s...
    ## $ surveyCategory  <chr> "withdrawal behavior", "positive re-framing", ...
    ## $ assessmentTotal <dbl> 1, 1, 7, 6, 6, 0, 0, 8, 5, 4, 3, 9, 6, 7, 1, 6...

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
    ## 1        s1 positive re-framing   0.5000000
    ## 2       s10 withdrawal behavior   0.5589742
    ## 3        s2 withdrawal behavior   0.8056518
    ## 4        s3 positive re-framing   0.8694381
    ## 5        s4 withdrawal behavior   0.5589742
    ## 6        s5 positive re-framing   0.8056518

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
  # not breaking ties yet
  #stop("mismatch")
}
```

    ## NULL

``` r
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
    ## 1 s1        positive re-framing       0.500
    ## 2 s10       withdrawal behavior       0.559
    ## 3 s2        withdrawal behavior       0.806
    ## 4 s3        positive re-framing       0.869
    ## 5 s4        withdrawal behavior       0.559
    ## 6 s5        positive re-framing       0.806

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
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter  18046.328  20941.958  24417.268
    ##               data.table in memory   2864.686   3063.782   3189.346
    ##         base R tabular calculation   2501.790   3722.323   3909.184
    ##      base R sequential calculation   1053.912   1110.810   1797.853
    ##          base R cframe calculation    691.185    695.957   1498.591
    ##  rquery from memory to db and back  35982.475  36643.104  42428.500
    ##              rquery database count  20029.841  21790.684  23786.861
    ##               rquery database land  27687.687  27897.720  29981.035
    ##   dplyr from memory to db and back 129928.792 143390.798 183965.084
    ##               dplyr database count 122310.141 128222.790 174072.323
    ##                dplyr database land 146101.862 156609.556 171654.809
    ##      median         uq        max neval
    ##   23408.892  23759.023  35930.141     5
    ##    3199.830   3236.977   3581.455     5
    ##    4388.859   4407.765   4525.181     5
    ##    1589.282   2077.995   3157.267     5
    ##     874.088    885.720   4346.007     5
    ##   37545.245  46171.825  55799.852     5
    ##   21815.663  24512.974  30785.143     5
    ##   30623.241  31004.575  32691.951     5
    ##  164116.296 209706.358 272683.174     5
    ##  132420.628 233712.202 253695.856     5
    ##  159315.602 160079.006 236168.019     5

![](QTiming_files/figure-markdown_github/timings-1.png)

    ## [1] 10
    ## Unit: microseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter  18191.589  19584.877  24112.543
    ##               data.table in memory   3344.846   3380.123   4002.435
    ##         base R tabular calculation   2651.852   2834.210   4167.003
    ##      base R sequential calculation    963.785   1029.829   1132.864
    ##          base R cframe calculation    695.073    747.451    979.529
    ##  rquery from memory to db and back  37143.062  37272.185  50031.720
    ##              rquery database count  21277.565  21779.921  25207.986
    ##               rquery database land  29487.845  31649.779  40837.068
    ##   dplyr from memory to db and back 132134.075 133425.744 169085.608
    ##               dplyr database count 120939.985 125892.606 127486.834
    ##                dplyr database land 147716.088 151678.587 153377.675
    ##      median         uq        max neval
    ##   21628.025  24448.321  36709.902     5
    ##    3465.800   4321.195   5500.211     5
    ##    4842.650   5225.172   5281.133     5
    ##    1053.553   1291.312   1325.843     5
    ##     841.670    872.910   1740.541     5
    ##   44275.075  65597.107  65871.171     5
    ##   22428.271  24822.316  35731.857     5
    ##   42766.164  46945.851  53335.701     5
    ##  138642.879 199275.852 241949.491     5
    ##  127302.839 130273.302 133025.436     5
    ##  152740.357 156260.841 158492.500     5

![](QTiming_files/figure-markdown_github/timings-2.png)

    ## [1] 100
    ## Unit: microseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter  21429.195  22818.567  22741.430
    ##               data.table in memory   4101.406   4227.712   4789.217
    ##         base R tabular calculation   3984.105   4416.592   4509.039
    ##      base R sequential calculation   1198.215   1300.266   1588.657
    ##          base R cframe calculation    835.410    906.730   1015.011
    ##  rquery from memory to db and back  36981.814  37428.178  42231.289
    ##              rquery database count  20711.384  20818.273  21703.468
    ##               rquery database land  27921.518  29743.112  30561.667
    ##   dplyr from memory to db and back 131045.298 132154.282 152949.028
    ##               dplyr database count 122614.440 124556.402 131997.668
    ##                dplyr database land 148018.993 150224.272 177790.712
    ##      median         uq        max neval
    ##   23072.163  23135.643  23251.582     5
    ##    4242.677   4994.076   6380.216     5
    ##    4424.144   4596.946   5123.407     5
    ##    1496.559   1793.072   2155.174     5
    ##    1088.876   1095.246   1148.795     5
    ##   37843.563  38542.675  60360.215     5
    ##   21614.254  22055.188  23318.243     5
    ##   30603.770  32145.192  32394.745     5
    ##  138913.129 157750.728 204881.704     5
    ##  129630.306 129661.429 153525.765     5
    ##  156845.940 214936.037 218928.317     5

![](QTiming_files/figure-markdown_github/timings-3.png)

    ## [1] 1000
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter  53.103629  54.464618  61.957805
    ##               data.table in memory  13.832499  14.056323  14.706166
    ##         base R tabular calculation  17.709795  17.748422  18.833077
    ##      base R sequential calculation   3.025384   3.085611   3.257998
    ##          base R cframe calculation   1.332667   1.361520   1.458609
    ##  rquery from memory to db and back  48.310698  50.024064  51.512469
    ##              rquery database count  26.966328  27.524393  29.205253
    ##               rquery database land  37.155087  37.212426  38.175773
    ##   dplyr from memory to db and back 143.511966 146.373589 164.020321
    ##               dplyr database count 131.070928 132.719237 138.227449
    ##                dplyr database land 158.520885 160.668847 170.932992
    ##      median         uq        max neval
    ##   55.472438  58.598657  88.149684     5
    ##   14.255434  14.324071  17.062503     5
    ##   18.311749  19.168142  21.227278     5
    ##    3.235884   3.297901   3.645210     5
    ##    1.403561   1.429081   1.766218     5
    ##   51.227221  53.802711  54.197653     5
    ##   27.800825  30.284614  33.450105     5
    ##   37.268355  37.918181  41.324816     5
    ##  165.893332 181.927579 182.395137     5
    ##  134.151253 136.685812 156.510013     5
    ##  163.296641 175.399141 196.779445     5

![](QTiming_files/figure-markdown_github/timings-4.png)

    ## [1] 10000
    ## Unit: milliseconds
    ##                               expr       min         lq       mean
    ##  dplyr in memory no grouped filter 417.07736 423.809746 433.496449
    ##               data.table in memory 117.64531 119.670564 137.596930
    ##         base R tabular calculation 203.75131 209.129398 226.883959
    ##      base R sequential calculation  29.30829  31.926063  32.421174
    ##          base R cframe calculation   5.69294   6.536227   7.340465
    ##  rquery from memory to db and back 159.50981 159.654650 161.761611
    ##              rquery database count  83.18851  83.695954  84.750636
    ##               rquery database land  97.43994  99.071193 102.436663
    ##   dplyr from memory to db and back 309.37640 310.474102 326.056321
    ##               dplyr database count 223.55948 225.936423 229.927927
    ##                dplyr database land 260.71526 261.714280 263.524193
    ##     median         uq        max neval
    ##  424.38728 448.327900 453.879963     5
    ##  122.26605 122.334102 206.068624     5
    ##  212.74565 216.115676 292.677769     5
    ##   32.87189  33.215226  34.784399     5
    ##    6.55398   8.555895   9.363282     5
    ##  160.71006 162.118011 166.815522     5
    ##   83.88329  84.999377  87.986053     5
    ##  100.48802 101.849818 113.334343     5
    ##  313.70411 316.966671 379.760324     5
    ##  226.85928 227.372598 245.911853     5
    ##  264.49680 265.030882 265.663749     5

![](QTiming_files/figure-markdown_github/timings-5.png)

    ## [1] 1e+05
    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 4890.6142 4903.9855 4915.1301 4907.8422
    ##               data.table in memory 1177.5073 1235.8423 1268.5115 1245.4268
    ##         base R tabular calculation 3239.9722 3323.7404 3354.2935 3364.6357
    ##      base R sequential calculation  423.2129  443.8643  458.5600  443.9370
    ##          base R cframe calculation  101.4658  121.1013  169.3462  199.9944
    ##  rquery from memory to db and back 2999.9253 3012.0484 3117.2299 3109.2258
    ##              rquery database count 2319.4976 2325.5796 2337.2615 2327.9461
    ##               rquery database land 2404.0984 2440.7502 2451.9615 2458.9259
    ##   dplyr from memory to db and back 3319.6991 3328.6280 3389.7345 3359.5280
    ##               dplyr database count 2614.6398 2663.7003 2663.4188 2672.5836
    ##                dplyr database land 2727.6791 2741.9322 2750.8925 2746.0466
    ##         uq       max neval
    ##  4934.4934 4938.7152     5
    ##  1253.7009 1430.0800     5
    ##  3396.9792 3446.1398     5
    ##   448.7025  533.0834     5
    ##   210.0784  214.0912     5
    ##  3224.2558 3240.6944     5
    ##  2338.3756 2374.9084     5
    ##  2467.8510 2488.1824     5
    ##  3428.5608 3512.2568     5
    ##  2676.3158 2689.8547     5
    ##  2753.3563 2785.4482     5

![](QTiming_files/figure-markdown_github/timings-6.png)

    ## [1] 1e+06
    ## Unit: seconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 59.655281 61.168900 62.758946 61.780330
    ##               data.table in memory 11.919399 12.129517 12.859142 13.112466
    ##         base R tabular calculation 35.782201 36.092755 36.653868 36.415260
    ##      base R sequential calculation  5.214197  5.334990  5.552545  5.441256
    ##          base R cframe calculation  1.359733  1.661497  1.697051  1.734769
    ##  rquery from memory to db and back 34.764620 34.884616 37.062427 38.029169
    ##              rquery database count 27.377490 27.546566 27.861941 27.608988
    ##               rquery database land 28.337746 28.489709 30.223238 30.532050
    ##   dplyr from memory to db and back 39.610203 39.848296 40.685087 39.928662
    ##               dplyr database count 33.281848 33.309473 34.307129 33.439757
    ##                dplyr database land 33.693857 33.746575 35.454239 34.003970
    ##         uq       max neval
    ##  64.695723 66.494498     5
    ##  13.447835 13.686493     5
    ##  36.837290 38.141833     5
    ##   5.826793  5.945491     5
    ##   1.824060  1.905199     5
    ##  38.735430 38.898299     5
    ##  27.857822 28.918841     5
    ##  31.389493 32.367192     5
    ##  40.557993 43.480281     5
    ##  34.351992 37.152576     5
    ##  37.826212 38.000583     5

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
    ## [7] wrapr_1.1.1         
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.14.2      dbplyr_1.2.0        pillar_1.0.1       
    ##  [4] compiler_3.4.3      plyr_1.8.4          bindr_0.1          
    ##  [7] tools_3.4.3         RPostgres_1.0-4     digest_0.6.13      
    ## [10] bit_1.1-12          evaluate_0.10.1     tibble_1.4.1       
    ## [13] gtable_0.2.0        pkgconfig_2.0.1     rlang_0.1.6        
    ## [16] cli_1.0.0           DBI_0.7             yaml_2.1.16        
    ## [19] withr_2.1.1         stringr_1.2.0       knitr_1.18         
    ## [22] hms_0.4.0           tidyselect_0.2.3    rprojroot_1.3-2    
    ## [25] bit64_0.9-7         grid_3.4.3          data.table_1.10.4-3
    ## [28] glue_1.2.0          R6_2.2.2            rmarkdown_1.8      
    ## [31] purrr_0.2.4         blob_1.1.0          magrittr_1.5       
    ## [34] backports_1.1.2     scales_0.5.0        htmltools_0.3.6    
    ## [37] assertthat_0.2.0    colorspace_1.3-2    utf8_1.1.3         
    ## [40] stringi_1.1.6       lazyeval_0.2.1      munsell_0.4.3      
    ## [43] crayon_1.3.4

``` r
winvector_temp_db_handle <- NULL
if(!is.null(db)) {
  DBI::dbDisconnect(db)
  db <- NULL
}
```
