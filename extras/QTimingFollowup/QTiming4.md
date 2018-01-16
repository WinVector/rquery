QTiming4
================
Win-Vector LLC
1/15/2018

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
# db <- DBI::dbConnect(RPostgres::Postgres(),
#                      host = 'localhost',
#                      port = 5432,
#                      user = 'postgres',
#                      password = 'pg')
if(!is.null(db)) {
  winvector_temp_db_handle <- list(db = db)
  print(db)
  DBI::dbGetQuery(db, "SELECT version()", stringsAsFactors = FALSE)
}

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
    ## 1        s1 withdrawal behavior               3
    ## 2        s1 positive re-framing               8
    ## 3       s10 withdrawal behavior               9
    ## 4       s10 positive re-framing               1
    ## 5        s2 withdrawal behavior               3
    ## 6        s2 positive re-framing               5

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
    ## 1        s1 positive re-framing   0.7658456
    ## 2       s10 withdrawal behavior   0.8694381
    ## 3        s2 positive re-framing   0.6163301
    ## 4        s3 positive re-framing   0.6706221
    ## 5        s4 positive re-framing   0.5000000
    ## 6        s5 positive re-framing   0.6163301

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

# From: http://www.win-vector.com/blog/2018/01/base-r-can-be-fast/#comment-66751
data.table_local4 <- function(dLocal) {
 dDT <- data.table::data.table(dLocal)
 setnames(dDT, "surveyCategory", "diagnosis")
 dDT[,expaTs:=exp(assessmentTotal*scale)]
 dDT[,sum_expaTs:=sum(expaTs),subjectID] # precalculate -> this uses gsum internally
 dDT[,probability := expaTs / sum_expaTs]
 dDT[,c("assessmentTotal","expaTs","sum_expaTs"):=NULL]
 setorder(dDT, subjectID, -probability, diagnosis)
 dDT[,.SD[1],subjectID]
}

if(!equiv_res(check, data.table_local4(dLocal))) {
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

Now let's measure the speeds with `microbenchmark`.

``` r
timings <- NULL

# improved code from:
# http://www.win-vector.com/blog/2018/01/base-r-can-be-fast/#comment-66746
data.table_local3 <- function() {
  dDT <- data.table::data.table(dLocal)
  dDT <- dDT[,list(diagnosis = surveyCategory,
                   probability = exp (assessmentTotal * scale ) /
                     sum ( exp ( assessmentTotal * scale ) ))
             ,subjectID ]
  setorder(dDT, subjectID, probability, -diagnosis)
  dDT <- dDT[,.SD[.N],subjectID]
  setorder(dDT, subjectID)
}

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
    "data.table in memory" =  bquote({nrow(data.table_local4(dLocal))}),
    # "base R row calculation" =  bquote({nrow(base_R_row_calculation())}),
    "base R tabular calculation" =  bquote({nrow(base_R_tabular_calculation())}),
    # "base R sequential calculation" =  bquote({nrow(base_R_sequential_calculation())})
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
  print(paste("nrep:", nrep))
  dLocal <- mkData(nrep)
  print(paste("rows:", nrow(dLocal)))
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
  plt <- autoplot(tm) + ggtitle(paste0("rows: ", nrow(dLocal)))
  print(plt)
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

    ## [1] "nrep: 1"
    ## [1] "rows: 2"
    ## Unit: microseconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 17093.711 18502.633 20873.779 18885.434
    ##               data.table in memory  2398.241  2461.082  2838.094  2461.398
    ##         base R tabular calculation  2174.808  2181.188  2578.358  2303.722
    ##          base R cframe calculation   677.162   712.871  1095.345  1058.710
    ##         uq       max neval
    ##  22898.726 26988.391     5
    ##   3063.820  3805.928     5
    ##   2330.217  3901.855     5
    ##   1281.909  1746.075     5

![](QTiming4_files/figure-markdown_github/timings-1.png)

    ## [1] "nrep: 10"
    ## [1] "rows: 20"
    ## Unit: microseconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 18103.938 23419.238 23386.661 24279.069
    ##               data.table in memory  2642.359  3085.040  3201.963  3266.684
    ##         base R tabular calculation  2692.558  3144.201  3407.787  3374.973
    ##          base R cframe calculation   726.014  1072.098  1075.301  1111.188
    ##         uq       max neval
    ##  25208.984 25922.075     5
    ##   3316.276  3699.456     5
    ##   3477.826  4349.377     5
    ##   1157.387  1309.818     5

![](QTiming4_files/figure-markdown_github/timings-2.png)

    ## [1] "nrep: 100"
    ## [1] "rows: 200"
    ## Unit: microseconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 22284.477 22516.579 25150.174 24497.526
    ##               data.table in memory  3221.646  3254.257  3428.856  3430.565
    ##         base R tabular calculation  3888.089  4323.419  5325.524  5992.453
    ##          base R cframe calculation   936.452   994.121  1142.918  1181.963
    ##         uq       max neval
    ##  28092.619 28359.668     5
    ##   3556.454  3681.357     5
    ##   6095.100  6328.557     5
    ##   1263.678  1338.374     5

![](QTiming4_files/figure-markdown_github/timings-3.png)

    ## [1] "nrep: 1000"
    ## [1] "rows: 2000"
    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 51.674285 77.150215 75.517975 81.633480
    ##               data.table in memory  3.339440  3.474448  4.040997  4.318096
    ##         base R tabular calculation 18.842085 19.221082 23.318909 21.780901
    ##          base R cframe calculation  1.193218  1.368413  1.443241  1.380113
    ##        uq       max neval
    ##  82.51997 84.611928     5
    ##   4.35286  4.720141     5
    ##  27.78488 28.965598     5
    ##   1.40330  1.871162     5

![](QTiming4_files/figure-markdown_github/timings-4.png)

    ## [1] "nrep: 10000"
    ## [1] "rows: 20000"
    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##  dplyr in memory no grouped filter 401.330091 434.587584 480.510831
    ##               data.table in memory   9.889524  12.812629  15.485638
    ##         base R tabular calculation 223.716139 257.465046 292.931891
    ##          base R cframe calculation   6.575244   6.707435   9.369459
    ##     median        uq       max neval
    ##  479.98477 483.86224 602.78948     5
    ##   13.94164  20.20621  20.57819     5
    ##  300.67289 338.20146 344.60392     5
    ##    9.78236  11.01194  12.77032     5

![](QTiming4_files/figure-markdown_github/timings-5.png)

    ## [1] "nrep: 1e+05"
    ## [1] "rows: 200000"
    ## Unit: milliseconds
    ##                               expr        min         lq      mean
    ##  dplyr in memory no grouped filter 4931.53225 4963.63557 5651.3629
    ##               data.table in memory   70.09192   73.99669  104.3876
    ##         base R tabular calculation 3087.54650 3175.19333 3249.3754
    ##          base R cframe calculation   95.02364  101.13144  179.5916
    ##      median        uq       max neval
    ##  5178.66185 5911.1958 7271.7890     5
    ##    86.96318  101.8792  189.0069     5
    ##  3228.73675 3230.2300 3525.1706     5
    ##   190.81691  250.2387  260.7472     5

![](QTiming4_files/figure-markdown_github/timings-6.png)

    ## [1] "nrep: 1e+06"
    ## [1] "rows: 2000000"
    ## Unit: milliseconds
    ##                               expr        min         lq      mean
    ##  dplyr in memory no grouped filter 58101.8360 58388.8882 61621.171
    ##               data.table in memory   873.8924   957.7749  1041.705
    ##         base R tabular calculation 34946.6670 36124.8858 36788.736
    ##          base R cframe calculation  1399.6995  1657.8473  1733.163
    ##     median        uq       max neval
    ##  58509.173 66193.804 66912.155     5
    ##   1054.780  1128.150  1193.927     5
    ##  36151.583 36491.518 40229.027     5
    ##   1744.881  1750.348  2113.041     5

![](QTiming4_files/figure-markdown_github/timings-7.png)

``` r
saveRDS(timings, "qtimings4.RDS")
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
    ##  [1] Rcpp_0.12.14.2   knitr_1.18       bindr_0.1        magrittr_1.5    
    ##  [5] munsell_0.4.3    colorspace_1.3-2 R6_2.2.2         rlang_0.1.6     
    ##  [9] plyr_1.8.4       stringr_1.2.0    tools_3.4.3      grid_3.4.3      
    ## [13] gtable_0.2.0     htmltools_0.3.6  lazyeval_0.2.1   yaml_2.1.16     
    ## [17] rprojroot_1.3-2  digest_0.6.13    assertthat_0.2.0 tibble_1.4.1    
    ## [21] glue_1.2.0       evaluate_0.10.1  rmarkdown_1.8    stringi_1.1.6   
    ## [25] compiler_3.4.3   pillar_1.0.1     scales_0.5.0     backports_1.1.2 
    ## [29] pkgconfig_2.0.1

``` r
winvector_temp_db_handle <- NULL
if(!is.null(db)) {
  DBI::dbDisconnect(db)
  db <- NULL
}
```
