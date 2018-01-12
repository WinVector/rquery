QTiming
================
Win-Vector LLC
1/10/2018

Let's time [`rquery`](https://winvector.github.io/rquery/), [`dplyr`](https://CRAN.R-project.org/package=dplyr), and [`data.table`](https://CRAN.R-project.org/package=data.table) on a non-trivial example.

These timings are on an late 2014 Mac Mini with 8GB of RAM running OSX 10.12.6, version 3.4.3 (2017-11-30) -- "Kite-Eating Tree", and the current (2018-01-07) CRAN versions of all packages (except `rquery`, which is not yet up on CRAN). We are getting database services from PostgreSQL version `9.6.1` in a docker container.

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
    ## 1       0_1 withdrawal behavior               5
    ## 2       0_1 positive re-framing               2
    ## 3       0_2 withdrawal behavior               3
    ## 4       0_2 positive re-framing               4
    ## 5       1_1 withdrawal behavior               5
    ## 6       1_1 positive re-framing               2

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
head(base_R_sequential_calculation())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3       1_1 withdrawal behavior   0.6706221
    ## 4       1_2 positive re-framing   0.5589742
    ## 5       2_1 withdrawal behavior   0.6706221
    ## 6       2_2 positive re-framing   0.5589742

``` r
head(base_R_row_calculation())
```

    ##    subjectID           diagnosis probability
    ## 1        0_1 withdrawal behavior   0.6706221
    ## 4        0_2 positive re-framing   0.5589742
    ## 5        1_1 withdrawal behavior   0.6706221
    ## 8        1_2 positive re-framing   0.5589742
    ## 9        2_1 withdrawal behavior   0.6706221
    ## 12       2_2 positive re-framing   0.5589742

``` r
head(base_R_tabular_calculation())
```

    ##    subjectID           diagnosis probability
    ## 1        0_1 withdrawal behavior   0.6706221
    ## 4        0_2 positive re-framing   0.5589742
    ## 5        1_1 withdrawal behavior   0.6706221
    ## 8        1_2 positive re-framing   0.5589742
    ## 9        2_1 withdrawal behavior   0.6706221
    ## 12       2_2 positive re-framing   0.5589742

``` r
head(dplyr_local())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1_1       withdrawal behavior       0.671
    ## 4 1_2       positive re-framing       0.559
    ## 5 2_1       withdrawal behavior       0.671
    ## 6 2_2       positive re-framing       0.559

``` r
head(dplyr_tbl())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1_1       withdrawal behavior       0.671
    ## 4 1_2       positive re-framing       0.559
    ## 5 2_1       withdrawal behavior       0.671
    ## 6 2_2       positive re-framing       0.559

``` r
head(dplyr_local_no_grouped_filter())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1_1       withdrawal behavior       0.671
    ## 4 1_2       positive re-framing       0.559
    ## 5 2_1       withdrawal behavior       0.671
    ## 6 2_2       positive re-framing       0.559

``` r
head(data.table_local())
```

    ##    subjectID           diagnosis probability
    ## 1:       0_1 withdrawal behavior   0.6706221
    ## 2:       0_2 positive re-framing   0.5589742
    ## 3:       1_1 withdrawal behavior   0.6706221
    ## 4:       1_2 positive re-framing   0.5589742
    ## 5:       2_1 withdrawal behavior   0.6706221
    ## 6:       2_2 positive re-framing   0.5589742

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
    "base R sequential calculation" =  bquote({nrow(base_R_sequential_calculation())})
)

prune <- FALSE

for(nrep in c(1, 10, 100, 1000, 10000, 100000, 1000000)) {
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
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 20081.843 21950.741 22634.161 22423.122
    ##               data.table in memory  3215.362  3256.698  4273.250  3956.023
    ##         base R tabular calculation  2676.416  2680.018  3617.027  2970.067
    ##      base R sequential calculation   951.693  1017.004  1285.237  1038.503
    ##         uq       max neval
    ##  23013.445 25701.655     5
    ##   5180.384  5757.783     5
    ##   3104.936  6653.699     5
    ##   1157.469  2261.515     5

![](QTiming_files/figure-markdown_github/timings-1.png)

    ## [1] 10
    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 18.660366 19.722157 20.358343 20.916360
    ##               data.table in memory  3.110576  3.197681  3.352857  3.228199
    ##         base R tabular calculation  2.647719  2.670854  2.829938  2.781028
    ##      base R sequential calculation  1.012027  1.014694  1.134927  1.080395
    ##         uq       max neval
    ##  20.964506 21.528325     5
    ##   3.555649  3.672182     5
    ##   2.825525  3.224562     5
    ##   1.086058  1.481460     5

![](QTiming_files/figure-markdown_github/timings-2.png)

    ## [1] 100
    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 22.669109 23.592071 24.015004 23.999698
    ##               data.table in memory  5.119279  5.181336  7.087883  5.451549
    ##         base R tabular calculation  5.058928  5.147611  5.284115  5.299227
    ##      base R sequential calculation  1.409780  1.466084  1.522841  1.565979
    ##         uq       max neval
    ##  24.609760 25.204384     5
    ##   5.527909 14.159344     5
    ##   5.303029  5.611782     5
    ##   1.570969  1.601392     5

![](QTiming_files/figure-markdown_github/timings-3.png)

    ## [1] 1000
    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 85.754280 85.929779 94.654138 88.969166
    ##               data.table in memory 24.379719 26.399666 27.081402 26.459844
    ##         base R tabular calculation 36.763997 36.788362 38.937554 38.891825
    ##      base R sequential calculation  7.572615  7.880037  8.231179  8.159244
    ##         uq        max neval
    ##  94.866048 117.751419     5
    ##  28.742125  29.425656     5
    ##  39.006449  43.237138     5
    ##   8.661103   8.882894     5

![](QTiming_files/figure-markdown_github/timings-4.png)

    ## [1] 10000
    ## Unit: milliseconds
    ##                               expr      min       lq     mean   median
    ##  dplyr in memory no grouped filter 816.4125 822.9419 826.4549 829.3220
    ##               data.table in memory 239.9854 240.6386 254.8661 242.7468
    ##         base R tabular calculation 501.2060 558.1601 565.6389 562.5540
    ##      base R sequential calculation 104.6783 108.9845 109.4886 110.2807
    ##        uq      max neval
    ##  829.7719 833.8263     5
    ##  245.5178 305.4421     5
    ##  576.7551 629.5195     5
    ##  110.6984 112.8009     5

![](QTiming_files/figure-markdown_github/timings-5.png)

    ## [1] 1e+05
    ## Unit: seconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 10.273704 10.404244 10.548351 10.444627
    ##               data.table in memory  2.593409  2.631182  2.689273  2.713473
    ##         base R tabular calculation  6.811532  6.943840  6.990008  6.956764
    ##      base R sequential calculation  1.471420  1.483063  1.552604  1.551256
    ##         uq       max neval
    ##  10.665635 10.953545     5
    ##   2.742851  2.765448     5
    ##   7.105706  7.132200     5
    ##   1.619016  1.638265     5

![](QTiming_files/figure-markdown_github/timings-6.png)

    ## [1] 1e+06
    ## Unit: seconds
    ##                               expr       min        lq      mean    median
    ##  dplyr in memory no grouped filter 130.81599 131.09589 132.43224 131.78721
    ##               data.table in memory  28.62938  28.65105  30.51792  28.72840
    ##         base R tabular calculation  83.28940  87.21334  88.45402  88.40691
    ##      base R sequential calculation  21.55591  21.73302  21.98456  22.03053
    ##         uq       max neval
    ##  133.03484 135.42728     5
    ##   32.04523  34.53553     5
    ##   89.41110  93.94936     5
    ##   22.29596  22.30735     5

![](QTiming_files/figure-markdown_github/timings-7.png)

``` r
saveRDS(timings, "qtimings.RDS")
```

`rquery` appears to be relatively fast. Some of the time for "`rquery` local" is because `rquery` doesn't *really* have a local mode, it has to copy the data to the database and back in that case.

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
    ##  [1] Rcpp_0.12.14.2      knitr_1.18          bindr_0.1          
    ##  [4] magrittr_1.5        munsell_0.4.3       colorspace_1.3-2   
    ##  [7] R6_2.2.2            rlang_0.1.6         plyr_1.8.4         
    ## [10] stringr_1.2.0       tools_3.4.3         grid_3.4.3         
    ## [13] data.table_1.10.4-3 gtable_0.2.0        utf8_1.1.3         
    ## [16] cli_1.0.0           htmltools_0.3.6     lazyeval_0.2.1     
    ## [19] yaml_2.1.16         rprojroot_1.3-2     digest_0.6.13      
    ## [22] assertthat_0.2.0    tibble_1.4.1        crayon_1.3.4       
    ## [25] glue_1.2.0          evaluate_0.10.1     rmarkdown_1.8      
    ## [28] stringi_1.1.6       compiler_3.4.3      pillar_1.0.1       
    ## [31] scales_0.5.0        backports_1.1.2     pkgconfig_2.0.1

``` r
winvector_temp_db_handle <- NULL
if(!is.null(db)) {
  DBI::dbDisconnect(db)
  db <- NULL
}
```
