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

db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'postgres',
                     password = 'pg')
winvector_temp_db_handle <- list(db = db)

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
print(db)
```

    ## <PqConnection> postgres@localhost:5432

``` r
DBI::dbGetQuery(db, "SELECT version()")
```

    ##                                                                                    version
    ## 1 PostgreSQL 9.6.1 on x86_64-pc-linux-gnu, compiled by gcc (Debian 4.9.2-10) 4.9.2, 64-bit

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
nrep <- 10000


dLocal <- data.frame(
  subjectID = c(1,                   
                1,
                2,                   
                2),
  surveyCategory = c(
    'withdrawal behavior',
    'positive re-framing',
    'withdrawal behavior',
    'positive re-framing'
  ),
  assessmentTotal = c(5,                 
                      2,
                      3,                  
                      4),
  stringsAsFactors = FALSE)
norig <- nrow(dLocal)
dLocal <- dLocal[rep(seq_len(norig), nrep), , drop=FALSE]
dLocal$subjectID <- paste((seq_len(nrow(dLocal)) -1)%/% norig,
                          dLocal$subjectID, 
                          sep = "_")
rownames(dLocal) <- NULL
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
dR <- rquery::dbi_copy_to(db, 'dR',
                          dLocal,
                          temporary = TRUE, 
                          overwrite = TRUE)
cdata::qlook(db, dR$table_name)
```

    ## table "dR" PqConnection 
    ##  nrow: 40000 
    ##  NOTE: "obs" below is count of sample, not number of rows of data.
    ## 'data.frame':    10 obs. of  3 variables:
    ##  $ subjectID      : chr  "0_1" "0_1" "0_2" "0_2" ...
    ##  $ surveyCategory : chr  "withdrawal behavior" "positive re-framing" "withdrawal behavior" "positive re-framing" ...
    ##  $ assessmentTotal: num  5 2 3 4 5 2 3 4 5 2

``` r
dTbl <- dplyr::tbl(db, dR$table_name)
dplyr::glimpse(dTbl)
```

    ## Observations: NA
    ## Variables: 3
    ## $ subjectID       <chr> "0_1", "0_1", "0_2", "0_2", "1_1", "1_1", "1_2...
    ## $ surveyCategory  <chr> "withdrawal behavior", "positive re-framing", ...
    ## $ assessmentTotal <dbl> 5, 2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4...

Now we declare our operation pipelines, both on local (in-memory `data.frame`) and remote (already in a database) data.

``` r
scale <- 0.237

# base-R function
# could also try base::split() or base:table()
base_r_calculate_tabular <- function(d) {
  d <- d[order(d$subjectID, d$surveyCategory), , drop=FALSE]
  # compute un-normalized probability
  d$probability <- exp(d$assessmentTotal * scale)
  # set up of for selection
  dmax <- stats::aggregate(d$probability, 
                           by = list(subjectID = d$subjectID), 
                           FUN = max)
  maxv <- dmax$x
  names(maxv) <- dmax$subjectID
  # set up for normalization
  dsum <- stats::aggregate(d$probability, 
                           by = list(subjectID = d$subjectID), 
                           FUN = sum)
  sumv <- dsum$x
  names(sumv) <- dsum$subjectID
  # start selection
  d$maxv <- maxv[d$subjectID]
  d <- d[d$probability >= d$maxv, 
                   , 
                   drop=FALSE]
  # de-dup
  d$rownum <- seq_len(nrow(d))
  drow <-  stats::aggregate(d$rownum, 
                           by = list(subjectID = d$subjectID), 
                           FUN = min)
  minv <- drow$x
  names(minv) <- drow$subjectID
  d$rmin <- minv[d$subjectID]
  d <- d[d$rownum <= d$rmin, , drop=FALSE]
  # renormalize
  d$probability <- d$probability/sumv[d$subjectID]
  d <- d[, c("subjectID", "surveyCategory", "probability")]
  colnames(d)[[2]] <- "diagnosis"
  d
}

# base-R function
# could also try base::split() or base:table()
base_r_calculate_brute_force <- function(d) {
  cats <- base::sort(base::unique(d$surveyCategory))
  res <- NULL
  for(ci in cats) {
    di <- d[d$surveyCategory == ci, , drop=FALSE]
    di <- di[base::order(di$subjectID), , drop=FALSE]
    di$probability <- exp(di$assessmentTotal * scale)
    if(length(base::unique(di$subjectID))!=nrow(di)) {
      stop("base_r_calculate repeated subjectID")
    }
    if(is.null(res)) {
      res <- data.frame(subjectID = di$subjectID,
                        totalProb = di$probability,
                        bestScore = di$probability,
                        diagnosis = ci,
                        stringsAsFactors = FALSE)
    } else {
      if((nrow(di)!=nrow(res)) ||
         (!all(di$subjectID == res$subjectID))) {
        stop("base_r_calculate saw irregular data")
      }
      change <- di$probability > res$bestScore
      res$diagnosis[change] <- ci
      res$bestScore <- base::pmax(res$bestScore, 
                                  di$probability)
      res$totalProb <- res$totalProb + di$probability
    }
  }
  res$probability <- res$bestScore/res$totalProb
  res <- res[, c("subjectID", 
                 "diagnosis", 
                 "probability")]
  res
}

# this is a function, 
# so body not evaluated until used
rquery_pipeline <- . := {
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale)/
               sum(exp(assessmentTotal * scale)),
             count := count(1),
             partitionby = 'subjectID') %.>%
    extend_nse(.,
               rank := rank(),
               partitionby = 'subjectID',
               orderby = c('probability', 'surveyCategory'))  %.>%
    rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
    select_rows_nse(., rank == count) %.>%
    select_columns(., c('subjectID', 
                        'diagnosis', 
                        'probability')) %.>%
    orderby(., 'subjectID') 
}


base_R_brute_calculation <- function() {
  base_r_calculate_brute_force(dLocal)
}

base_R_tabular_calculation <- function() {
  base_r_calculate_tabular(dLocal)
}

rquery_local <- function() {
  dLocal %.>% 
    rquery_pipeline(.) %.>%
    as.data.frame(.) # force execution
}

rquery_database_pull <- function() {
  dR %.>% 
    rquery_pipeline(.) %.>% 
    to_sql(., db) %.>% 
    DBI::dbGetQuery(db, .) %.>%
    as.data.frame(.) # shouldn't be needed
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
    DBI::dbGetQuery(db, .) %.>%
    as.data.frame(.) # shouldn't be needed
}

# this is a function, 
# so body not evaluated until used
dplyr_pipeline <- . %>%
  group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
  arrange(probability, surveyCategory) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID)

# this is a function, 
# so body not evaluated until used
# pipeline re-factored to have filter outside
# mutate 
# work around: https://github.com/tidyverse/dplyr/issues/3294
dplyr_pipeline2 <- . %>%
  group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
  arrange(probability, surveyCategory) %>%
  mutate(count = n(), rank = row_number()) %>%
  ungroup() %>%
  filter(count == rank) %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID)


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

.datatable.aware <- TRUE

data.table_local <- function() {
  dDT <- data.table::data.table(dLocal)
  dDT[
    , one := 1 ][
      , probability := exp ( assessmentTotal * scale ) / 
        sum ( exp ( assessmentTotal * scale ) ) ,subjectID ][
          , count := sum ( one ) ,subjectID ][
            , rank := rank ( probability ) ,subjectID ][
              rank == count ][
                , diagnosis := surveyCategory ][
                  , c('subjectID', 'diagnosis', 'probability') ][
                    order(subjectID) ]
}
```

Let's inspect the functions.

``` r
head(base_R_brute_calculation())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3       1_1 withdrawal behavior   0.6706221
    ## 4       1_2 positive re-framing   0.5589742
    ## 5      10_1 withdrawal behavior   0.6706221
    ## 6      10_2 positive re-framing   0.5589742

``` r
head(base_R_tabular_calculation())
```

    ##    subjectID           diagnosis probability
    ## 1        0_1 withdrawal behavior   0.6706221
    ## 4        0_2 positive re-framing   0.5589742
    ## 5        1_1 withdrawal behavior   0.6706221
    ## 8        1_2 positive re-framing   0.5589742
    ## 41      10_1 withdrawal behavior   0.6706221
    ## 44      10_2 positive re-framing   0.5589742

``` r
head(rquery_local())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3    1000_1 withdrawal behavior   0.6706221
    ## 4    1000_2 positive re-framing   0.5589742
    ## 5     100_1 withdrawal behavior   0.6706221
    ## 6    1001_1 withdrawal behavior   0.6706221

``` r
rquery_database_land()
```

    ## NULL

``` r
head(rquery_database_pull())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3    1000_1 withdrawal behavior   0.6706221
    ## 4    1000_2 positive re-framing   0.5589742
    ## 5     100_1 withdrawal behavior   0.6706221
    ## 6    1001_1 withdrawal behavior   0.6706221

``` r
rquery_database_count()
```

    ##       n
    ## 1 20000

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
    ## 5 10_1      withdrawal behavior       0.671
    ## 6 10_2      positive re-framing       0.559

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
    ## 5 10_1      withdrawal behavior       0.671
    ## 6 10_2      positive re-framing       0.559

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
    ## 5 10_1      withdrawal behavior       0.671
    ## 6 10_2      positive re-framing       0.559

``` r
dplyr_database_land()
```

    ## NULL

``` r
head(dplyr_database_pull())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1000_1    withdrawal behavior       0.671
    ## 4 1000_2    positive re-framing       0.559
    ## 5 100_1     withdrawal behavior       0.671
    ## 6 1001_1    withdrawal behavior       0.671

``` r
dplyr_database_count()
```

    ## # A tibble: 1 x 1
    ##   n              
    ##   <S3: integer64>
    ## 1 20000

``` r
head(dplyr_round_trip())
```

    ## # A tibble: 6 x 3
    ##   subjectID diagnosis           probability
    ##   <chr>     <chr>                     <dbl>
    ## 1 0_1       withdrawal behavior       0.671
    ## 2 0_2       positive re-framing       0.559
    ## 3 1000_1    withdrawal behavior       0.671
    ## 4 1000_2    positive re-framing       0.559
    ## 5 100_1     withdrawal behavior       0.671
    ## 6 1001_1    withdrawal behavior       0.671

``` r
head(data.table_local())
```

    ##    subjectID           diagnosis probability
    ## 1:       0_1 withdrawal behavior   0.6706221
    ## 2:       0_2 positive re-framing   0.5589742
    ## 3:    1000_1 withdrawal behavior   0.6706221
    ## 4:    1000_2 positive re-framing   0.5589742
    ## 5:    1001_1 withdrawal behavior   0.6706221
    ## 6:    1001_2 positive re-framing   0.5589742

Now let's measure the speeds with `microbenchmark`.

base\_R\_brute\_calculation base\_R\_tabular\_calculation

``` r
tm <- microbenchmark(
  "rquery in memory" = nrow(rquery_local()),
  "rquery from db to memory" = nrow(rquery_database_pull()),
  "rquery database count" = rquery_database_count(),
  "rquery database land" = rquery_database_land(),
  "dplyr in memory" = nrow(dplyr_local()),
  "dplyr tbl in memory" = nrow(dplyr_tbl()),
  "dplyr in memory no grouped filter" = nrow(dplyr_local_no_grouped_filter()),
  "dplyr from memory to db and back" = nrow(dplyr_round_trip()),
  "dplyr from db to memory" = nrow(dplyr_database_pull()),
  "dplyr database count" = dplyr_database_count(),
  "dplyr database land" = dplyr_database_land(),
  "data.table in memory" = nrow(data.table_local()),
  "base R tabular calculation" = nrow(base_R_tabular_calculation()),
  "base R sequential calculation" = nrow(base_R_brute_calculation())
)
saveRDS(tm, "qtimings.RDS")
print(tm)
```

    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##                   rquery in memory  339.2819  346.5192  365.5742  352.0665
    ##           rquery from db to memory  231.5717  241.7728  255.2745  248.3432
    ##              rquery database count  199.4778  203.2662  218.1638  207.3330
    ##               rquery database land  217.7878  222.1298  232.1719  226.3787
    ##                    dplyr in memory 1184.7168 1216.0992 1295.3127 1252.2095
    ##                dplyr tbl in memory 1185.1292 1213.8207 1288.5267 1250.9576
    ##  dplyr in memory no grouped filter  803.8868  828.5966  894.1179  860.5143
    ##   dplyr from memory to db and back  581.7443  595.5464  641.5124  613.4384
    ##            dplyr from db to memory  380.1288  390.5660  421.6581  398.8316
    ##               dplyr database count  366.0760  372.9545  404.8615  381.3254
    ##                dplyr database land  413.1878  419.5564  445.5154  427.9188
    ##               data.table in memory  231.8094  243.6267  272.8844  254.1875
    ##         base R tabular calculation  484.2003  540.7298  574.0857  567.0941
    ##      base R sequential calculation  104.9290  108.4058  116.1558  111.1875
    ##         uq       max neval
    ##   366.8266  656.4589   100
    ##   263.9711  323.2430   100
    ##   215.8895  382.0567   100
    ##   233.4544  341.6554   100
    ##  1316.9411 2266.1614   100
    ##  1316.9795 1680.5972   100
    ##   918.3206 1601.6495   100
    ##   658.8230 1325.0461   100
    ##   418.8582  930.1331   100
    ##   410.8621  869.8656   100
    ##   452.0623  834.5970   100
    ##   288.9526  432.2933   100
    ##   581.4651  885.9790   100
    ##   118.3874  217.7817   100

``` r
autoplot(tm)
```

![](QTiming_files/figure-markdown_github/timings-1.png)

`rquery` appears to be fast. The extra time for "`rquery` local" is because `rquery` doesn't *really* have a local mode, it has to copy the data to the database and back in that case. I currently guess `rquery` and `dplyr` are both picking up parallelism in the database.

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
DBI::dbDisconnect(db)
```
