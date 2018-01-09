QTiming2
================
Win-Vector LLC
1/9/2018

Expanding [expirment](https://github.com/WinVector/rquery/blob/master/extras/QTiming.md) to see if [`dplyr` issue 3294](https://github.com/tidyverse/dplyr/issues/3294) explains all of the difference (it does not). Don't want to replace original run as numbers (of course) jitter.

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

``` r
scale <- 0.237

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
           sum(exp(assessmentTotal * scale)), na.rm = TRUE) %>%
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
           sum(exp(assessmentTotal * scale)), na.rm = TRUE) %>%
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
dplyr_database_land()
```

    ## Warning: Missing values are always removed in SQL.
    ## Use `sum(x, na.rm = TRUE)` to silence this warning

    ## NULL

``` r
head(dplyr_database_pull())
```

    ## Warning: Missing values are always removed in SQL.
    ## Use `sum(x, na.rm = TRUE)` to silence this warning

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

    ## Warning: Missing values are always removed in SQL.
    ## Use `sum(x, na.rm = TRUE)` to silence this warning

    ## # A tibble: 1 x 1
    ##   n              
    ##   <S3: integer64>
    ## 1 20000

``` r
head(dplyr_round_trip())
```

    ## Warning: Missing values are always removed in SQL.
    ## Use `sum(x, na.rm = TRUE)` to silence this warning

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

``` r
tm <- microbenchmark(
  "rquery in memory" = nrow(rquery_local()),
  "rquery from db to memory" = nrow(rquery_database_pull()),
  "rquery database count" = rquery_database_count(),
  "rquery database land" = rquery_database_land(),
  "dplyr in memory" = nrow(dplyr_local()),
  "dplyr in memory no grouped filter" = nrow(dplyr_local_no_grouped_filter()),
  "dplyr tbl in memory" = nrow(dplyr_tbl()),
  "dplyr from memory to db and back" = nrow(dplyr_round_trip()),
  "dplyr from db to memory" = nrow(dplyr_database_pull()),
  "dplyr database count" = dplyr_database_count(),
  "dplyr database land" = dplyr_database_land(),
  "data.table in memory" = nrow(data.table_local())
)
saveRDS(tm, "qtimings2.RDS")
print(tm)
```

    ## Unit: milliseconds
    ##                               expr       min        lq      mean    median
    ##                   rquery in memory  331.7290  338.3388  350.2313  342.2119
    ##           rquery from db to memory  228.3414  234.2707  239.9585  236.7957
    ##              rquery database count  197.6531  201.0083  205.0761  203.4345
    ##               rquery database land  215.8273  219.4785  223.0758  221.1381
    ##                    dplyr in memory 1153.9803 1176.9278 1204.8762 1187.1679
    ##  dplyr in memory no grouped filter  798.1550  809.8372  829.0831  818.4442
    ##                dplyr tbl in memory 1153.8806 1176.8637 1208.2717 1187.3578
    ##   dplyr from memory to db and back  576.4352  584.3709  604.0386  590.3421
    ##            dplyr from db to memory  377.9217  384.4332  399.5301  389.1973
    ##               dplyr database count  363.5714  368.0569  378.2925  372.7274
    ##                dplyr database land  406.1407  414.6825  433.7513  419.7929
    ##               data.table in memory  230.8315  243.0899  261.5366  247.2220
    ##         uq       max neval
    ##   349.0030  433.8113   100
    ##   239.3524  311.4613   100
    ##   205.3718  252.6704   100
    ##   223.7178  294.9885   100
    ##  1225.7919 1306.8742   100
    ##   833.9620  999.1176   100
    ##  1228.3633 1499.5398   100
    ##   607.8350  757.2020   100
    ##   397.0483  605.5059   100
    ##   378.1154  473.2085   100
    ##   431.0584  626.9727   100
    ##   260.4654  387.5718   100

``` r
autoplot(tm)
```

![](QTiming2_files/figure-markdown_github/timings-1.png)

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
