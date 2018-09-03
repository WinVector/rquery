QTiming
================
Win-Vector LLC
9/2/2018

Let's time [`rquery`](https://winvector.github.io/rquery/), [`dplyr`](https://CRAN.R-project.org/package=dplyr), and [`data.table`](https://CRAN.R-project.org/package=data.table) on a non-trivial example.

These timings are on an late 2014 Mac Mini with 8GB of RAM running OSX everything current as of run-date.

First let's load our packages, establish a database connection, and declare an [`rquery` ad hoc execution service](https://winvector.github.io/rquery/articles/AdHocQueries.html) (the "`winvector_temp_db_handle`").

``` r
library("data.table")
library("rquery")
library("rqdatatable")
library("dplyr")
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

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

db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')
# db <- DBI::dbConnect(MonetDBLite::MonetDBLite())
dbopts <- rq_connection_tests(db)
db_hdl <- rquery_db_info(connection = db,
                         is_dbi = TRUE,
                         connection_options = dbopts)
print(db_hdl)
```

    ## [1] "rquery_db_info(PqConnection, is_dbi=TRUE, note=\"\")"

``` r
packageVersion("rquery")
```

    ## [1] '1.0.0'

``` r
packageVersion("dplyr")
```

    ## [1] '0.7.6'

``` r
packageVersion("dbplyr")
```

    ## [1] '1.2.2'

``` r
packageVersion("DBI")
```

    ## [1] '1.0.0'

``` r
packageVersion("data.table")
```

    ## [1] '1.11.4'

``` r
packageVersion("RPostgres")
```

    ## [1] '1.1.1'

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
    ## [1] "5.0"
    ## 
    ## $year
    ## [1] "2018"
    ## 
    ## $month
    ## [1] "04"
    ## 
    ## $day
    ## [1] "23"
    ## 
    ## $`svn rev`
    ## [1] "74626"
    ## 
    ## $language
    ## [1] "R"
    ## 
    ## $version.string
    ## [1] "R version 3.5.0 (2018-04-23)"
    ## 
    ## $nickname
    ## [1] "Joy in Playing"

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
dR <- rquery::rq_copy_to(db, 'dR',
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

    ## Observations: ??
    ## Variables: 3
    ## $ subjectID       <chr> "0_1", "0_1", "0_2", "0_2", "1_1", "1_1", "1_2...
    ## $ surveyCategory  <chr> "withdrawal behavior", "positive re-framing", ...
    ## $ assessmentTotal <dbl> 5, 2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4...

Now we declare our operation pipelines, both on local (in-memory `data.frame`) and remote (already in a database) data.

``` r
scale <- 0.237

# this is a function, 
# so body not evaluated until used
rquery_pipeline <- dR %.>%
  extend_nse(.,
             probability %:=%
               exp(assessmentTotal * scale))  %.>% 
  normalize_cols(.,
                 "probability",
                 partitionby = 'subjectID') %.>%
  pick_top_k(.,
             partitionby = 'subjectID',
             orderby = c('probability', 'surveyCategory'),
             reverse = c('probability')) %.>% 
  rename_columns(., 'diagnosis' %:=% 'surveyCategory') %.>%
  select_columns(., c('subjectID', 
                      'diagnosis', 
                      'probability')) %.>%
  orderby(., cols = 'subjectID')

rqdatatable <- function() {
  dLocal %.>% rquery_pipeline
}

rquery_database_roundtrip <- function() {
  dRT <- rquery::rq_copy_to(db, 'dR',
                          dLocal,
                          temporary = TRUE, 
                          overwrite = TRUE)
  rquery::execute(db_hdl, rquery_pipeline)
}


rquery_database_pull <- function() {
  rquery::execute(db_hdl, rquery_pipeline)
}

rquery_database_land <- function() {
  tabName <- "rquery_tmpx"
  rquery::materialize(db_hdl, rquery_pipeline, table_name = tabName,
                      overwrite = TRUE, temporary = TRUE)
  NULL
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
                         overwrite = TRUE,
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

.datatable.aware <- TRUE

# improved code from:
# http://www.win-vector.com/blog/2018/01/base-r-can-be-fast/#comment-66746
data.table_local <- function() {
  dDT <- data.table::data.table(dLocal)
  dDT <- dDT[,list(diagnosis = surveyCategory,
                   probability = exp (assessmentTotal * scale ) /
                     sum ( exp ( assessmentTotal * scale ) ))
             ,subjectID ]
  setorder(dDT, subjectID, probability, -diagnosis)
  dDT <- dDT[,.SD[.N],subjectID]
  setorder(dDT, subjectID)
}
```

Let's inspect the functions.

``` r
head(rqdatatable())
```

    ##    subjectID           diagnosis probability
    ## 1:       0_1 withdrawal behavior   0.6706221
    ## 2:       0_2 positive re-framing   0.5589742
    ## 3:    1000_1 withdrawal behavior   0.6706221
    ## 4:    1000_2 positive re-framing   0.5589742
    ## 5:    1001_1 withdrawal behavior   0.6706221
    ## 6:    1001_2 positive re-framing   0.5589742

``` r
head(rquery_database_roundtrip())
```

    ##   subjectID           diagnosis probability
    ## 1       0_1 withdrawal behavior   0.6706221
    ## 2       0_2 positive re-framing   0.5589742
    ## 3    1000_1 withdrawal behavior   0.6706221
    ## 4    1000_2 positive re-framing   0.5589742
    ## 5    1001_1 withdrawal behavior   0.6706221
    ## 6    1001_2 positive re-framing   0.5589742

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
    ## 5    1001_1 withdrawal behavior   0.6706221
    ## 6    1001_2 positive re-framing   0.5589742

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
    ## 5 1001_1    withdrawal behavior       0.671
    ## 6 1001_2    positive re-framing       0.559

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
    ## 5 1001_1    withdrawal behavior       0.671
    ## 6 1001_2    positive re-framing       0.559

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

``` r
tm <- microbenchmark(
  "rqdatatable" = nrow(rqdatatable()),
  "rquery database roundtrip" = nrow(rquery_database_roundtrip()),
  "rquery from db to memory" = nrow(rquery_database_pull()),
  "rquery database land" = rquery_database_land(),
  "dplyr in memory" = nrow(dplyr_local()),
  "dplyr tbl in memory" = nrow(dplyr_tbl()),
  "dplyr in memory no grouped filter" = nrow(dplyr_local_no_grouped_filter()),
  "dplyr from memory to db and back" = nrow(dplyr_round_trip()),
  "dplyr from db to memory" = nrow(dplyr_database_pull()),
  "dplyr database land" = dplyr_database_land(),
  "data.table in memory" = nrow(data.table_local())
)
saveRDS(tm, "qtimings.RDS")
print(tm)
```

    ## Unit: milliseconds
    ##                               expr        min         lq       mean
    ##                        rqdatatable   70.77334   73.20129   79.14639
    ##          rquery database roundtrip  736.52685  830.08294  848.06077
    ##           rquery from db to memory  642.01160  728.18040  737.62455
    ##               rquery database land  649.08938  742.14278  754.49151
    ##                    dplyr in memory 1129.96133 1171.07291 1201.49031
    ##                dplyr tbl in memory 1126.14372 1175.52373 1213.71515
    ##  dplyr in memory no grouped filter  783.20897  803.76274  835.98151
    ##   dplyr from memory to db and back 1372.55581 1533.31120 1548.73857
    ##            dplyr from db to memory  938.74446 1059.54369 1073.19727
    ##                dplyr database land  971.26286 1101.80752 1116.63241
    ##               data.table in memory   70.47491   76.65464   85.45156
    ##      median         uq       max neval     cld
    ##    77.21041   80.03941  185.1339   100 a      
    ##   844.27562  868.63970  967.3601   100   c    
    ##   738.37703  749.93326  813.9729   100  b     
    ##   751.47924  768.10678  836.0420   100  b     
    ##  1189.31987 1221.10367 1465.4911   100      f 
    ##  1203.47292 1245.88814 1360.0681   100      f 
    ##   820.88013  863.67755 1017.0145   100   c    
    ##  1546.99201 1565.88040 1649.0785   100       g
    ##  1071.93852 1091.92242 1230.6090   100    d   
    ##  1118.96104 1136.74233 1227.2759   100     e  
    ##    82.18142   89.18355  128.5811   100 a

``` r
autoplot(tm)
```

    ## Coordinate system already present. Adding new coordinate system, which will replace the existing one.

![](QTiming_files/figure-markdown_github/timings-1.png)

`rquery` appears to be fast. The extra time for "`rquery` local" is because `rquery` doesn't *really* have a local mode, it has to copy the data to the database and back in that case. I currently guess `rquery` and `dplyr` are both picking up parallelism in the database.

``` r
sessionInfo()
```

    ## R version 3.5.0 (2018-04-23)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.6
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] bindrcpp_0.2.2       ggplot2_3.0.0        microbenchmark_1.4-4
    ## [4] dplyr_0.7.6          rqdatatable_1.0.0    rquery_1.0.0        
    ## [7] data.table_1.11.4   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] zoo_1.8-3        tidyselect_0.2.4 purrr_0.2.5      splines_3.5.0   
    ##  [5] lattice_0.20-35  colorspace_1.3-2 htmltools_0.3.6  yaml_2.2.0      
    ##  [9] utf8_1.1.4       blob_1.1.1       survival_2.42-6  rlang_0.2.2.9000
    ## [13] pillar_1.3.0     glue_1.3.0       withr_2.1.2      DBI_1.0.0       
    ## [17] bit64_0.9-7      dbplyr_1.2.2     multcomp_1.4-8   bindr_0.1.1     
    ## [21] plyr_1.8.4       stringr_1.3.1    munsell_0.5.0    gtable_0.2.0    
    ## [25] mvtnorm_1.0-8    codetools_0.2-15 evaluate_0.11    knitr_1.20      
    ## [29] parallel_3.5.0   fansi_0.3.0      TH.data_1.0-9    Rcpp_0.12.18    
    ## [33] scales_1.0.0     backports_1.1.2  cdata_1.0.0      bit_1.1-14      
    ## [37] hms_0.4.2        digest_0.6.16    stringi_1.2.4    grid_3.5.0      
    ## [41] rprojroot_1.3-2  cli_1.0.0        tools_3.5.0      sandwich_2.5-0  
    ## [45] magrittr_1.5     lazyeval_0.2.1   tibble_1.4.2     crayon_1.3.4    
    ## [49] wrapr_1.6.1      pkgconfig_2.0.2  MASS_7.3-50      Matrix_1.2-14   
    ## [53] assertthat_0.2.0 rmarkdown_1.10   RPostgres_1.1.1  R6_2.2.2        
    ## [57] compiler_3.5.0

``` r
DBI::dbDisconnect(db_hdl$connection)
```
