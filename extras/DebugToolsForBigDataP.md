Debug Tools for Big Data (PostgreSQL)
================
2018-05-11

<!-- file.md is generated from file.Rmd. Please edit that file -->
``` r
base::date()
```

    ## [1] "Fri May 11 22:52:00 2018"

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
library("rquery")
```

    ## Loading required package: wrapr

``` r
my_db <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                        host = 'localhost',
                        port = 5432,
                        user = 'johnmount',
                        password = '')

# configure rquery options
dbopts <- dbi_connection_tests(my_db)
print(dbopts)
```

    ## $rquery.PostgreSQLConnection.use_DBI_dbListFields
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbRemoveTable
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbExecute
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.create_temporary
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.control_temporary
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.control_rownames
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbExistsTable
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.check_logical_column_types
    ## [1] FALSE

``` r
options(dbopts)

base::date()
```

    ## [1] "Fri May 11 22:52:01 2018"

``` r
base::date()
```

    ## [1] "Fri May 11 22:52:01 2018"

``` r
# build up example data
nSubj <- 1000000
nIrrelCol <- 200

d_local <- data.frame(subjectID = sort(rep(seq_len(nSubj),2)),
                 surveyCategory = c(
                   'withdrawal behavior',
                   'positive re-framing'),
                 stringsAsFactors = FALSE)
d_local$assessmentTotal <- sample.int(10, nrow(d_local), replace = TRUE)
d_small <- rquery::dbi_copy_to(my_db, 'd_small',
                 d_local,
                 overwrite = TRUE, 
                 temporary = TRUE)
rm(list = "d_local")
# cdata::qlook(my_db, d_small$table_name)

base::date()
```

    ## [1] "Fri May 11 22:52:04 2018"

``` r
base::date()
```

    ## [1] "Fri May 11 22:52:04 2018"

``` r
# add in irrelevant columns
# simulates performing a calculation against a larger data mart
assignments <- 
  vapply(seq_len(nIrrelCol), 
         function(i) {
           paste("irrelevantCol", sprintf("%07g", i), sep = "_")
         }, character(1)) := rep("1.5", nIrrelCol)
d_large <- d_small %.>%
  extend_se(., assignments) %.>%
  materialize(my_db, ., 
              overwrite = TRUE,
              temporary = TRUE)
rm(list = "d_small")
# cdata::qlook(my_db, d_large$table_name)

# build dplyr reference
d_large_tbl <- tbl(my_db, d_large$table_name)

# rquery view of table
rquery::dbi_nrow(my_db, d_large$table_name)
```

    ## [1] 2e+06

``` r
length(column_names(d_large))
```

    ## [1] 203

``` r
base::date()
```

    ## [1] "Fri May 11 22:52:54 2018"

Define and demonstrate pipelines:

``` r
base::date()
```

    ## [1] "Fri May 11 22:52:54 2018"

``` r
scale <- 0.237

rquery_pipeline <- d_large %.>%
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale))  %.>% 
  normalize_cols(.,
                 "probability",
                 partitionby = 'subjectID') %.>%
  pick_top_k(.,
             partitionby = 'subjectID',
             rev_orderby = c('probability', 'surveyCategory')) %.>%
  rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
  select_columns(., qc(subjectID, diagnosis, probability)) %.>%
  orderby(., 'subjectID') 

# special debug-mode limits all sources to 1 row.
# not correct for windowed calculations or joins- 
# but lets us at least see something execute quickly.
system.time(nrow(as.data.frame(execute(my_db, rquery_pipeline, source_limit = 1L))))
```

    ##    user  system elapsed 
    ##   0.027   0.000   0.030

``` r
# full run
system.time(nrow(as.data.frame(execute(my_db, rquery_pipeline))))
```

    ##    user  system elapsed 
    ##   0.862   0.171  25.986

``` r
base::date()
```

    ## [1] "Fri May 11 22:53:20 2018"

``` r
base::date()
```

    ## [1] "Fri May 11 22:53:20 2018"

``` r
scale <- 0.237

dplyr_pipeline <- d_large_tbl %>%
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


# full run
system.time(nrow(as.data.frame(dplyr_pipeline)))
```

    ##    user  system elapsed 
    ##   0.959   0.183  36.149

``` r
base::date()
```

    ## [1] "Fri May 11 22:53:56 2018"

Now, let's show how/where erroneous pipelines are debugged in each system.

In `rquery` many user errors are caught during pipeline construction, independent of database.

``` r
base::date()
```

    ## [1] "Fri May 11 22:53:56 2018"

``` r
# rquery catches the error during pipeline definition,
# prior to sending it to the database or Spark data system.
rquery_pipeline_late_error <- d_large %.>%
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale))  %.>% 
  normalize_cols(.,
                 "probability",
                 partitionby = 'subjectID') %.>%
  pick_top_k(.,
             partitionby = 'subjectID',
             rev_orderby = c('probability', 'surveyCategory')) %.>%
  rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
  select_columns(., qc(subjectID, diagnosis, probability)) %.>%
  orderby(., 'ZubjectIDZZZ') # <- error non-existent column
```

    ## Error in check_have_cols(have, unique(c(cols, rev_cols)), "rquery::orderby"): rquery::orderby unknown columns ZubjectIDZZZ

``` r
base::date()
```

    ## [1] "Fri May 11 22:53:56 2018"

With `dplyr` user errors are mostly caught when the command is analyzed on the remote data system.

``` r
base::date()
```

    ## [1] "Fri May 11 22:53:56 2018"

``` r
# dplyr accepts an incorrect pipeline
dplyr_pipeline_late_error <- d_large_tbl %>%
  group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
  arrange(probability, surveyCategory) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(ZubjectIDZZZ)  # <- error non-existent column

# dplyr will generate (incorrect) SQL from the incorrect pipeline
cat(dbplyr::remote_query(dplyr_pipeline_late_error))
```

    ## SELECT "subjectID", "diagnosis", "probability"
    ## FROM (SELECT "subjectID", "surveyCategory" AS "diagnosis", "assessmentTotal", "irrelevantCol_0000001", "irrelevantCol_0000002", "irrelevantCol_0000003", "irrelevantCol_0000004", "irrelevantCol_0000005", "irrelevantCol_0000006", "irrelevantCol_0000007", "irrelevantCol_0000008", "irrelevantCol_0000009", "irrelevantCol_0000010", "irrelevantCol_0000011", "irrelevantCol_0000012", "irrelevantCol_0000013", "irrelevantCol_0000014", "irrelevantCol_0000015", "irrelevantCol_0000016", "irrelevantCol_0000017", "irrelevantCol_0000018", "irrelevantCol_0000019", "irrelevantCol_0000020", "irrelevantCol_0000021", "irrelevantCol_0000022", "irrelevantCol_0000023", "irrelevantCol_0000024", "irrelevantCol_0000025", "irrelevantCol_0000026", "irrelevantCol_0000027", "irrelevantCol_0000028", "irrelevantCol_0000029", "irrelevantCol_0000030", "irrelevantCol_0000031", "irrelevantCol_0000032", "irrelevantCol_0000033", "irrelevantCol_0000034", "irrelevantCol_0000035", "irrelevantCol_0000036", "irrelevantCol_0000037", "irrelevantCol_0000038", "irrelevantCol_0000039", "irrelevantCol_0000040", "irrelevantCol_0000041", "irrelevantCol_0000042", "irrelevantCol_0000043", "irrelevantCol_0000044", "irrelevantCol_0000045", "irrelevantCol_0000046", "irrelevantCol_0000047", "irrelevantCol_0000048", "irrelevantCol_0000049", "irrelevantCol_0000050", "irrelevantCol_0000051", "irrelevantCol_0000052", "irrelevantCol_0000053", "irrelevantCol_0000054", "irrelevantCol_0000055", "irrelevantCol_0000056", "irrelevantCol_0000057", "irrelevantCol_0000058", "irrelevantCol_0000059", "irrelevantCol_0000060", "irrelevantCol_0000061", "irrelevantCol_0000062", "irrelevantCol_0000063", "irrelevantCol_0000064", "irrelevantCol_0000065", "irrelevantCol_0000066", "irrelevantCol_0000067", "irrelevantCol_0000068", "irrelevantCol_0000069", "irrelevantCol_0000070", "irrelevantCol_0000071", "irrelevantCol_0000072", "irrelevantCol_0000073", "irrelevantCol_0000074", "irrelevantCol_0000075", "irrelevantCol_0000076", "irrelevantCol_0000077", "irrelevantCol_0000078", "irrelevantCol_0000079", "irrelevantCol_0000080", "irrelevantCol_0000081", "irrelevantCol_0000082", "irrelevantCol_0000083", "irrelevantCol_0000084", "irrelevantCol_0000085", "irrelevantCol_0000086", "irrelevantCol_0000087", "irrelevantCol_0000088", "irrelevantCol_0000089", "irrelevantCol_0000090", "irrelevantCol_0000091", "irrelevantCol_0000092", "irrelevantCol_0000093", "irrelevantCol_0000094", "irrelevantCol_0000095", "irrelevantCol_0000096", "irrelevantCol_0000097", "irrelevantCol_0000098", "irrelevantCol_0000099", "irrelevantCol_0000100", "irrelevantCol_0000101", "irrelevantCol_0000102", "irrelevantCol_0000103", "irrelevantCol_0000104", "irrelevantCol_0000105", "irrelevantCol_0000106", "irrelevantCol_0000107", "irrelevantCol_0000108", "irrelevantCol_0000109", "irrelevantCol_0000110", "irrelevantCol_0000111", "irrelevantCol_0000112", "irrelevantCol_0000113", "irrelevantCol_0000114", "irrelevantCol_0000115", "irrelevantCol_0000116", "irrelevantCol_0000117", "irrelevantCol_0000118", "irrelevantCol_0000119", "irrelevantCol_0000120", "irrelevantCol_0000121", "irrelevantCol_0000122", "irrelevantCol_0000123", "irrelevantCol_0000124", "irrelevantCol_0000125", "irrelevantCol_0000126", "irrelevantCol_0000127", "irrelevantCol_0000128", "irrelevantCol_0000129", "irrelevantCol_0000130", "irrelevantCol_0000131", "irrelevantCol_0000132", "irrelevantCol_0000133", "irrelevantCol_0000134", "irrelevantCol_0000135", "irrelevantCol_0000136", "irrelevantCol_0000137", "irrelevantCol_0000138", "irrelevantCol_0000139", "irrelevantCol_0000140", "irrelevantCol_0000141", "irrelevantCol_0000142", "irrelevantCol_0000143", "irrelevantCol_0000144", "irrelevantCol_0000145", "irrelevantCol_0000146", "irrelevantCol_0000147", "irrelevantCol_0000148", "irrelevantCol_0000149", "irrelevantCol_0000150", "irrelevantCol_0000151", "irrelevantCol_0000152", "irrelevantCol_0000153", "irrelevantCol_0000154", "irrelevantCol_0000155", "irrelevantCol_0000156", "irrelevantCol_0000157", "irrelevantCol_0000158", "irrelevantCol_0000159", "irrelevantCol_0000160", "irrelevantCol_0000161", "irrelevantCol_0000162", "irrelevantCol_0000163", "irrelevantCol_0000164", "irrelevantCol_0000165", "irrelevantCol_0000166", "irrelevantCol_0000167", "irrelevantCol_0000168", "irrelevantCol_0000169", "irrelevantCol_0000170", "irrelevantCol_0000171", "irrelevantCol_0000172", "irrelevantCol_0000173", "irrelevantCol_0000174", "irrelevantCol_0000175", "irrelevantCol_0000176", "irrelevantCol_0000177", "irrelevantCol_0000178", "irrelevantCol_0000179", "irrelevantCol_0000180", "irrelevantCol_0000181", "irrelevantCol_0000182", "irrelevantCol_0000183", "irrelevantCol_0000184", "irrelevantCol_0000185", "irrelevantCol_0000186", "irrelevantCol_0000187", "irrelevantCol_0000188", "irrelevantCol_0000189", "irrelevantCol_0000190", "irrelevantCol_0000191", "irrelevantCol_0000192", "irrelevantCol_0000193", "irrelevantCol_0000194", "irrelevantCol_0000195", "irrelevantCol_0000196", "irrelevantCol_0000197", "irrelevantCol_0000198", "irrelevantCol_0000199", "irrelevantCol_0000200", "probability"
    ## FROM (SELECT "subjectID", "surveyCategory", "assessmentTotal", "irrelevantCol_0000001", "irrelevantCol_0000002", "irrelevantCol_0000003", "irrelevantCol_0000004", "irrelevantCol_0000005", "irrelevantCol_0000006", "irrelevantCol_0000007", "irrelevantCol_0000008", "irrelevantCol_0000009", "irrelevantCol_0000010", "irrelevantCol_0000011", "irrelevantCol_0000012", "irrelevantCol_0000013", "irrelevantCol_0000014", "irrelevantCol_0000015", "irrelevantCol_0000016", "irrelevantCol_0000017", "irrelevantCol_0000018", "irrelevantCol_0000019", "irrelevantCol_0000020", "irrelevantCol_0000021", "irrelevantCol_0000022", "irrelevantCol_0000023", "irrelevantCol_0000024", "irrelevantCol_0000025", "irrelevantCol_0000026", "irrelevantCol_0000027", "irrelevantCol_0000028", "irrelevantCol_0000029", "irrelevantCol_0000030", "irrelevantCol_0000031", "irrelevantCol_0000032", "irrelevantCol_0000033", "irrelevantCol_0000034", "irrelevantCol_0000035", "irrelevantCol_0000036", "irrelevantCol_0000037", "irrelevantCol_0000038", "irrelevantCol_0000039", "irrelevantCol_0000040", "irrelevantCol_0000041", "irrelevantCol_0000042", "irrelevantCol_0000043", "irrelevantCol_0000044", "irrelevantCol_0000045", "irrelevantCol_0000046", "irrelevantCol_0000047", "irrelevantCol_0000048", "irrelevantCol_0000049", "irrelevantCol_0000050", "irrelevantCol_0000051", "irrelevantCol_0000052", "irrelevantCol_0000053", "irrelevantCol_0000054", "irrelevantCol_0000055", "irrelevantCol_0000056", "irrelevantCol_0000057", "irrelevantCol_0000058", "irrelevantCol_0000059", "irrelevantCol_0000060", "irrelevantCol_0000061", "irrelevantCol_0000062", "irrelevantCol_0000063", "irrelevantCol_0000064", "irrelevantCol_0000065", "irrelevantCol_0000066", "irrelevantCol_0000067", "irrelevantCol_0000068", "irrelevantCol_0000069", "irrelevantCol_0000070", "irrelevantCol_0000071", "irrelevantCol_0000072", "irrelevantCol_0000073", "irrelevantCol_0000074", "irrelevantCol_0000075", "irrelevantCol_0000076", "irrelevantCol_0000077", "irrelevantCol_0000078", "irrelevantCol_0000079", "irrelevantCol_0000080", "irrelevantCol_0000081", "irrelevantCol_0000082", "irrelevantCol_0000083", "irrelevantCol_0000084", "irrelevantCol_0000085", "irrelevantCol_0000086", "irrelevantCol_0000087", "irrelevantCol_0000088", "irrelevantCol_0000089", "irrelevantCol_0000090", "irrelevantCol_0000091", "irrelevantCol_0000092", "irrelevantCol_0000093", "irrelevantCol_0000094", "irrelevantCol_0000095", "irrelevantCol_0000096", "irrelevantCol_0000097", "irrelevantCol_0000098", "irrelevantCol_0000099", "irrelevantCol_0000100", "irrelevantCol_0000101", "irrelevantCol_0000102", "irrelevantCol_0000103", "irrelevantCol_0000104", "irrelevantCol_0000105", "irrelevantCol_0000106", "irrelevantCol_0000107", "irrelevantCol_0000108", "irrelevantCol_0000109", "irrelevantCol_0000110", "irrelevantCol_0000111", "irrelevantCol_0000112", "irrelevantCol_0000113", "irrelevantCol_0000114", "irrelevantCol_0000115", "irrelevantCol_0000116", "irrelevantCol_0000117", "irrelevantCol_0000118", "irrelevantCol_0000119", "irrelevantCol_0000120", "irrelevantCol_0000121", "irrelevantCol_0000122", "irrelevantCol_0000123", "irrelevantCol_0000124", "irrelevantCol_0000125", "irrelevantCol_0000126", "irrelevantCol_0000127", "irrelevantCol_0000128", "irrelevantCol_0000129", "irrelevantCol_0000130", "irrelevantCol_0000131", "irrelevantCol_0000132", "irrelevantCol_0000133", "irrelevantCol_0000134", "irrelevantCol_0000135", "irrelevantCol_0000136", "irrelevantCol_0000137", "irrelevantCol_0000138", "irrelevantCol_0000139", "irrelevantCol_0000140", "irrelevantCol_0000141", "irrelevantCol_0000142", "irrelevantCol_0000143", "irrelevantCol_0000144", "irrelevantCol_0000145", "irrelevantCol_0000146", "irrelevantCol_0000147", "irrelevantCol_0000148", "irrelevantCol_0000149", "irrelevantCol_0000150", "irrelevantCol_0000151", "irrelevantCol_0000152", "irrelevantCol_0000153", "irrelevantCol_0000154", "irrelevantCol_0000155", "irrelevantCol_0000156", "irrelevantCol_0000157", "irrelevantCol_0000158", "irrelevantCol_0000159", "irrelevantCol_0000160", "irrelevantCol_0000161", "irrelevantCol_0000162", "irrelevantCol_0000163", "irrelevantCol_0000164", "irrelevantCol_0000165", "irrelevantCol_0000166", "irrelevantCol_0000167", "irrelevantCol_0000168", "irrelevantCol_0000169", "irrelevantCol_0000170", "irrelevantCol_0000171", "irrelevantCol_0000172", "irrelevantCol_0000173", "irrelevantCol_0000174", "irrelevantCol_0000175", "irrelevantCol_0000176", "irrelevantCol_0000177", "irrelevantCol_0000178", "irrelevantCol_0000179", "irrelevantCol_0000180", "irrelevantCol_0000181", "irrelevantCol_0000182", "irrelevantCol_0000183", "irrelevantCol_0000184", "irrelevantCol_0000185", "irrelevantCol_0000186", "irrelevantCol_0000187", "irrelevantCol_0000188", "irrelevantCol_0000189", "irrelevantCol_0000190", "irrelevantCol_0000191", "irrelevantCol_0000192", "irrelevantCol_0000193", "irrelevantCol_0000194", "irrelevantCol_0000195", "irrelevantCol_0000196", "irrelevantCol_0000197", "irrelevantCol_0000198", "irrelevantCol_0000199", "irrelevantCol_0000200", "probability"
    ## FROM (SELECT "subjectID", "surveyCategory", "assessmentTotal", "irrelevantCol_0000001", "irrelevantCol_0000002", "irrelevantCol_0000003", "irrelevantCol_0000004", "irrelevantCol_0000005", "irrelevantCol_0000006", "irrelevantCol_0000007", "irrelevantCol_0000008", "irrelevantCol_0000009", "irrelevantCol_0000010", "irrelevantCol_0000011", "irrelevantCol_0000012", "irrelevantCol_0000013", "irrelevantCol_0000014", "irrelevantCol_0000015", "irrelevantCol_0000016", "irrelevantCol_0000017", "irrelevantCol_0000018", "irrelevantCol_0000019", "irrelevantCol_0000020", "irrelevantCol_0000021", "irrelevantCol_0000022", "irrelevantCol_0000023", "irrelevantCol_0000024", "irrelevantCol_0000025", "irrelevantCol_0000026", "irrelevantCol_0000027", "irrelevantCol_0000028", "irrelevantCol_0000029", "irrelevantCol_0000030", "irrelevantCol_0000031", "irrelevantCol_0000032", "irrelevantCol_0000033", "irrelevantCol_0000034", "irrelevantCol_0000035", "irrelevantCol_0000036", "irrelevantCol_0000037", "irrelevantCol_0000038", "irrelevantCol_0000039", "irrelevantCol_0000040", "irrelevantCol_0000041", "irrelevantCol_0000042", "irrelevantCol_0000043", "irrelevantCol_0000044", "irrelevantCol_0000045", "irrelevantCol_0000046", "irrelevantCol_0000047", "irrelevantCol_0000048", "irrelevantCol_0000049", "irrelevantCol_0000050", "irrelevantCol_0000051", "irrelevantCol_0000052", "irrelevantCol_0000053", "irrelevantCol_0000054", "irrelevantCol_0000055", "irrelevantCol_0000056", "irrelevantCol_0000057", "irrelevantCol_0000058", "irrelevantCol_0000059", "irrelevantCol_0000060", "irrelevantCol_0000061", "irrelevantCol_0000062", "irrelevantCol_0000063", "irrelevantCol_0000064", "irrelevantCol_0000065", "irrelevantCol_0000066", "irrelevantCol_0000067", "irrelevantCol_0000068", "irrelevantCol_0000069", "irrelevantCol_0000070", "irrelevantCol_0000071", "irrelevantCol_0000072", "irrelevantCol_0000073", "irrelevantCol_0000074", "irrelevantCol_0000075", "irrelevantCol_0000076", "irrelevantCol_0000077", "irrelevantCol_0000078", "irrelevantCol_0000079", "irrelevantCol_0000080", "irrelevantCol_0000081", "irrelevantCol_0000082", "irrelevantCol_0000083", "irrelevantCol_0000084", "irrelevantCol_0000085", "irrelevantCol_0000086", "irrelevantCol_0000087", "irrelevantCol_0000088", "irrelevantCol_0000089", "irrelevantCol_0000090", "irrelevantCol_0000091", "irrelevantCol_0000092", "irrelevantCol_0000093", "irrelevantCol_0000094", "irrelevantCol_0000095", "irrelevantCol_0000096", "irrelevantCol_0000097", "irrelevantCol_0000098", "irrelevantCol_0000099", "irrelevantCol_0000100", "irrelevantCol_0000101", "irrelevantCol_0000102", "irrelevantCol_0000103", "irrelevantCol_0000104", "irrelevantCol_0000105", "irrelevantCol_0000106", "irrelevantCol_0000107", "irrelevantCol_0000108", "irrelevantCol_0000109", "irrelevantCol_0000110", "irrelevantCol_0000111", "irrelevantCol_0000112", "irrelevantCol_0000113", "irrelevantCol_0000114", "irrelevantCol_0000115", "irrelevantCol_0000116", "irrelevantCol_0000117", "irrelevantCol_0000118", "irrelevantCol_0000119", "irrelevantCol_0000120", "irrelevantCol_0000121", "irrelevantCol_0000122", "irrelevantCol_0000123", "irrelevantCol_0000124", "irrelevantCol_0000125", "irrelevantCol_0000126", "irrelevantCol_0000127", "irrelevantCol_0000128", "irrelevantCol_0000129", "irrelevantCol_0000130", "irrelevantCol_0000131", "irrelevantCol_0000132", "irrelevantCol_0000133", "irrelevantCol_0000134", "irrelevantCol_0000135", "irrelevantCol_0000136", "irrelevantCol_0000137", "irrelevantCol_0000138", "irrelevantCol_0000139", "irrelevantCol_0000140", "irrelevantCol_0000141", "irrelevantCol_0000142", "irrelevantCol_0000143", "irrelevantCol_0000144", "irrelevantCol_0000145", "irrelevantCol_0000146", "irrelevantCol_0000147", "irrelevantCol_0000148", "irrelevantCol_0000149", "irrelevantCol_0000150", "irrelevantCol_0000151", "irrelevantCol_0000152", "irrelevantCol_0000153", "irrelevantCol_0000154", "irrelevantCol_0000155", "irrelevantCol_0000156", "irrelevantCol_0000157", "irrelevantCol_0000158", "irrelevantCol_0000159", "irrelevantCol_0000160", "irrelevantCol_0000161", "irrelevantCol_0000162", "irrelevantCol_0000163", "irrelevantCol_0000164", "irrelevantCol_0000165", "irrelevantCol_0000166", "irrelevantCol_0000167", "irrelevantCol_0000168", "irrelevantCol_0000169", "irrelevantCol_0000170", "irrelevantCol_0000171", "irrelevantCol_0000172", "irrelevantCol_0000173", "irrelevantCol_0000174", "irrelevantCol_0000175", "irrelevantCol_0000176", "irrelevantCol_0000177", "irrelevantCol_0000178", "irrelevantCol_0000179", "irrelevantCol_0000180", "irrelevantCol_0000181", "irrelevantCol_0000182", "irrelevantCol_0000183", "irrelevantCol_0000184", "irrelevantCol_0000185", "irrelevantCol_0000186", "irrelevantCol_0000187", "irrelevantCol_0000188", "irrelevantCol_0000189", "irrelevantCol_0000190", "irrelevantCol_0000191", "irrelevantCol_0000192", "irrelevantCol_0000193", "irrelevantCol_0000194", "irrelevantCol_0000195", "irrelevantCol_0000196", "irrelevantCol_0000197", "irrelevantCol_0000198", "irrelevantCol_0000199", "irrelevantCol_0000200", "probability", row_number() OVER (PARTITION BY "subjectID" ORDER BY "probability", "surveyCategory") AS "zzz4", COUNT(*) OVER (PARTITION BY "subjectID") AS "zzz5"
    ## FROM (SELECT *
    ## FROM (SELECT "subjectID", "surveyCategory", "assessmentTotal", "irrelevantCol_0000001", "irrelevantCol_0000002", "irrelevantCol_0000003", "irrelevantCol_0000004", "irrelevantCol_0000005", "irrelevantCol_0000006", "irrelevantCol_0000007", "irrelevantCol_0000008", "irrelevantCol_0000009", "irrelevantCol_0000010", "irrelevantCol_0000011", "irrelevantCol_0000012", "irrelevantCol_0000013", "irrelevantCol_0000014", "irrelevantCol_0000015", "irrelevantCol_0000016", "irrelevantCol_0000017", "irrelevantCol_0000018", "irrelevantCol_0000019", "irrelevantCol_0000020", "irrelevantCol_0000021", "irrelevantCol_0000022", "irrelevantCol_0000023", "irrelevantCol_0000024", "irrelevantCol_0000025", "irrelevantCol_0000026", "irrelevantCol_0000027", "irrelevantCol_0000028", "irrelevantCol_0000029", "irrelevantCol_0000030", "irrelevantCol_0000031", "irrelevantCol_0000032", "irrelevantCol_0000033", "irrelevantCol_0000034", "irrelevantCol_0000035", "irrelevantCol_0000036", "irrelevantCol_0000037", "irrelevantCol_0000038", "irrelevantCol_0000039", "irrelevantCol_0000040", "irrelevantCol_0000041", "irrelevantCol_0000042", "irrelevantCol_0000043", "irrelevantCol_0000044", "irrelevantCol_0000045", "irrelevantCol_0000046", "irrelevantCol_0000047", "irrelevantCol_0000048", "irrelevantCol_0000049", "irrelevantCol_0000050", "irrelevantCol_0000051", "irrelevantCol_0000052", "irrelevantCol_0000053", "irrelevantCol_0000054", "irrelevantCol_0000055", "irrelevantCol_0000056", "irrelevantCol_0000057", "irrelevantCol_0000058", "irrelevantCol_0000059", "irrelevantCol_0000060", "irrelevantCol_0000061", "irrelevantCol_0000062", "irrelevantCol_0000063", "irrelevantCol_0000064", "irrelevantCol_0000065", "irrelevantCol_0000066", "irrelevantCol_0000067", "irrelevantCol_0000068", "irrelevantCol_0000069", "irrelevantCol_0000070", "irrelevantCol_0000071", "irrelevantCol_0000072", "irrelevantCol_0000073", "irrelevantCol_0000074", "irrelevantCol_0000075", "irrelevantCol_0000076", "irrelevantCol_0000077", "irrelevantCol_0000078", "irrelevantCol_0000079", "irrelevantCol_0000080", "irrelevantCol_0000081", "irrelevantCol_0000082", "irrelevantCol_0000083", "irrelevantCol_0000084", "irrelevantCol_0000085", "irrelevantCol_0000086", "irrelevantCol_0000087", "irrelevantCol_0000088", "irrelevantCol_0000089", "irrelevantCol_0000090", "irrelevantCol_0000091", "irrelevantCol_0000092", "irrelevantCol_0000093", "irrelevantCol_0000094", "irrelevantCol_0000095", "irrelevantCol_0000096", "irrelevantCol_0000097", "irrelevantCol_0000098", "irrelevantCol_0000099", "irrelevantCol_0000100", "irrelevantCol_0000101", "irrelevantCol_0000102", "irrelevantCol_0000103", "irrelevantCol_0000104", "irrelevantCol_0000105", "irrelevantCol_0000106", "irrelevantCol_0000107", "irrelevantCol_0000108", "irrelevantCol_0000109", "irrelevantCol_0000110", "irrelevantCol_0000111", "irrelevantCol_0000112", "irrelevantCol_0000113", "irrelevantCol_0000114", "irrelevantCol_0000115", "irrelevantCol_0000116", "irrelevantCol_0000117", "irrelevantCol_0000118", "irrelevantCol_0000119", "irrelevantCol_0000120", "irrelevantCol_0000121", "irrelevantCol_0000122", "irrelevantCol_0000123", "irrelevantCol_0000124", "irrelevantCol_0000125", "irrelevantCol_0000126", "irrelevantCol_0000127", "irrelevantCol_0000128", "irrelevantCol_0000129", "irrelevantCol_0000130", "irrelevantCol_0000131", "irrelevantCol_0000132", "irrelevantCol_0000133", "irrelevantCol_0000134", "irrelevantCol_0000135", "irrelevantCol_0000136", "irrelevantCol_0000137", "irrelevantCol_0000138", "irrelevantCol_0000139", "irrelevantCol_0000140", "irrelevantCol_0000141", "irrelevantCol_0000142", "irrelevantCol_0000143", "irrelevantCol_0000144", "irrelevantCol_0000145", "irrelevantCol_0000146", "irrelevantCol_0000147", "irrelevantCol_0000148", "irrelevantCol_0000149", "irrelevantCol_0000150", "irrelevantCol_0000151", "irrelevantCol_0000152", "irrelevantCol_0000153", "irrelevantCol_0000154", "irrelevantCol_0000155", "irrelevantCol_0000156", "irrelevantCol_0000157", "irrelevantCol_0000158", "irrelevantCol_0000159", "irrelevantCol_0000160", "irrelevantCol_0000161", "irrelevantCol_0000162", "irrelevantCol_0000163", "irrelevantCol_0000164", "irrelevantCol_0000165", "irrelevantCol_0000166", "irrelevantCol_0000167", "irrelevantCol_0000168", "irrelevantCol_0000169", "irrelevantCol_0000170", "irrelevantCol_0000171", "irrelevantCol_0000172", "irrelevantCol_0000173", "irrelevantCol_0000174", "irrelevantCol_0000175", "irrelevantCol_0000176", "irrelevantCol_0000177", "irrelevantCol_0000178", "irrelevantCol_0000179", "irrelevantCol_0000180", "irrelevantCol_0000181", "irrelevantCol_0000182", "irrelevantCol_0000183", "irrelevantCol_0000184", "irrelevantCol_0000185", "irrelevantCol_0000186", "irrelevantCol_0000187", "irrelevantCol_0000188", "irrelevantCol_0000189", "irrelevantCol_0000190", "irrelevantCol_0000191", "irrelevantCol_0000192", "irrelevantCol_0000193", "irrelevantCol_0000194", "irrelevantCol_0000195", "irrelevantCol_0000196", "irrelevantCol_0000197", "irrelevantCol_0000198", "irrelevantCol_0000199", "irrelevantCol_0000200", EXP("assessmentTotal" * 0.237) / sum(EXP("assessmentTotal" * 0.237)) OVER (PARTITION BY "subjectID") AS "probability"
    ## FROM "rquery_mat_86511684861390906969_0000000000") "wozqrvqdtt"
    ## ORDER BY "probability", "surveyCategory") "mmdkskanzm") "ngxbnbjeec"
    ## WHERE ("zzz4" = "zzz5")) "nvwiivfomm") "erqnwepwsk"
    ## ORDER BY "ZubjectIDZZZ"

``` r
# Fortunately, the database's query analyzer does catch the error quickly
# in this case.
system.time(nrow(as.data.frame(dplyr_pipeline_late_error)))
```

    ## Error in postgresqlExecStatement(conn, statement, ...): RS-DBI driver: (could not Retrieve the result : ERROR:  column "ZubjectIDZZZ" does not exist
    ## LINE 10: ORDER BY "ZubjectIDZZZ"
    ##                   ^
    ## )

    ## Timing stopped at: 0.076 0.001 0.081

``` r
base::date()
```

    ## [1] "Fri May 11 22:53:57 2018"

``` r
base::date()
```

    ## [1] "Fri May 11 22:53:57 2018"

``` r
DBI::dbDisconnect(my_db)
```

    ## [1] TRUE

``` r
base::date()
```

    ## [1] "Fri May 11 22:53:57 2018"
