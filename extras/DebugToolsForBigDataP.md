Debug Tools for Big Data (PostgreSQL)
================
2018-05-15

<!-- file.md is generated from file.Rmd. Please edit that file -->
``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:01 2019"

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
library("wrapr")
```

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     coalesce

``` r
my_db <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                        host = 'localhost',
                        port = 5432,
                        user = 'johnmount',
                        password = '')

# configure rquery options
dbopts <- rq_connection_tests(
  my_db,
  # seeing a bug here in RPostgreSQL, possibly on CREATE queries
  overrides = c(use_DBI_dbExistsTable = FALSE, use_DBI_dbRemoveTable = FALSE) 
)
print(dbopts)
```

    ## $rquery.PostgreSQLConnection.use_DBI_dbListFields
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbRemoveTable
    ## [1] FALSE
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
    ## $rquery.PostgreSQLConnection.control_temporary_view
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.control_rownames
    ## [1] TRUE
    ## 
    ## $rquery.PostgreSQLConnection.use_DBI_dbExistsTable
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.check_logical_column_types
    ## [1] FALSE
    ## 
    ## $rquery.PostgreSQLConnection.fn_name_map
    ##  mean 
    ## "avg"

``` r
options(dbopts)

base::date()
```

    ## [1] "Sat Jan 12 13:02:02 2019"

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:02 2019"

``` r
# build up medium sized example data
nSubj <- 100000
nIrrelCol <- 500

d_local <- data.frame(subjectID = sort(rep(seq_len(nSubj),2)),
                 surveyCategory = c(
                   'withdrawal behavior',
                   'positive re-framing'),
                 stringsAsFactors = FALSE)
d_local$assessmentTotal <- sample.int(10, nrow(d_local), replace = TRUE)
irrel_col_1 <- paste("irrelevantCol", sprintf("%07g", 1), sep = "_")
d_local[[irrel_col_1]] <- runif(nrow(d_local))
d_small <- rquery::rq_copy_to(my_db, 'd_small',
                 d_local,
                 overwrite = TRUE, 
                 temporary = TRUE)
rm(list = "d_local")
# cdata::qlook(my_db, d_small$table_name)

base::date()
```

    ## [1] "Sat Jan 12 13:02:02 2019"

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:02 2019"

``` r
# add in irrelevant columns
# simulates performing a calculation against a larger data mart
assignments <- 
  vapply(2:nIrrelCol, 
         function(i) {
           paste("irrelevantCol", sprintf("%07g", i), sep = "_")
         }, character(1)) := 
  vapply(2:nIrrelCol, 
         function(i) {
           paste(irrel_col_1, "+", i)
         }, character(1))
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
rquery::rq_nrow(my_db, d_large$table_name)
```

    ## [1] 2e+05

``` r
length(column_names(d_large))
```

    ## [1] 503

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:14 2019"

Define and demonstrate pipelines:

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:14 2019"

``` r
system.time({
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
               orderby = c('probability', 'surveyCategory'),
               reverse = c('probability', 'surveyCategory')) %.>%
    rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
    select_columns(., qc(subjectID, diagnosis, probability)) %.>%
    orderby(., 'subjectID') 
})
```

    ##    user  system elapsed 
    ##   0.012   0.000   0.012

``` r
# special debug-mode limits all sources to 1 row.
# not correct for windowed calculations or joins- 
# but lets us at least see something execute quickly.
system.time(nrow(as.data.frame(execute(my_db, rquery_pipeline, 
                                       source_limit = 1L))))
```

    ##    user  system elapsed 
    ##   0.019   0.000   0.021

``` r
# full run
system.time(nrow(as.data.frame(execute(my_db, rquery_pipeline))))
```

    ##    user  system elapsed 
    ##   0.085   0.012   2.959

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:17 2019"

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:17 2019"

``` r
system.time({
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
})
```

    ##    user  system elapsed 
    ##   0.020   0.001   0.021

``` r
# full run
system.time(nrow(as.data.frame(dplyr_pipeline)))
```

    ##    user  system elapsed 
    ##   0.171   0.020   5.446

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:23 2019"

`rquery` `materialize_node()` works with `rquery`'s column narrowing calculations.

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:23 2019"

``` r
system.time({
  scale <- 0.237
  
  rquery_pipeline_cached <- d_large %.>%
    extend_nse(.,
               probability :=
                 exp(assessmentTotal * scale))  %.>% 
    normalize_cols(.,
                   "probability",
                   partitionby = 'subjectID') %.>%
    pick_top_k(.,
               partitionby = 'subjectID',
               orderby = c('probability', 'surveyCategory'),
               reverse = c('probability', 'surveyCategory')) %.>%
    materialize_node(., "tmp_res") %.>%  # <- insert caching node into pipeline prior to narrowing
    rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
    select_columns(., qc(subjectID, diagnosis, probability)) %.>%
    orderby(., 'subjectID') 
  
  sql_list <- to_sql(rquery_pipeline_cached, my_db)
})
```

    ##    user  system elapsed 
    ##   0.026   0.000   0.025

``` r
for(i in seq_len(length(sql_list))) {
  print(paste("step", i))
  cat(format(sql_list[[i]]))
  cat("\n")
}
```

    ## [1] "step 1"
    ## CREATE TEMPORARY TABLE "tmp_res"  AS     SELECT * FROM (
    ##      SELECT
    ##       "subjectID",
    ##       "surveyCategory",
    ##       "probability",
    ##       row_number ( ) OVER (  PARTITION BY "subjectID" ORDER BY "probability" DESC, "surveyCategory" DESC ) AS "row_number"
    ##      FROM (
    ##       SELECT
    ##        "subjectID",
    ##        "surveyCategory",
    ##        "probability" / sum ( "probability" ) OVER (  PARTITION BY "subjectID" ) AS "probability"
    ##       FROM (
    ##        SELECT
    ##         "subjectID",
    ##         "surveyCategory",
    ##         exp ( "assessmentTotal" * 0.237 )  AS "probability"
    ##        FROM (
    ##         SELECT
    ##          "subjectID",
    ##          "surveyCategory",
    ##          "assessmentTotal"
    ##         FROM
    ##          "rquery_mat_32917467332525125588_0000000000"
    ##         ) tsql_49612101967505429168_0000000000
    ##        ) tsql_49612101967505429168_0000000001
    ##       ) tsql_49612101967505429168_0000000002
    ##     ) tsql_49612101967505429168_0000000003
    ##     WHERE "row_number" <= 1
    ## [1] "step 2"
    ## non SQL step:  materialize_node(tmp_res)
    ## [1] "step 3"
    ## SELECT * FROM (
    ##  SELECT
    ##   "subjectID",
    ##   "diagnosis",
    ##   "probability"
    ##  FROM (
    ##   SELECT
    ##    "subjectID" AS "subjectID",
    ##    "surveyCategory" AS "diagnosis",
    ##    "probability" AS "probability"
    ##   FROM (
    ##     SELECT
    ##      "subjectID",
    ##      "surveyCategory",
    ##      "probability"
    ##     FROM
    ##      "tmp_res"
    ##   ) tsql_49612101967505429168_0000000004
    ##  ) tsql_49612101967505429168_0000000005
    ## ) tsql_49612101967505429168_0000000006 ORDER BY "subjectID"

``` r
# special debug-mode limits all sources to 1 row.
# not correct for windowed calculations or joins- 
# but lets us at least see something execute quickly.
system.time(nrow(as.data.frame(execute(my_db, rquery_pipeline_cached, source_limit = 1L))))
```

    ##    user  system elapsed 
    ##   0.039   0.001   0.053

``` r
# full run
system.time(nrow(as.data.frame(execute(my_db, rquery_pipeline_cached))))
```

    ##    user  system elapsed 
    ##   0.101   0.011   2.917

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:26 2019"

The introduction of a `dplyr::compute()` node (with the intent of speeding things up through caching) can be expensive.

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:02:26 2019"

``` r
system.time({
  scale <- 0.237
  
  d_large_tbl %>%
    group_by(subjectID) %>%
    mutate(probability =
             exp(assessmentTotal * scale)/
             sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
    arrange(probability, surveyCategory) %>%
    filter(row_number() == n()) %>%
    compute() %>%     # <- inopportune place to try to cache
    ungroup() %>%
    rename(diagnosis = surveyCategory) %>%
    select(subjectID, diagnosis, probability) %>%
    arrange(subjectID) %>%
    as.data.frame() %>% # force calculation, and bring back result
    nrow()
})
```

    ##    user  system elapsed 
    ##   0.206   0.024  41.324

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:03:08 2019"

Now, let's show how/where erroneous pipelines are debugged in each system.

In `rquery` many user errors are caught during pipeline construction, independent of database.

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:03:08 2019"

``` r
system.time({
  scale <- 0.237
  
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
               orderby = c('probability', 'surveyCategory'),
               reverse = c('probability', 'surveyCategory')) %.>%
    rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
    select_columns(., qc(subjectID, diagnosis, probability)) %.>%
    orderby(., 'ZubjectIDZZZ') # <- error non-existent column
})
```

    ## Error in check_have_cols(have, cols, "rquery::orderby.relop"): rquery::orderby.relop unknown columns ZubjectIDZZZ

    ## Timing stopped at: 0.008 0 0.008

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:03:08 2019"

With `dplyr` user errors are mostly caught when the command is analyzed on the remote data system.

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:03:08 2019"

``` r
system.time({
  scale <- 0.237
  
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
})
```

    ##    user  system elapsed 
    ##   0.011   0.000   0.011

``` r
# dplyr will generate (incorrect) SQL from the incorrect pipeline
cat(dbplyr::remote_query(dplyr_pipeline_late_error))
```

    ## SELECT "subjectID", "diagnosis", "probability"
    ## FROM (SELECT "subjectID", "surveyCategory" AS "diagnosis", "assessmentTotal", "irrelevantCol_0000001", "irrelevantCol_0000002", "irrelevantCol_0000003", "irrelevantCol_0000004", "irrelevantCol_0000005", "irrelevantCol_0000006", "irrelevantCol_0000007", "irrelevantCol_0000008", "irrelevantCol_0000009", "irrelevantCol_0000010", "irrelevantCol_0000011", "irrelevantCol_0000012", "irrelevantCol_0000013", "irrelevantCol_0000014", "irrelevantCol_0000015", "irrelevantCol_0000016", "irrelevantCol_0000017", "irrelevantCol_0000018", "irrelevantCol_0000019", "irrelevantCol_0000020", "irrelevantCol_0000021", "irrelevantCol_0000022", "irrelevantCol_0000023", "irrelevantCol_0000024", "irrelevantCol_0000025", "irrelevantCol_0000026", "irrelevantCol_0000027", "irrelevantCol_0000028", "irrelevantCol_0000029", "irrelevantCol_0000030", "irrelevantCol_0000031", "irrelevantCol_0000032", "irrelevantCol_0000033", "irrelevantCol_0000034", "irrelevantCol_0000035", "irrelevantCol_0000036", "irrelevantCol_0000037", "irrelevantCol_0000038", "irrelevantCol_0000039", "irrelevantCol_0000040", "irrelevantCol_0000041", "irrelevantCol_0000042", "irrelevantCol_0000043", "irrelevantCol_0000044", "irrelevantCol_0000045", "irrelevantCol_0000046", "irrelevantCol_0000047", "irrelevantCol_0000048", "irrelevantCol_0000049", "irrelevantCol_0000050", "irrelevantCol_0000051", "irrelevantCol_0000052", "irrelevantCol_0000053", "irrelevantCol_0000054", "irrelevantCol_0000055", "irrelevantCol_0000056", "irrelevantCol_0000057", "irrelevantCol_0000058", "irrelevantCol_0000059", "irrelevantCol_0000060", "irrelevantCol_0000061", "irrelevantCol_0000062", "irrelevantCol_0000063", "irrelevantCol_0000064", "irrelevantCol_0000065", "irrelevantCol_0000066", "irrelevantCol_0000067", "irrelevantCol_0000068", "irrelevantCol_0000069", "irrelevantCol_0000070", "irrelevantCol_0000071", "irrelevantCol_0000072", "irrelevantCol_0000073", "irrelevantCol_0000074", "irrelevantCol_0000075", "irrelevantCol_0000076", "irrelevantCol_0000077", "irrelevantCol_0000078", "irrelevantCol_0000079", "irrelevantCol_0000080", "irrelevantCol_0000081", "irrelevantCol_0000082", "irrelevantCol_0000083", "irrelevantCol_0000084", "irrelevantCol_0000085", "irrelevantCol_0000086", "irrelevantCol_0000087", "irrelevantCol_0000088", "irrelevantCol_0000089", "irrelevantCol_0000090", "irrelevantCol_0000091", "irrelevantCol_0000092", "irrelevantCol_0000093", "irrelevantCol_0000094", "irrelevantCol_0000095", "irrelevantCol_0000096", "irrelevantCol_0000097", "irrelevantCol_0000098", "irrelevantCol_0000099", "irrelevantCol_0000100", "irrelevantCol_0000101", "irrelevantCol_0000102", "irrelevantCol_0000103", "irrelevantCol_0000104", "irrelevantCol_0000105", "irrelevantCol_0000106", "irrelevantCol_0000107", "irrelevantCol_0000108", "irrelevantCol_0000109", "irrelevantCol_0000110", "irrelevantCol_0000111", "irrelevantCol_0000112", "irrelevantCol_0000113", "irrelevantCol_0000114", "irrelevantCol_0000115", "irrelevantCol_0000116", "irrelevantCol_0000117", "irrelevantCol_0000118", "irrelevantCol_0000119", "irrelevantCol_0000120", "irrelevantCol_0000121", "irrelevantCol_0000122", "irrelevantCol_0000123", "irrelevantCol_0000124", "irrelevantCol_0000125", "irrelevantCol_0000126", "irrelevantCol_0000127", "irrelevantCol_0000128", "irrelevantCol_0000129", "irrelevantCol_0000130", "irrelevantCol_0000131", "irrelevantCol_0000132", "irrelevantCol_0000133", "irrelevantCol_0000134", "irrelevantCol_0000135", "irrelevantCol_0000136", "irrelevantCol_0000137", "irrelevantCol_0000138", "irrelevantCol_0000139", "irrelevantCol_0000140", "irrelevantCol_0000141", "irrelevantCol_0000142", "irrelevantCol_0000143", "irrelevantCol_0000144", "irrelevantCol_0000145", "irrelevantCol_0000146", "irrelevantCol_0000147", "irrelevantCol_0000148", "irrelevantCol_0000149", "irrelevantCol_0000150", "irrelevantCol_0000151", "irrelevantCol_0000152", "irrelevantCol_0000153", "irrelevantCol_0000154", "irrelevantCol_0000155", "irrelevantCol_0000156", "irrelevantCol_0000157", "irrelevantCol_0000158", "irrelevantCol_0000159", "irrelevantCol_0000160", "irrelevantCol_0000161", "irrelevantCol_0000162", "irrelevantCol_0000163", "irrelevantCol_0000164", "irrelevantCol_0000165", "irrelevantCol_0000166", "irrelevantCol_0000167", "irrelevantCol_0000168", "irrelevantCol_0000169", "irrelevantCol_0000170", "irrelevantCol_0000171", "irrelevantCol_0000172", "irrelevantCol_0000173", "irrelevantCol_0000174", "irrelevantCol_0000175", "irrelevantCol_0000176", "irrelevantCol_0000177", "irrelevantCol_0000178", "irrelevantCol_0000179", "irrelevantCol_0000180", "irrelevantCol_0000181", "irrelevantCol_0000182", "irrelevantCol_0000183", "irrelevantCol_0000184", "irrelevantCol_0000185", "irrelevantCol_0000186", "irrelevantCol_0000187", "irrelevantCol_0000188", "irrelevantCol_0000189", "irrelevantCol_0000190", "irrelevantCol_0000191", "irrelevantCol_0000192", "irrelevantCol_0000193", "irrelevantCol_0000194", "irrelevantCol_0000195", "irrelevantCol_0000196", "irrelevantCol_0000197", "irrelevantCol_0000198", "irrelevantCol_0000199", "irrelevantCol_0000200", "irrelevantCol_0000201", "irrelevantCol_0000202", "irrelevantCol_0000203", "irrelevantCol_0000204", "irrelevantCol_0000205", "irrelevantCol_0000206", "irrelevantCol_0000207", "irrelevantCol_0000208", "irrelevantCol_0000209", "irrelevantCol_0000210", "irrelevantCol_0000211", "irrelevantCol_0000212", "irrelevantCol_0000213", "irrelevantCol_0000214", "irrelevantCol_0000215", "irrelevantCol_0000216", "irrelevantCol_0000217", "irrelevantCol_0000218", "irrelevantCol_0000219", "irrelevantCol_0000220", "irrelevantCol_0000221", "irrelevantCol_0000222", "irrelevantCol_0000223", "irrelevantCol_0000224", "irrelevantCol_0000225", "irrelevantCol_0000226", "irrelevantCol_0000227", "irrelevantCol_0000228", "irrelevantCol_0000229", "irrelevantCol_0000230", "irrelevantCol_0000231", "irrelevantCol_0000232", "irrelevantCol_0000233", "irrelevantCol_0000234", "irrelevantCol_0000235", "irrelevantCol_0000236", "irrelevantCol_0000237", "irrelevantCol_0000238", "irrelevantCol_0000239", "irrelevantCol_0000240", "irrelevantCol_0000241", "irrelevantCol_0000242", "irrelevantCol_0000243", "irrelevantCol_0000244", "irrelevantCol_0000245", "irrelevantCol_0000246", "irrelevantCol_0000247", "irrelevantCol_0000248", "irrelevantCol_0000249", "irrelevantCol_0000250", "irrelevantCol_0000251", "irrelevantCol_0000252", "irrelevantCol_0000253", "irrelevantCol_0000254", "irrelevantCol_0000255", "irrelevantCol_0000256", "irrelevantCol_0000257", "irrelevantCol_0000258", "irrelevantCol_0000259", "irrelevantCol_0000260", "irrelevantCol_0000261", "irrelevantCol_0000262", "irrelevantCol_0000263", "irrelevantCol_0000264", "irrelevantCol_0000265", "irrelevantCol_0000266", "irrelevantCol_0000267", "irrelevantCol_0000268", "irrelevantCol_0000269", "irrelevantCol_0000270", "irrelevantCol_0000271", "irrelevantCol_0000272", "irrelevantCol_0000273", "irrelevantCol_0000274", "irrelevantCol_0000275", "irrelevantCol_0000276", "irrelevantCol_0000277", "irrelevantCol_0000278", "irrelevantCol_0000279", "irrelevantCol_0000280", "irrelevantCol_0000281", "irrelevantCol_0000282", "irrelevantCol_0000283", "irrelevantCol_0000284", "irrelevantCol_0000285", "irrelevantCol_0000286", "irrelevantCol_0000287", "irrelevantCol_0000288", "irrelevantCol_0000289", "irrelevantCol_0000290", "irrelevantCol_0000291", "irrelevantCol_0000292", "irrelevantCol_0000293", "irrelevantCol_0000294", "irrelevantCol_0000295", "irrelevantCol_0000296", "irrelevantCol_0000297", "irrelevantCol_0000298", "irrelevantCol_0000299", "irrelevantCol_0000300", "irrelevantCol_0000301", "irrelevantCol_0000302", "irrelevantCol_0000303", "irrelevantCol_0000304", "irrelevantCol_0000305", "irrelevantCol_0000306", "irrelevantCol_0000307", "irrelevantCol_0000308", "irrelevantCol_0000309", "irrelevantCol_0000310", "irrelevantCol_0000311", "irrelevantCol_0000312", "irrelevantCol_0000313", "irrelevantCol_0000314", "irrelevantCol_0000315", "irrelevantCol_0000316", "irrelevantCol_0000317", "irrelevantCol_0000318", "irrelevantCol_0000319", "irrelevantCol_0000320", "irrelevantCol_0000321", "irrelevantCol_0000322", "irrelevantCol_0000323", "irrelevantCol_0000324", "irrelevantCol_0000325", "irrelevantCol_0000326", "irrelevantCol_0000327", "irrelevantCol_0000328", "irrelevantCol_0000329", "irrelevantCol_0000330", "irrelevantCol_0000331", "irrelevantCol_0000332", "irrelevantCol_0000333", "irrelevantCol_0000334", "irrelevantCol_0000335", "irrelevantCol_0000336", "irrelevantCol_0000337", "irrelevantCol_0000338", "irrelevantCol_0000339", "irrelevantCol_0000340", "irrelevantCol_0000341", "irrelevantCol_0000342", "irrelevantCol_0000343", "irrelevantCol_0000344", "irrelevantCol_0000345", "irrelevantCol_0000346", "irrelevantCol_0000347", "irrelevantCol_0000348", "irrelevantCol_0000349", "irrelevantCol_0000350", "irrelevantCol_0000351", "irrelevantCol_0000352", "irrelevantCol_0000353", "irrelevantCol_0000354", "irrelevantCol_0000355", "irrelevantCol_0000356", "irrelevantCol_0000357", "irrelevantCol_0000358", "irrelevantCol_0000359", "irrelevantCol_0000360", "irrelevantCol_0000361", "irrelevantCol_0000362", "irrelevantCol_0000363", "irrelevantCol_0000364", "irrelevantCol_0000365", "irrelevantCol_0000366", "irrelevantCol_0000367", "irrelevantCol_0000368", "irrelevantCol_0000369", "irrelevantCol_0000370", "irrelevantCol_0000371", "irrelevantCol_0000372", "irrelevantCol_0000373", "irrelevantCol_0000374", "irrelevantCol_0000375", "irrelevantCol_0000376", "irrelevantCol_0000377", "irrelevantCol_0000378", "irrelevantCol_0000379", "irrelevantCol_0000380", "irrelevantCol_0000381", "irrelevantCol_0000382", "irrelevantCol_0000383", "irrelevantCol_0000384", "irrelevantCol_0000385", "irrelevantCol_0000386", "irrelevantCol_0000387", "irrelevantCol_0000388", "irrelevantCol_0000389", "irrelevantCol_0000390", "irrelevantCol_0000391", "irrelevantCol_0000392", "irrelevantCol_0000393", "irrelevantCol_0000394", "irrelevantCol_0000395", "irrelevantCol_0000396", "irrelevantCol_0000397", "irrelevantCol_0000398", "irrelevantCol_0000399", "irrelevantCol_0000400", "irrelevantCol_0000401", "irrelevantCol_0000402", "irrelevantCol_0000403", "irrelevantCol_0000404", "irrelevantCol_0000405", "irrelevantCol_0000406", "irrelevantCol_0000407", "irrelevantCol_0000408", "irrelevantCol_0000409", "irrelevantCol_0000410", "irrelevantCol_0000411", "irrelevantCol_0000412", "irrelevantCol_0000413", "irrelevantCol_0000414", "irrelevantCol_0000415", "irrelevantCol_0000416", "irrelevantCol_0000417", "irrelevantCol_0000418", "irrelevantCol_0000419", "irrelevantCol_0000420", "irrelevantCol_0000421", "irrelevantCol_0000422", "irrelevantCol_0000423", "irrelevantCol_0000424", "irrelevantCol_0000425", "irrelevantCol_0000426", "irrelevantCol_0000427", "irrelevantCol_0000428", "irrelevantCol_0000429", "irrelevantCol_0000430", "irrelevantCol_0000431", "irrelevantCol_0000432", "irrelevantCol_0000433", "irrelevantCol_0000434", "irrelevantCol_0000435", "irrelevantCol_0000436", "irrelevantCol_0000437", "irrelevantCol_0000438", "irrelevantCol_0000439", "irrelevantCol_0000440", "irrelevantCol_0000441", "irrelevantCol_0000442", "irrelevantCol_0000443", "irrelevantCol_0000444", "irrelevantCol_0000445", "irrelevantCol_0000446", "irrelevantCol_0000447", "irrelevantCol_0000448", "irrelevantCol_0000449", "irrelevantCol_0000450", "irrelevantCol_0000451", "irrelevantCol_0000452", "irrelevantCol_0000453", "irrelevantCol_0000454", "irrelevantCol_0000455", "irrelevantCol_0000456", "irrelevantCol_0000457", "irrelevantCol_0000458", "irrelevantCol_0000459", "irrelevantCol_0000460", "irrelevantCol_0000461", "irrelevantCol_0000462", "irrelevantCol_0000463", "irrelevantCol_0000464", "irrelevantCol_0000465", "irrelevantCol_0000466", "irrelevantCol_0000467", "irrelevantCol_0000468", "irrelevantCol_0000469", "irrelevantCol_0000470", "irrelevantCol_0000471", "irrelevantCol_0000472", "irrelevantCol_0000473", "irrelevantCol_0000474", "irrelevantCol_0000475", "irrelevantCol_0000476", "irrelevantCol_0000477", "irrelevantCol_0000478", "irrelevantCol_0000479", "irrelevantCol_0000480", "irrelevantCol_0000481", "irrelevantCol_0000482", "irrelevantCol_0000483", "irrelevantCol_0000484", "irrelevantCol_0000485", "irrelevantCol_0000486", "irrelevantCol_0000487", "irrelevantCol_0000488", "irrelevantCol_0000489", "irrelevantCol_0000490", "irrelevantCol_0000491", "irrelevantCol_0000492", "irrelevantCol_0000493", "irrelevantCol_0000494", "irrelevantCol_0000495", "irrelevantCol_0000496", "irrelevantCol_0000497", "irrelevantCol_0000498", "irrelevantCol_0000499", "irrelevantCol_0000500", "probability"
    ## FROM (SELECT "subjectID", "surveyCategory", "assessmentTotal", "irrelevantCol_0000001", "irrelevantCol_0000002", "irrelevantCol_0000003", "irrelevantCol_0000004", "irrelevantCol_0000005", "irrelevantCol_0000006", "irrelevantCol_0000007", "irrelevantCol_0000008", "irrelevantCol_0000009", "irrelevantCol_0000010", "irrelevantCol_0000011", "irrelevantCol_0000012", "irrelevantCol_0000013", "irrelevantCol_0000014", "irrelevantCol_0000015", "irrelevantCol_0000016", "irrelevantCol_0000017", "irrelevantCol_0000018", "irrelevantCol_0000019", "irrelevantCol_0000020", "irrelevantCol_0000021", "irrelevantCol_0000022", "irrelevantCol_0000023", "irrelevantCol_0000024", "irrelevantCol_0000025", "irrelevantCol_0000026", "irrelevantCol_0000027", "irrelevantCol_0000028", "irrelevantCol_0000029", "irrelevantCol_0000030", "irrelevantCol_0000031", "irrelevantCol_0000032", "irrelevantCol_0000033", "irrelevantCol_0000034", "irrelevantCol_0000035", "irrelevantCol_0000036", "irrelevantCol_0000037", "irrelevantCol_0000038", "irrelevantCol_0000039", "irrelevantCol_0000040", "irrelevantCol_0000041", "irrelevantCol_0000042", "irrelevantCol_0000043", "irrelevantCol_0000044", "irrelevantCol_0000045", "irrelevantCol_0000046", "irrelevantCol_0000047", "irrelevantCol_0000048", "irrelevantCol_0000049", "irrelevantCol_0000050", "irrelevantCol_0000051", "irrelevantCol_0000052", "irrelevantCol_0000053", "irrelevantCol_0000054", "irrelevantCol_0000055", "irrelevantCol_0000056", "irrelevantCol_0000057", "irrelevantCol_0000058", "irrelevantCol_0000059", "irrelevantCol_0000060", "irrelevantCol_0000061", "irrelevantCol_0000062", "irrelevantCol_0000063", "irrelevantCol_0000064", "irrelevantCol_0000065", "irrelevantCol_0000066", "irrelevantCol_0000067", "irrelevantCol_0000068", "irrelevantCol_0000069", "irrelevantCol_0000070", "irrelevantCol_0000071", "irrelevantCol_0000072", "irrelevantCol_0000073", "irrelevantCol_0000074", "irrelevantCol_0000075", "irrelevantCol_0000076", "irrelevantCol_0000077", "irrelevantCol_0000078", "irrelevantCol_0000079", "irrelevantCol_0000080", "irrelevantCol_0000081", "irrelevantCol_0000082", "irrelevantCol_0000083", "irrelevantCol_0000084", "irrelevantCol_0000085", "irrelevantCol_0000086", "irrelevantCol_0000087", "irrelevantCol_0000088", "irrelevantCol_0000089", "irrelevantCol_0000090", "irrelevantCol_0000091", "irrelevantCol_0000092", "irrelevantCol_0000093", "irrelevantCol_0000094", "irrelevantCol_0000095", "irrelevantCol_0000096", "irrelevantCol_0000097", "irrelevantCol_0000098", "irrelevantCol_0000099", "irrelevantCol_0000100", "irrelevantCol_0000101", "irrelevantCol_0000102", "irrelevantCol_0000103", "irrelevantCol_0000104", "irrelevantCol_0000105", "irrelevantCol_0000106", "irrelevantCol_0000107", "irrelevantCol_0000108", "irrelevantCol_0000109", "irrelevantCol_0000110", "irrelevantCol_0000111", "irrelevantCol_0000112", "irrelevantCol_0000113", "irrelevantCol_0000114", "irrelevantCol_0000115", "irrelevantCol_0000116", "irrelevantCol_0000117", "irrelevantCol_0000118", "irrelevantCol_0000119", "irrelevantCol_0000120", "irrelevantCol_0000121", "irrelevantCol_0000122", "irrelevantCol_0000123", "irrelevantCol_0000124", "irrelevantCol_0000125", "irrelevantCol_0000126", "irrelevantCol_0000127", "irrelevantCol_0000128", "irrelevantCol_0000129", "irrelevantCol_0000130", "irrelevantCol_0000131", "irrelevantCol_0000132", "irrelevantCol_0000133", "irrelevantCol_0000134", "irrelevantCol_0000135", "irrelevantCol_0000136", "irrelevantCol_0000137", "irrelevantCol_0000138", "irrelevantCol_0000139", "irrelevantCol_0000140", "irrelevantCol_0000141", "irrelevantCol_0000142", "irrelevantCol_0000143", "irrelevantCol_0000144", "irrelevantCol_0000145", "irrelevantCol_0000146", "irrelevantCol_0000147", "irrelevantCol_0000148", "irrelevantCol_0000149", "irrelevantCol_0000150", "irrelevantCol_0000151", "irrelevantCol_0000152", "irrelevantCol_0000153", "irrelevantCol_0000154", "irrelevantCol_0000155", "irrelevantCol_0000156", "irrelevantCol_0000157", "irrelevantCol_0000158", "irrelevantCol_0000159", "irrelevantCol_0000160", "irrelevantCol_0000161", "irrelevantCol_0000162", "irrelevantCol_0000163", "irrelevantCol_0000164", "irrelevantCol_0000165", "irrelevantCol_0000166", "irrelevantCol_0000167", "irrelevantCol_0000168", "irrelevantCol_0000169", "irrelevantCol_0000170", "irrelevantCol_0000171", "irrelevantCol_0000172", "irrelevantCol_0000173", "irrelevantCol_0000174", "irrelevantCol_0000175", "irrelevantCol_0000176", "irrelevantCol_0000177", "irrelevantCol_0000178", "irrelevantCol_0000179", "irrelevantCol_0000180", "irrelevantCol_0000181", "irrelevantCol_0000182", "irrelevantCol_0000183", "irrelevantCol_0000184", "irrelevantCol_0000185", "irrelevantCol_0000186", "irrelevantCol_0000187", "irrelevantCol_0000188", "irrelevantCol_0000189", "irrelevantCol_0000190", "irrelevantCol_0000191", "irrelevantCol_0000192", "irrelevantCol_0000193", "irrelevantCol_0000194", "irrelevantCol_0000195", "irrelevantCol_0000196", "irrelevantCol_0000197", "irrelevantCol_0000198", "irrelevantCol_0000199", "irrelevantCol_0000200", "irrelevantCol_0000201", "irrelevantCol_0000202", "irrelevantCol_0000203", "irrelevantCol_0000204", "irrelevantCol_0000205", "irrelevantCol_0000206", "irrelevantCol_0000207", "irrelevantCol_0000208", "irrelevantCol_0000209", "irrelevantCol_0000210", "irrelevantCol_0000211", "irrelevantCol_0000212", "irrelevantCol_0000213", "irrelevantCol_0000214", "irrelevantCol_0000215", "irrelevantCol_0000216", "irrelevantCol_0000217", "irrelevantCol_0000218", "irrelevantCol_0000219", "irrelevantCol_0000220", "irrelevantCol_0000221", "irrelevantCol_0000222", "irrelevantCol_0000223", "irrelevantCol_0000224", "irrelevantCol_0000225", "irrelevantCol_0000226", "irrelevantCol_0000227", "irrelevantCol_0000228", "irrelevantCol_0000229", "irrelevantCol_0000230", "irrelevantCol_0000231", "irrelevantCol_0000232", "irrelevantCol_0000233", "irrelevantCol_0000234", "irrelevantCol_0000235", "irrelevantCol_0000236", "irrelevantCol_0000237", "irrelevantCol_0000238", "irrelevantCol_0000239", "irrelevantCol_0000240", "irrelevantCol_0000241", "irrelevantCol_0000242", "irrelevantCol_0000243", "irrelevantCol_0000244", "irrelevantCol_0000245", "irrelevantCol_0000246", "irrelevantCol_0000247", "irrelevantCol_0000248", "irrelevantCol_0000249", "irrelevantCol_0000250", "irrelevantCol_0000251", "irrelevantCol_0000252", "irrelevantCol_0000253", "irrelevantCol_0000254", "irrelevantCol_0000255", "irrelevantCol_0000256", "irrelevantCol_0000257", "irrelevantCol_0000258", "irrelevantCol_0000259", "irrelevantCol_0000260", "irrelevantCol_0000261", "irrelevantCol_0000262", "irrelevantCol_0000263", "irrelevantCol_0000264", "irrelevantCol_0000265", "irrelevantCol_0000266", "irrelevantCol_0000267", "irrelevantCol_0000268", "irrelevantCol_0000269", "irrelevantCol_0000270", "irrelevantCol_0000271", "irrelevantCol_0000272", "irrelevantCol_0000273", "irrelevantCol_0000274", "irrelevantCol_0000275", "irrelevantCol_0000276", "irrelevantCol_0000277", "irrelevantCol_0000278", "irrelevantCol_0000279", "irrelevantCol_0000280", "irrelevantCol_0000281", "irrelevantCol_0000282", "irrelevantCol_0000283", "irrelevantCol_0000284", "irrelevantCol_0000285", "irrelevantCol_0000286", "irrelevantCol_0000287", "irrelevantCol_0000288", "irrelevantCol_0000289", "irrelevantCol_0000290", "irrelevantCol_0000291", "irrelevantCol_0000292", "irrelevantCol_0000293", "irrelevantCol_0000294", "irrelevantCol_0000295", "irrelevantCol_0000296", "irrelevantCol_0000297", "irrelevantCol_0000298", "irrelevantCol_0000299", "irrelevantCol_0000300", "irrelevantCol_0000301", "irrelevantCol_0000302", "irrelevantCol_0000303", "irrelevantCol_0000304", "irrelevantCol_0000305", "irrelevantCol_0000306", "irrelevantCol_0000307", "irrelevantCol_0000308", "irrelevantCol_0000309", "irrelevantCol_0000310", "irrelevantCol_0000311", "irrelevantCol_0000312", "irrelevantCol_0000313", "irrelevantCol_0000314", "irrelevantCol_0000315", "irrelevantCol_0000316", "irrelevantCol_0000317", "irrelevantCol_0000318", "irrelevantCol_0000319", "irrelevantCol_0000320", "irrelevantCol_0000321", "irrelevantCol_0000322", "irrelevantCol_0000323", "irrelevantCol_0000324", "irrelevantCol_0000325", "irrelevantCol_0000326", "irrelevantCol_0000327", "irrelevantCol_0000328", "irrelevantCol_0000329", "irrelevantCol_0000330", "irrelevantCol_0000331", "irrelevantCol_0000332", "irrelevantCol_0000333", "irrelevantCol_0000334", "irrelevantCol_0000335", "irrelevantCol_0000336", "irrelevantCol_0000337", "irrelevantCol_0000338", "irrelevantCol_0000339", "irrelevantCol_0000340", "irrelevantCol_0000341", "irrelevantCol_0000342", "irrelevantCol_0000343", "irrelevantCol_0000344", "irrelevantCol_0000345", "irrelevantCol_0000346", "irrelevantCol_0000347", "irrelevantCol_0000348", "irrelevantCol_0000349", "irrelevantCol_0000350", "irrelevantCol_0000351", "irrelevantCol_0000352", "irrelevantCol_0000353", "irrelevantCol_0000354", "irrelevantCol_0000355", "irrelevantCol_0000356", "irrelevantCol_0000357", "irrelevantCol_0000358", "irrelevantCol_0000359", "irrelevantCol_0000360", "irrelevantCol_0000361", "irrelevantCol_0000362", "irrelevantCol_0000363", "irrelevantCol_0000364", "irrelevantCol_0000365", "irrelevantCol_0000366", "irrelevantCol_0000367", "irrelevantCol_0000368", "irrelevantCol_0000369", "irrelevantCol_0000370", "irrelevantCol_0000371", "irrelevantCol_0000372", "irrelevantCol_0000373", "irrelevantCol_0000374", "irrelevantCol_0000375", "irrelevantCol_0000376", "irrelevantCol_0000377", "irrelevantCol_0000378", "irrelevantCol_0000379", "irrelevantCol_0000380", "irrelevantCol_0000381", "irrelevantCol_0000382", "irrelevantCol_0000383", "irrelevantCol_0000384", "irrelevantCol_0000385", "irrelevantCol_0000386", "irrelevantCol_0000387", "irrelevantCol_0000388", "irrelevantCol_0000389", "irrelevantCol_0000390", "irrelevantCol_0000391", "irrelevantCol_0000392", "irrelevantCol_0000393", "irrelevantCol_0000394", "irrelevantCol_0000395", "irrelevantCol_0000396", "irrelevantCol_0000397", "irrelevantCol_0000398", "irrelevantCol_0000399", "irrelevantCol_0000400", "irrelevantCol_0000401", "irrelevantCol_0000402", "irrelevantCol_0000403", "irrelevantCol_0000404", "irrelevantCol_0000405", "irrelevantCol_0000406", "irrelevantCol_0000407", "irrelevantCol_0000408", "irrelevantCol_0000409", "irrelevantCol_0000410", "irrelevantCol_0000411", "irrelevantCol_0000412", "irrelevantCol_0000413", "irrelevantCol_0000414", "irrelevantCol_0000415", "irrelevantCol_0000416", "irrelevantCol_0000417", "irrelevantCol_0000418", "irrelevantCol_0000419", "irrelevantCol_0000420", "irrelevantCol_0000421", "irrelevantCol_0000422", "irrelevantCol_0000423", "irrelevantCol_0000424", "irrelevantCol_0000425", "irrelevantCol_0000426", "irrelevantCol_0000427", "irrelevantCol_0000428", "irrelevantCol_0000429", "irrelevantCol_0000430", "irrelevantCol_0000431", "irrelevantCol_0000432", "irrelevantCol_0000433", "irrelevantCol_0000434", "irrelevantCol_0000435", "irrelevantCol_0000436", "irrelevantCol_0000437", "irrelevantCol_0000438", "irrelevantCol_0000439", "irrelevantCol_0000440", "irrelevantCol_0000441", "irrelevantCol_0000442", "irrelevantCol_0000443", "irrelevantCol_0000444", "irrelevantCol_0000445", "irrelevantCol_0000446", "irrelevantCol_0000447", "irrelevantCol_0000448", "irrelevantCol_0000449", "irrelevantCol_0000450", "irrelevantCol_0000451", "irrelevantCol_0000452", "irrelevantCol_0000453", "irrelevantCol_0000454", "irrelevantCol_0000455", "irrelevantCol_0000456", "irrelevantCol_0000457", "irrelevantCol_0000458", "irrelevantCol_0000459", "irrelevantCol_0000460", "irrelevantCol_0000461", "irrelevantCol_0000462", "irrelevantCol_0000463", "irrelevantCol_0000464", "irrelevantCol_0000465", "irrelevantCol_0000466", "irrelevantCol_0000467", "irrelevantCol_0000468", "irrelevantCol_0000469", "irrelevantCol_0000470", "irrelevantCol_0000471", "irrelevantCol_0000472", "irrelevantCol_0000473", "irrelevantCol_0000474", "irrelevantCol_0000475", "irrelevantCol_0000476", "irrelevantCol_0000477", "irrelevantCol_0000478", "irrelevantCol_0000479", "irrelevantCol_0000480", "irrelevantCol_0000481", "irrelevantCol_0000482", "irrelevantCol_0000483", "irrelevantCol_0000484", "irrelevantCol_0000485", "irrelevantCol_0000486", "irrelevantCol_0000487", "irrelevantCol_0000488", "irrelevantCol_0000489", "irrelevantCol_0000490", "irrelevantCol_0000491", "irrelevantCol_0000492", "irrelevantCol_0000493", "irrelevantCol_0000494", "irrelevantCol_0000495", "irrelevantCol_0000496", "irrelevantCol_0000497", "irrelevantCol_0000498", "irrelevantCol_0000499", "irrelevantCol_0000500", "probability"
    ## FROM (SELECT "subjectID", "surveyCategory", "assessmentTotal", "irrelevantCol_0000001", "irrelevantCol_0000002", "irrelevantCol_0000003", "irrelevantCol_0000004", "irrelevantCol_0000005", "irrelevantCol_0000006", "irrelevantCol_0000007", "irrelevantCol_0000008", "irrelevantCol_0000009", "irrelevantCol_0000010", "irrelevantCol_0000011", "irrelevantCol_0000012", "irrelevantCol_0000013", "irrelevantCol_0000014", "irrelevantCol_0000015", "irrelevantCol_0000016", "irrelevantCol_0000017", "irrelevantCol_0000018", "irrelevantCol_0000019", "irrelevantCol_0000020", "irrelevantCol_0000021", "irrelevantCol_0000022", "irrelevantCol_0000023", "irrelevantCol_0000024", "irrelevantCol_0000025", "irrelevantCol_0000026", "irrelevantCol_0000027", "irrelevantCol_0000028", "irrelevantCol_0000029", "irrelevantCol_0000030", "irrelevantCol_0000031", "irrelevantCol_0000032", "irrelevantCol_0000033", "irrelevantCol_0000034", "irrelevantCol_0000035", "irrelevantCol_0000036", "irrelevantCol_0000037", "irrelevantCol_0000038", "irrelevantCol_0000039", "irrelevantCol_0000040", "irrelevantCol_0000041", "irrelevantCol_0000042", "irrelevantCol_0000043", "irrelevantCol_0000044", "irrelevantCol_0000045", "irrelevantCol_0000046", "irrelevantCol_0000047", "irrelevantCol_0000048", "irrelevantCol_0000049", "irrelevantCol_0000050", "irrelevantCol_0000051", "irrelevantCol_0000052", "irrelevantCol_0000053", "irrelevantCol_0000054", "irrelevantCol_0000055", "irrelevantCol_0000056", "irrelevantCol_0000057", "irrelevantCol_0000058", "irrelevantCol_0000059", "irrelevantCol_0000060", "irrelevantCol_0000061", "irrelevantCol_0000062", "irrelevantCol_0000063", "irrelevantCol_0000064", "irrelevantCol_0000065", "irrelevantCol_0000066", "irrelevantCol_0000067", "irrelevantCol_0000068", "irrelevantCol_0000069", "irrelevantCol_0000070", "irrelevantCol_0000071", "irrelevantCol_0000072", "irrelevantCol_0000073", "irrelevantCol_0000074", "irrelevantCol_0000075", "irrelevantCol_0000076", "irrelevantCol_0000077", "irrelevantCol_0000078", "irrelevantCol_0000079", "irrelevantCol_0000080", "irrelevantCol_0000081", "irrelevantCol_0000082", "irrelevantCol_0000083", "irrelevantCol_0000084", "irrelevantCol_0000085", "irrelevantCol_0000086", "irrelevantCol_0000087", "irrelevantCol_0000088", "irrelevantCol_0000089", "irrelevantCol_0000090", "irrelevantCol_0000091", "irrelevantCol_0000092", "irrelevantCol_0000093", "irrelevantCol_0000094", "irrelevantCol_0000095", "irrelevantCol_0000096", "irrelevantCol_0000097", "irrelevantCol_0000098", "irrelevantCol_0000099", "irrelevantCol_0000100", "irrelevantCol_0000101", "irrelevantCol_0000102", "irrelevantCol_0000103", "irrelevantCol_0000104", "irrelevantCol_0000105", "irrelevantCol_0000106", "irrelevantCol_0000107", "irrelevantCol_0000108", "irrelevantCol_0000109", "irrelevantCol_0000110", "irrelevantCol_0000111", "irrelevantCol_0000112", "irrelevantCol_0000113", "irrelevantCol_0000114", "irrelevantCol_0000115", "irrelevantCol_0000116", "irrelevantCol_0000117", "irrelevantCol_0000118", "irrelevantCol_0000119", "irrelevantCol_0000120", "irrelevantCol_0000121", "irrelevantCol_0000122", "irrelevantCol_0000123", "irrelevantCol_0000124", "irrelevantCol_0000125", "irrelevantCol_0000126", "irrelevantCol_0000127", "irrelevantCol_0000128", "irrelevantCol_0000129", "irrelevantCol_0000130", "irrelevantCol_0000131", "irrelevantCol_0000132", "irrelevantCol_0000133", "irrelevantCol_0000134", "irrelevantCol_0000135", "irrelevantCol_0000136", "irrelevantCol_0000137", "irrelevantCol_0000138", "irrelevantCol_0000139", "irrelevantCol_0000140", "irrelevantCol_0000141", "irrelevantCol_0000142", "irrelevantCol_0000143", "irrelevantCol_0000144", "irrelevantCol_0000145", "irrelevantCol_0000146", "irrelevantCol_0000147", "irrelevantCol_0000148", "irrelevantCol_0000149", "irrelevantCol_0000150", "irrelevantCol_0000151", "irrelevantCol_0000152", "irrelevantCol_0000153", "irrelevantCol_0000154", "irrelevantCol_0000155", "irrelevantCol_0000156", "irrelevantCol_0000157", "irrelevantCol_0000158", "irrelevantCol_0000159", "irrelevantCol_0000160", "irrelevantCol_0000161", "irrelevantCol_0000162", "irrelevantCol_0000163", "irrelevantCol_0000164", "irrelevantCol_0000165", "irrelevantCol_0000166", "irrelevantCol_0000167", "irrelevantCol_0000168", "irrelevantCol_0000169", "irrelevantCol_0000170", "irrelevantCol_0000171", "irrelevantCol_0000172", "irrelevantCol_0000173", "irrelevantCol_0000174", "irrelevantCol_0000175", "irrelevantCol_0000176", "irrelevantCol_0000177", "irrelevantCol_0000178", "irrelevantCol_0000179", "irrelevantCol_0000180", "irrelevantCol_0000181", "irrelevantCol_0000182", "irrelevantCol_0000183", "irrelevantCol_0000184", "irrelevantCol_0000185", "irrelevantCol_0000186", "irrelevantCol_0000187", "irrelevantCol_0000188", "irrelevantCol_0000189", "irrelevantCol_0000190", "irrelevantCol_0000191", "irrelevantCol_0000192", "irrelevantCol_0000193", "irrelevantCol_0000194", "irrelevantCol_0000195", "irrelevantCol_0000196", "irrelevantCol_0000197", "irrelevantCol_0000198", "irrelevantCol_0000199", "irrelevantCol_0000200", "irrelevantCol_0000201", "irrelevantCol_0000202", "irrelevantCol_0000203", "irrelevantCol_0000204", "irrelevantCol_0000205", "irrelevantCol_0000206", "irrelevantCol_0000207", "irrelevantCol_0000208", "irrelevantCol_0000209", "irrelevantCol_0000210", "irrelevantCol_0000211", "irrelevantCol_0000212", "irrelevantCol_0000213", "irrelevantCol_0000214", "irrelevantCol_0000215", "irrelevantCol_0000216", "irrelevantCol_0000217", "irrelevantCol_0000218", "irrelevantCol_0000219", "irrelevantCol_0000220", "irrelevantCol_0000221", "irrelevantCol_0000222", "irrelevantCol_0000223", "irrelevantCol_0000224", "irrelevantCol_0000225", "irrelevantCol_0000226", "irrelevantCol_0000227", "irrelevantCol_0000228", "irrelevantCol_0000229", "irrelevantCol_0000230", "irrelevantCol_0000231", "irrelevantCol_0000232", "irrelevantCol_0000233", "irrelevantCol_0000234", "irrelevantCol_0000235", "irrelevantCol_0000236", "irrelevantCol_0000237", "irrelevantCol_0000238", "irrelevantCol_0000239", "irrelevantCol_0000240", "irrelevantCol_0000241", "irrelevantCol_0000242", "irrelevantCol_0000243", "irrelevantCol_0000244", "irrelevantCol_0000245", "irrelevantCol_0000246", "irrelevantCol_0000247", "irrelevantCol_0000248", "irrelevantCol_0000249", "irrelevantCol_0000250", "irrelevantCol_0000251", "irrelevantCol_0000252", "irrelevantCol_0000253", "irrelevantCol_0000254", "irrelevantCol_0000255", "irrelevantCol_0000256", "irrelevantCol_0000257", "irrelevantCol_0000258", "irrelevantCol_0000259", "irrelevantCol_0000260", "irrelevantCol_0000261", "irrelevantCol_0000262", "irrelevantCol_0000263", "irrelevantCol_0000264", "irrelevantCol_0000265", "irrelevantCol_0000266", "irrelevantCol_0000267", "irrelevantCol_0000268", "irrelevantCol_0000269", "irrelevantCol_0000270", "irrelevantCol_0000271", "irrelevantCol_0000272", "irrelevantCol_0000273", "irrelevantCol_0000274", "irrelevantCol_0000275", "irrelevantCol_0000276", "irrelevantCol_0000277", "irrelevantCol_0000278", "irrelevantCol_0000279", "irrelevantCol_0000280", "irrelevantCol_0000281", "irrelevantCol_0000282", "irrelevantCol_0000283", "irrelevantCol_0000284", "irrelevantCol_0000285", "irrelevantCol_0000286", "irrelevantCol_0000287", "irrelevantCol_0000288", "irrelevantCol_0000289", "irrelevantCol_0000290", "irrelevantCol_0000291", "irrelevantCol_0000292", "irrelevantCol_0000293", "irrelevantCol_0000294", "irrelevantCol_0000295", "irrelevantCol_0000296", "irrelevantCol_0000297", "irrelevantCol_0000298", "irrelevantCol_0000299", "irrelevantCol_0000300", "irrelevantCol_0000301", "irrelevantCol_0000302", "irrelevantCol_0000303", "irrelevantCol_0000304", "irrelevantCol_0000305", "irrelevantCol_0000306", "irrelevantCol_0000307", "irrelevantCol_0000308", "irrelevantCol_0000309", "irrelevantCol_0000310", "irrelevantCol_0000311", "irrelevantCol_0000312", "irrelevantCol_0000313", "irrelevantCol_0000314", "irrelevantCol_0000315", "irrelevantCol_0000316", "irrelevantCol_0000317", "irrelevantCol_0000318", "irrelevantCol_0000319", "irrelevantCol_0000320", "irrelevantCol_0000321", "irrelevantCol_0000322", "irrelevantCol_0000323", "irrelevantCol_0000324", "irrelevantCol_0000325", "irrelevantCol_0000326", "irrelevantCol_0000327", "irrelevantCol_0000328", "irrelevantCol_0000329", "irrelevantCol_0000330", "irrelevantCol_0000331", "irrelevantCol_0000332", "irrelevantCol_0000333", "irrelevantCol_0000334", "irrelevantCol_0000335", "irrelevantCol_0000336", "irrelevantCol_0000337", "irrelevantCol_0000338", "irrelevantCol_0000339", "irrelevantCol_0000340", "irrelevantCol_0000341", "irrelevantCol_0000342", "irrelevantCol_0000343", "irrelevantCol_0000344", "irrelevantCol_0000345", "irrelevantCol_0000346", "irrelevantCol_0000347", "irrelevantCol_0000348", "irrelevantCol_0000349", "irrelevantCol_0000350", "irrelevantCol_0000351", "irrelevantCol_0000352", "irrelevantCol_0000353", "irrelevantCol_0000354", "irrelevantCol_0000355", "irrelevantCol_0000356", "irrelevantCol_0000357", "irrelevantCol_0000358", "irrelevantCol_0000359", "irrelevantCol_0000360", "irrelevantCol_0000361", "irrelevantCol_0000362", "irrelevantCol_0000363", "irrelevantCol_0000364", "irrelevantCol_0000365", "irrelevantCol_0000366", "irrelevantCol_0000367", "irrelevantCol_0000368", "irrelevantCol_0000369", "irrelevantCol_0000370", "irrelevantCol_0000371", "irrelevantCol_0000372", "irrelevantCol_0000373", "irrelevantCol_0000374", "irrelevantCol_0000375", "irrelevantCol_0000376", "irrelevantCol_0000377", "irrelevantCol_0000378", "irrelevantCol_0000379", "irrelevantCol_0000380", "irrelevantCol_0000381", "irrelevantCol_0000382", "irrelevantCol_0000383", "irrelevantCol_0000384", "irrelevantCol_0000385", "irrelevantCol_0000386", "irrelevantCol_0000387", "irrelevantCol_0000388", "irrelevantCol_0000389", "irrelevantCol_0000390", "irrelevantCol_0000391", "irrelevantCol_0000392", "irrelevantCol_0000393", "irrelevantCol_0000394", "irrelevantCol_0000395", "irrelevantCol_0000396", "irrelevantCol_0000397", "irrelevantCol_0000398", "irrelevantCol_0000399", "irrelevantCol_0000400", "irrelevantCol_0000401", "irrelevantCol_0000402", "irrelevantCol_0000403", "irrelevantCol_0000404", "irrelevantCol_0000405", "irrelevantCol_0000406", "irrelevantCol_0000407", "irrelevantCol_0000408", "irrelevantCol_0000409", "irrelevantCol_0000410", "irrelevantCol_0000411", "irrelevantCol_0000412", "irrelevantCol_0000413", "irrelevantCol_0000414", "irrelevantCol_0000415", "irrelevantCol_0000416", "irrelevantCol_0000417", "irrelevantCol_0000418", "irrelevantCol_0000419", "irrelevantCol_0000420", "irrelevantCol_0000421", "irrelevantCol_0000422", "irrelevantCol_0000423", "irrelevantCol_0000424", "irrelevantCol_0000425", "irrelevantCol_0000426", "irrelevantCol_0000427", "irrelevantCol_0000428", "irrelevantCol_0000429", "irrelevantCol_0000430", "irrelevantCol_0000431", "irrelevantCol_0000432", "irrelevantCol_0000433", "irrelevantCol_0000434", "irrelevantCol_0000435", "irrelevantCol_0000436", "irrelevantCol_0000437", "irrelevantCol_0000438", "irrelevantCol_0000439", "irrelevantCol_0000440", "irrelevantCol_0000441", "irrelevantCol_0000442", "irrelevantCol_0000443", "irrelevantCol_0000444", "irrelevantCol_0000445", "irrelevantCol_0000446", "irrelevantCol_0000447", "irrelevantCol_0000448", "irrelevantCol_0000449", "irrelevantCol_0000450", "irrelevantCol_0000451", "irrelevantCol_0000452", "irrelevantCol_0000453", "irrelevantCol_0000454", "irrelevantCol_0000455", "irrelevantCol_0000456", "irrelevantCol_0000457", "irrelevantCol_0000458", "irrelevantCol_0000459", "irrelevantCol_0000460", "irrelevantCol_0000461", "irrelevantCol_0000462", "irrelevantCol_0000463", "irrelevantCol_0000464", "irrelevantCol_0000465", "irrelevantCol_0000466", "irrelevantCol_0000467", "irrelevantCol_0000468", "irrelevantCol_0000469", "irrelevantCol_0000470", "irrelevantCol_0000471", "irrelevantCol_0000472", "irrelevantCol_0000473", "irrelevantCol_0000474", "irrelevantCol_0000475", "irrelevantCol_0000476", "irrelevantCol_0000477", "irrelevantCol_0000478", "irrelevantCol_0000479", "irrelevantCol_0000480", "irrelevantCol_0000481", "irrelevantCol_0000482", "irrelevantCol_0000483", "irrelevantCol_0000484", "irrelevantCol_0000485", "irrelevantCol_0000486", "irrelevantCol_0000487", "irrelevantCol_0000488", "irrelevantCol_0000489", "irrelevantCol_0000490", "irrelevantCol_0000491", "irrelevantCol_0000492", "irrelevantCol_0000493", "irrelevantCol_0000494", "irrelevantCol_0000495", "irrelevantCol_0000496", "irrelevantCol_0000497", "irrelevantCol_0000498", "irrelevantCol_0000499", "irrelevantCol_0000500", "probability", row_number() OVER (PARTITION BY "subjectID" ORDER BY "probability", "surveyCategory") AS "zzz7", COUNT(*) OVER (PARTITION BY "subjectID") AS "zzz8"
    ## FROM (SELECT *
    ## FROM (SELECT "subjectID", "surveyCategory", "assessmentTotal", "irrelevantCol_0000001", "irrelevantCol_0000002", "irrelevantCol_0000003", "irrelevantCol_0000004", "irrelevantCol_0000005", "irrelevantCol_0000006", "irrelevantCol_0000007", "irrelevantCol_0000008", "irrelevantCol_0000009", "irrelevantCol_0000010", "irrelevantCol_0000011", "irrelevantCol_0000012", "irrelevantCol_0000013", "irrelevantCol_0000014", "irrelevantCol_0000015", "irrelevantCol_0000016", "irrelevantCol_0000017", "irrelevantCol_0000018", "irrelevantCol_0000019", "irrelevantCol_0000020", "irrelevantCol_0000021", "irrelevantCol_0000022", "irrelevantCol_0000023", "irrelevantCol_0000024", "irrelevantCol_0000025", "irrelevantCol_0000026", "irrelevantCol_0000027", "irrelevantCol_0000028", "irrelevantCol_0000029", "irrelevantCol_0000030", "irrelevantCol_0000031", "irrelevantCol_0000032", "irrelevantCol_0000033", "irrelevantCol_0000034", "irrelevantCol_0000035", "irrelevantCol_0000036", "irrelevantCol_0000037", "irrelevantCol_0000038", "irrelevantCol_0000039", "irrelevantCol_0000040", "irrelevantCol_0000041", "irrelevantCol_0000042", "irrelevantCol_0000043", "irrelevantCol_0000044", "irrelevantCol_0000045", "irrelevantCol_0000046", "irrelevantCol_0000047", "irrelevantCol_0000048", "irrelevantCol_0000049", "irrelevantCol_0000050", "irrelevantCol_0000051", "irrelevantCol_0000052", "irrelevantCol_0000053", "irrelevantCol_0000054", "irrelevantCol_0000055", "irrelevantCol_0000056", "irrelevantCol_0000057", "irrelevantCol_0000058", "irrelevantCol_0000059", "irrelevantCol_0000060", "irrelevantCol_0000061", "irrelevantCol_0000062", "irrelevantCol_0000063", "irrelevantCol_0000064", "irrelevantCol_0000065", "irrelevantCol_0000066", "irrelevantCol_0000067", "irrelevantCol_0000068", "irrelevantCol_0000069", "irrelevantCol_0000070", "irrelevantCol_0000071", "irrelevantCol_0000072", "irrelevantCol_0000073", "irrelevantCol_0000074", "irrelevantCol_0000075", "irrelevantCol_0000076", "irrelevantCol_0000077", "irrelevantCol_0000078", "irrelevantCol_0000079", "irrelevantCol_0000080", "irrelevantCol_0000081", "irrelevantCol_0000082", "irrelevantCol_0000083", "irrelevantCol_0000084", "irrelevantCol_0000085", "irrelevantCol_0000086", "irrelevantCol_0000087", "irrelevantCol_0000088", "irrelevantCol_0000089", "irrelevantCol_0000090", "irrelevantCol_0000091", "irrelevantCol_0000092", "irrelevantCol_0000093", "irrelevantCol_0000094", "irrelevantCol_0000095", "irrelevantCol_0000096", "irrelevantCol_0000097", "irrelevantCol_0000098", "irrelevantCol_0000099", "irrelevantCol_0000100", "irrelevantCol_0000101", "irrelevantCol_0000102", "irrelevantCol_0000103", "irrelevantCol_0000104", "irrelevantCol_0000105", "irrelevantCol_0000106", "irrelevantCol_0000107", "irrelevantCol_0000108", "irrelevantCol_0000109", "irrelevantCol_0000110", "irrelevantCol_0000111", "irrelevantCol_0000112", "irrelevantCol_0000113", "irrelevantCol_0000114", "irrelevantCol_0000115", "irrelevantCol_0000116", "irrelevantCol_0000117", "irrelevantCol_0000118", "irrelevantCol_0000119", "irrelevantCol_0000120", "irrelevantCol_0000121", "irrelevantCol_0000122", "irrelevantCol_0000123", "irrelevantCol_0000124", "irrelevantCol_0000125", "irrelevantCol_0000126", "irrelevantCol_0000127", "irrelevantCol_0000128", "irrelevantCol_0000129", "irrelevantCol_0000130", "irrelevantCol_0000131", "irrelevantCol_0000132", "irrelevantCol_0000133", "irrelevantCol_0000134", "irrelevantCol_0000135", "irrelevantCol_0000136", "irrelevantCol_0000137", "irrelevantCol_0000138", "irrelevantCol_0000139", "irrelevantCol_0000140", "irrelevantCol_0000141", "irrelevantCol_0000142", "irrelevantCol_0000143", "irrelevantCol_0000144", "irrelevantCol_0000145", "irrelevantCol_0000146", "irrelevantCol_0000147", "irrelevantCol_0000148", "irrelevantCol_0000149", "irrelevantCol_0000150", "irrelevantCol_0000151", "irrelevantCol_0000152", "irrelevantCol_0000153", "irrelevantCol_0000154", "irrelevantCol_0000155", "irrelevantCol_0000156", "irrelevantCol_0000157", "irrelevantCol_0000158", "irrelevantCol_0000159", "irrelevantCol_0000160", "irrelevantCol_0000161", "irrelevantCol_0000162", "irrelevantCol_0000163", "irrelevantCol_0000164", "irrelevantCol_0000165", "irrelevantCol_0000166", "irrelevantCol_0000167", "irrelevantCol_0000168", "irrelevantCol_0000169", "irrelevantCol_0000170", "irrelevantCol_0000171", "irrelevantCol_0000172", "irrelevantCol_0000173", "irrelevantCol_0000174", "irrelevantCol_0000175", "irrelevantCol_0000176", "irrelevantCol_0000177", "irrelevantCol_0000178", "irrelevantCol_0000179", "irrelevantCol_0000180", "irrelevantCol_0000181", "irrelevantCol_0000182", "irrelevantCol_0000183", "irrelevantCol_0000184", "irrelevantCol_0000185", "irrelevantCol_0000186", "irrelevantCol_0000187", "irrelevantCol_0000188", "irrelevantCol_0000189", "irrelevantCol_0000190", "irrelevantCol_0000191", "irrelevantCol_0000192", "irrelevantCol_0000193", "irrelevantCol_0000194", "irrelevantCol_0000195", "irrelevantCol_0000196", "irrelevantCol_0000197", "irrelevantCol_0000198", "irrelevantCol_0000199", "irrelevantCol_0000200", "irrelevantCol_0000201", "irrelevantCol_0000202", "irrelevantCol_0000203", "irrelevantCol_0000204", "irrelevantCol_0000205", "irrelevantCol_0000206", "irrelevantCol_0000207", "irrelevantCol_0000208", "irrelevantCol_0000209", "irrelevantCol_0000210", "irrelevantCol_0000211", "irrelevantCol_0000212", "irrelevantCol_0000213", "irrelevantCol_0000214", "irrelevantCol_0000215", "irrelevantCol_0000216", "irrelevantCol_0000217", "irrelevantCol_0000218", "irrelevantCol_0000219", "irrelevantCol_0000220", "irrelevantCol_0000221", "irrelevantCol_0000222", "irrelevantCol_0000223", "irrelevantCol_0000224", "irrelevantCol_0000225", "irrelevantCol_0000226", "irrelevantCol_0000227", "irrelevantCol_0000228", "irrelevantCol_0000229", "irrelevantCol_0000230", "irrelevantCol_0000231", "irrelevantCol_0000232", "irrelevantCol_0000233", "irrelevantCol_0000234", "irrelevantCol_0000235", "irrelevantCol_0000236", "irrelevantCol_0000237", "irrelevantCol_0000238", "irrelevantCol_0000239", "irrelevantCol_0000240", "irrelevantCol_0000241", "irrelevantCol_0000242", "irrelevantCol_0000243", "irrelevantCol_0000244", "irrelevantCol_0000245", "irrelevantCol_0000246", "irrelevantCol_0000247", "irrelevantCol_0000248", "irrelevantCol_0000249", "irrelevantCol_0000250", "irrelevantCol_0000251", "irrelevantCol_0000252", "irrelevantCol_0000253", "irrelevantCol_0000254", "irrelevantCol_0000255", "irrelevantCol_0000256", "irrelevantCol_0000257", "irrelevantCol_0000258", "irrelevantCol_0000259", "irrelevantCol_0000260", "irrelevantCol_0000261", "irrelevantCol_0000262", "irrelevantCol_0000263", "irrelevantCol_0000264", "irrelevantCol_0000265", "irrelevantCol_0000266", "irrelevantCol_0000267", "irrelevantCol_0000268", "irrelevantCol_0000269", "irrelevantCol_0000270", "irrelevantCol_0000271", "irrelevantCol_0000272", "irrelevantCol_0000273", "irrelevantCol_0000274", "irrelevantCol_0000275", "irrelevantCol_0000276", "irrelevantCol_0000277", "irrelevantCol_0000278", "irrelevantCol_0000279", "irrelevantCol_0000280", "irrelevantCol_0000281", "irrelevantCol_0000282", "irrelevantCol_0000283", "irrelevantCol_0000284", "irrelevantCol_0000285", "irrelevantCol_0000286", "irrelevantCol_0000287", "irrelevantCol_0000288", "irrelevantCol_0000289", "irrelevantCol_0000290", "irrelevantCol_0000291", "irrelevantCol_0000292", "irrelevantCol_0000293", "irrelevantCol_0000294", "irrelevantCol_0000295", "irrelevantCol_0000296", "irrelevantCol_0000297", "irrelevantCol_0000298", "irrelevantCol_0000299", "irrelevantCol_0000300", "irrelevantCol_0000301", "irrelevantCol_0000302", "irrelevantCol_0000303", "irrelevantCol_0000304", "irrelevantCol_0000305", "irrelevantCol_0000306", "irrelevantCol_0000307", "irrelevantCol_0000308", "irrelevantCol_0000309", "irrelevantCol_0000310", "irrelevantCol_0000311", "irrelevantCol_0000312", "irrelevantCol_0000313", "irrelevantCol_0000314", "irrelevantCol_0000315", "irrelevantCol_0000316", "irrelevantCol_0000317", "irrelevantCol_0000318", "irrelevantCol_0000319", "irrelevantCol_0000320", "irrelevantCol_0000321", "irrelevantCol_0000322", "irrelevantCol_0000323", "irrelevantCol_0000324", "irrelevantCol_0000325", "irrelevantCol_0000326", "irrelevantCol_0000327", "irrelevantCol_0000328", "irrelevantCol_0000329", "irrelevantCol_0000330", "irrelevantCol_0000331", "irrelevantCol_0000332", "irrelevantCol_0000333", "irrelevantCol_0000334", "irrelevantCol_0000335", "irrelevantCol_0000336", "irrelevantCol_0000337", "irrelevantCol_0000338", "irrelevantCol_0000339", "irrelevantCol_0000340", "irrelevantCol_0000341", "irrelevantCol_0000342", "irrelevantCol_0000343", "irrelevantCol_0000344", "irrelevantCol_0000345", "irrelevantCol_0000346", "irrelevantCol_0000347", "irrelevantCol_0000348", "irrelevantCol_0000349", "irrelevantCol_0000350", "irrelevantCol_0000351", "irrelevantCol_0000352", "irrelevantCol_0000353", "irrelevantCol_0000354", "irrelevantCol_0000355", "irrelevantCol_0000356", "irrelevantCol_0000357", "irrelevantCol_0000358", "irrelevantCol_0000359", "irrelevantCol_0000360", "irrelevantCol_0000361", "irrelevantCol_0000362", "irrelevantCol_0000363", "irrelevantCol_0000364", "irrelevantCol_0000365", "irrelevantCol_0000366", "irrelevantCol_0000367", "irrelevantCol_0000368", "irrelevantCol_0000369", "irrelevantCol_0000370", "irrelevantCol_0000371", "irrelevantCol_0000372", "irrelevantCol_0000373", "irrelevantCol_0000374", "irrelevantCol_0000375", "irrelevantCol_0000376", "irrelevantCol_0000377", "irrelevantCol_0000378", "irrelevantCol_0000379", "irrelevantCol_0000380", "irrelevantCol_0000381", "irrelevantCol_0000382", "irrelevantCol_0000383", "irrelevantCol_0000384", "irrelevantCol_0000385", "irrelevantCol_0000386", "irrelevantCol_0000387", "irrelevantCol_0000388", "irrelevantCol_0000389", "irrelevantCol_0000390", "irrelevantCol_0000391", "irrelevantCol_0000392", "irrelevantCol_0000393", "irrelevantCol_0000394", "irrelevantCol_0000395", "irrelevantCol_0000396", "irrelevantCol_0000397", "irrelevantCol_0000398", "irrelevantCol_0000399", "irrelevantCol_0000400", "irrelevantCol_0000401", "irrelevantCol_0000402", "irrelevantCol_0000403", "irrelevantCol_0000404", "irrelevantCol_0000405", "irrelevantCol_0000406", "irrelevantCol_0000407", "irrelevantCol_0000408", "irrelevantCol_0000409", "irrelevantCol_0000410", "irrelevantCol_0000411", "irrelevantCol_0000412", "irrelevantCol_0000413", "irrelevantCol_0000414", "irrelevantCol_0000415", "irrelevantCol_0000416", "irrelevantCol_0000417", "irrelevantCol_0000418", "irrelevantCol_0000419", "irrelevantCol_0000420", "irrelevantCol_0000421", "irrelevantCol_0000422", "irrelevantCol_0000423", "irrelevantCol_0000424", "irrelevantCol_0000425", "irrelevantCol_0000426", "irrelevantCol_0000427", "irrelevantCol_0000428", "irrelevantCol_0000429", "irrelevantCol_0000430", "irrelevantCol_0000431", "irrelevantCol_0000432", "irrelevantCol_0000433", "irrelevantCol_0000434", "irrelevantCol_0000435", "irrelevantCol_0000436", "irrelevantCol_0000437", "irrelevantCol_0000438", "irrelevantCol_0000439", "irrelevantCol_0000440", "irrelevantCol_0000441", "irrelevantCol_0000442", "irrelevantCol_0000443", "irrelevantCol_0000444", "irrelevantCol_0000445", "irrelevantCol_0000446", "irrelevantCol_0000447", "irrelevantCol_0000448", "irrelevantCol_0000449", "irrelevantCol_0000450", "irrelevantCol_0000451", "irrelevantCol_0000452", "irrelevantCol_0000453", "irrelevantCol_0000454", "irrelevantCol_0000455", "irrelevantCol_0000456", "irrelevantCol_0000457", "irrelevantCol_0000458", "irrelevantCol_0000459", "irrelevantCol_0000460", "irrelevantCol_0000461", "irrelevantCol_0000462", "irrelevantCol_0000463", "irrelevantCol_0000464", "irrelevantCol_0000465", "irrelevantCol_0000466", "irrelevantCol_0000467", "irrelevantCol_0000468", "irrelevantCol_0000469", "irrelevantCol_0000470", "irrelevantCol_0000471", "irrelevantCol_0000472", "irrelevantCol_0000473", "irrelevantCol_0000474", "irrelevantCol_0000475", "irrelevantCol_0000476", "irrelevantCol_0000477", "irrelevantCol_0000478", "irrelevantCol_0000479", "irrelevantCol_0000480", "irrelevantCol_0000481", "irrelevantCol_0000482", "irrelevantCol_0000483", "irrelevantCol_0000484", "irrelevantCol_0000485", "irrelevantCol_0000486", "irrelevantCol_0000487", "irrelevantCol_0000488", "irrelevantCol_0000489", "irrelevantCol_0000490", "irrelevantCol_0000491", "irrelevantCol_0000492", "irrelevantCol_0000493", "irrelevantCol_0000494", "irrelevantCol_0000495", "irrelevantCol_0000496", "irrelevantCol_0000497", "irrelevantCol_0000498", "irrelevantCol_0000499", "irrelevantCol_0000500", EXP("assessmentTotal" * 0.237) / sum(EXP("assessmentTotal" * 0.237)) OVER (PARTITION BY "subjectID") AS "probability"
    ## FROM "rquery_mat_32917467332525125588_0000000000") "ievyicslic"
    ## ORDER BY "probability", "surveyCategory") "luwnvnkdfp") "drdsjgacof"
    ## WHERE ("zzz7" = "zzz8")) "uewjvuteew") "kpzopqqrqo"
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

    ## Timing stopped at: 0.089 0.002 0.106

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:03:08 2019"

However, the remote command analysis can happen after many expensive steps if there is also a `dplyr::compute()` node breaking up the steps. This can make debugging arbitrarily slow and expensive.

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:03:08 2019"

``` r
system.time({
  scale <- 0.237
  
  dplyr_pipeline_c <- d_large_tbl %>%
    group_by(subjectID) %>%
    mutate(probability =
             exp(assessmentTotal * scale)/
             sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
    arrange(probability, surveyCategory) %>%
    filter(row_number() == n()) %>%
    compute() %>%     # <- inopportune place to try to cache
    ungroup() %>%
    rename(diagnosis = surveyCategory) %>%
    select(subjectID, diagnosis, probability) %>%
    arrange(ZubjectIDZZZ) %>% # <- error non-existent column
    as.data.frame() %>%  # force calculation, and bring back result
    nrow()
})
```

    ## Error in postgresqlExecStatement(conn, statement, ...): RS-DBI driver: (could not Retrieve the result : ERROR:  column "ZubjectIDZZZ" does not exist
    ## LINE 4: ORDER BY "ZubjectIDZZZ"
    ##                  ^
    ## )

    ## Timing stopped at: 0.132 0.012 45.49

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:03:54 2019"

The above matters for large projects. It is as with all coding problems: it isn't enough to be one who would "never make such a mistake or be so careless", one would have to be part of a team where nobody would ever make such a mistake.

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:03:54 2019"

``` r
DBI::dbDisconnect(my_db)
```

    ## [1] TRUE

``` r
base::date()
```

    ## [1] "Sat Jan 12 13:03:54 2019"
