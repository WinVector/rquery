dplyrSQL
================
Win-Vector LLC
12/11/2017

`dplyr` SQL for the [`rquery` example](https://winvector.github.io/rquery/). Notice the irrelevant columns live a few steps into the query sequence. Also notice the `dplyr` `SQL` does have less nesting than the `rquery` `SQL`.

``` r
suppressPackageStartupMessages(library("dplyr"))
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

``` r
packageVersion("dplyr")
```

    ## [1] '0.7.6'

``` r
my_db <- sparklyr::spark_connect(version='2.2.0', 
                                 master = "local")

d_local <- wrapr::build_frame(
   'subjectID', 'surveyCategory'     , 'assessmentTotal', 'irrelevantCol1', 'irrelevantCol2' |
   1          , 'withdrawal behavior', 5                , 'irrel1'        , 'irrel2'         |
   1          , 'positive re-framing', 2                , 'irrel1'        , 'irrel2'         |
   2          , 'withdrawal behavior', 3                , 'irrel1'        , 'irrel2'         |
   2          , 'positive re-framing', 4                , 'irrel1'        , 'irrel2'         )

d <- dplyr::copy_to(my_db,
                    d_local,
                    name =  'd',
                    temporary = TRUE,
                    overwrite = FALSE)



scale <- 0.237

dplyr_pipeline <- d %>%
  group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale), na.rm=TRUE)) %>%
  arrange(probability, surveyCategory) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID)

# directly prints, can not easilly and reliable capture SQL
show_query(dplyr_pipeline)
```

    ## <SQL>
    ## SELECT `subjectID`, `diagnosis`, `probability`
    ## FROM (SELECT `subjectID`, `surveyCategory` AS `diagnosis`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, `probability`
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, `probability`
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, `probability`, row_number() OVER (PARTITION BY `subjectID` ORDER BY `probability`, `surveyCategory`) AS `zzz2`, COUNT(*) OVER (PARTITION BY `subjectID`) AS `zzz3`
    ## FROM (SELECT *
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, EXP(`assessmentTotal` * 0.237) / sum(EXP(`assessmentTotal` * 0.237)) OVER (PARTITION BY `subjectID`) AS `probability`
    ## FROM `d`) `yjryiymakx`
    ## ORDER BY `probability`, `surveyCategory`) `ktopcuqwie`) `efcaihutre`
    ## WHERE (`zzz2` = `zzz3`)) `ykxoyqwoon`) `xkcntyvcwj`
    ## ORDER BY `subjectID`

``` r
# directly prints, can not easilly and reliable capture SQL
explain(dplyr_pipeline)
```

    ## <SQL>
    ## SELECT `subjectID`, `diagnosis`, `probability`
    ## FROM (SELECT `subjectID`, `surveyCategory` AS `diagnosis`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, `probability`
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, `probability`
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, `probability`, row_number() OVER (PARTITION BY `subjectID` ORDER BY `probability`, `surveyCategory`) AS `zzz4`, COUNT(*) OVER (PARTITION BY `subjectID`) AS `zzz5`
    ## FROM (SELECT *
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, EXP(`assessmentTotal` * 0.237) / sum(EXP(`assessmentTotal` * 0.237)) OVER (PARTITION BY `subjectID`) AS `probability`
    ## FROM `d`) `pghuhkzufg`
    ## ORDER BY `probability`, `surveyCategory`) `utjrlsrkrs`) `hghlzewqvr`
    ## WHERE (`zzz4` = `zzz5`)) `ebewmmjmnf`) `lqrqugaelb`
    ## ORDER BY `subjectID`

    ## 

    ## <PLAN>

``` r
# the the plan by hand
dplyr_plan <- DBI::dbGetQuery(my_db, paste("EXPLAIN ", 
                                           dbplyr::remote_query(dplyr_pipeline))) 
cat(dplyr_plan[1, 1])
```

    ## == Physical Plan ==
    ## *Sort [subjectID#15 ASC NULLS FIRST], true, 0
    ## +- Exchange rangepartitioning(subjectID#15 ASC NULLS FIRST, 4)
    ##    +- *Project [subjectID#15, surveyCategory#16 AS diagnosis#142, probability#139]
    ##       +- *Filter (isnotnull(zzz6#140) && (cast(zzz6#140 as bigint) = zzz7#141L))
    ##          +- Window [count(1) windowspecdefinition(subjectID#15, ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS zzz7#141L], [subjectID#15]
    ##             +- Window [row_number() windowspecdefinition(subjectID#15, probability#139 ASC NULLS FIRST, surveyCategory#16 ASC NULLS FIRST, ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS zzz6#140], [subjectID#15], [probability#139 ASC NULLS FIRST, surveyCategory#16 ASC NULLS FIRST]
    ##                +- *Sort [subjectID#15 ASC NULLS FIRST, probability#139 ASC NULLS FIRST, surveyCategory#16 ASC NULLS FIRST], false, 0
    ##                   +- Exchange hashpartitioning(subjectID#15, 4)
    ##                      +- *Sort [probability#139 ASC NULLS FIRST, surveyCategory#16 ASC NULLS FIRST], true, 0
    ##                         +- Exchange rangepartitioning(probability#139 ASC NULLS FIRST, surveyCategory#16 ASC NULLS FIRST, 4)
    ##                            +- *Project [subjectID#15, surveyCategory#16, (EXP((assessmentTotal#17 * 0.237)) / _we0#149) AS probability#139]
    ##                               +- Window [sum(_w0#148) windowspecdefinition(subjectID#15, ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS _we0#149], [subjectID#15]
    ##                                  +- *Sort [subjectID#15 ASC NULLS FIRST], false, 0
    ##                                     +- Exchange hashpartitioning(subjectID#15, 4)
    ##                                        +- *Project [subjectID#15, surveyCategory#16, assessmentTotal#17, EXP((assessmentTotal#17 * 0.237)) AS _w0#148]
    ##                                           +- InMemoryTableScan [subjectID#15, surveyCategory#16, assessmentTotal#17]
    ##                                                 +- InMemoryRelation [subjectID#15, surveyCategory#16, assessmentTotal#17, irrelevantCol1#18, irrelevantCol2#19], true, 10000, StorageLevel(disk, memory, deserialized, 1 replicas), `d`
    ##                                                       +- Scan ExistingRDD[subjectID#15,surveyCategory#16,assessmentTotal#17,irrelevantCol1#18,irrelevantCol2#19]

``` r
# run
dplyr_pipeline
```

    ## # Source:     lazy query [?? x 3]
    ## # Database:   spark_connection
    ## # Ordered by: probability, surveyCategory, subjectID
    ##   subjectID diagnosis           probability
    ##       <dbl> <chr>                     <dbl>
    ## 1         1 withdrawal behavior       0.671
    ## 2         2 positive re-framing       0.559

``` r
library("rquery")

scale <- 0.237

rquery_pipeline <- db_td(my_db, "d") %.>%
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

rquery_plan <- DBI::dbGetQuery(my_db, paste("EXPLAIN ",
                                            to_sql(rquery_pipeline, my_db)))
cat(rquery_plan[1, 1])
```

    ## == Physical Plan ==
    ## *Sort [subjectID#15 ASC NULLS FIRST], true, 0
    ## +- Exchange rangepartitioning(subjectID#15 ASC NULLS FIRST, 4)
    ##    +- *Project [subjectID#15, surveyCategory#16 AS diagnosis#275, probability#271]
    ##       +- *Filter (isnotnull(row_number#272) && (row_number#272 <= 1))
    ##          +- Window [row_number() windowspecdefinition(subjectID#15, probability#271 DESC NULLS LAST, surveyCategory#16 ASC NULLS FIRST, ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS row_number#272], [subjectID#15], [probability#271 DESC NULLS LAST, surveyCategory#16 ASC NULLS FIRST]
    ##             +- *Sort [subjectID#15 ASC NULLS FIRST, probability#271 DESC NULLS LAST, surveyCategory#16 ASC NULLS FIRST], false, 0
    ##                +- *Project [(probability#270 / _we0#280) AS probability#271, subjectID#15, surveyCategory#16]
    ##                   +- Window [sum(probability#270) windowspecdefinition(subjectID#15, ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS _we0#280], [subjectID#15]
    ##                      +- *Sort [subjectID#15 ASC NULLS FIRST], false, 0
    ##                         +- Exchange hashpartitioning(subjectID#15, 4)
    ##                            +- *Project [subjectID#15, surveyCategory#16, EXP((assessmentTotal#17 * 0.237)) AS probability#270]
    ##                               +- InMemoryTableScan [subjectID#15, surveyCategory#16, assessmentTotal#17]
    ##                                     +- InMemoryRelation [subjectID#15, surveyCategory#16, assessmentTotal#17, irrelevantCol1#18, irrelevantCol2#19], true, 10000, StorageLevel(disk, memory, deserialized, 1 replicas), `d`
    ##                                           +- Scan ExistingRDD[subjectID#15,surveyCategory#16,assessmentTotal#17,irrelevantCol1#18,irrelevantCol2#19]

``` r
sparklyr::spark_disconnect(my_db)
```
