NarrowEffect
================
Win-Vector LLC
12/18/2017

<!-- NarrowEffect.md is generated from NarrowEffect.Rmd. Please edit that file -->
For some time we have been teaching [`R`](https://journal.r-project.org) users "when working with wide tables on Spark or on databases: narrow to the columns you really want to work with early in your analysis."

This issue arises because wide tables (200 to 1000 columns) are quite common in big-data analytics projects. Often these are "denormalized marts" that are used to drive many different projects. For any one project only a small subset of the columns may be relevant in a calculation.

The idea behind the advice is: working with fewer columns makes for quicker queries.

Let's set up our experiment. The data is a larger version of the problem from ["Let’s Have Some Sympathy For The Part-time R User"](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/). We have expanded the number of subjects to 500000 and added 100 irrelevant columns to the example. We define a new function that uses `dplyr` and `Sparklyr` to compute the diagnoses. We vary if the table is first limited to columns of interest and if the results are brought back to `R`.

``` r
scale <- 0.237
```

``` r
dT %>%
  group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
  arrange(probability, surveyCategory) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID) %>%
  tally() %>%
  dbplyr::remote_query(.) %>%
  cat
```

    ## SELECT count(*) AS `n`
    ## FROM (SELECT *
    ## FROM (SELECT `subjectID`, `diagnosis`, `probability`
    ## FROM (SELECT `subjectID`, `surveyCategory` AS `diagnosis`, `assessmentTotal`, `irrelevantCol_0000001`, `irrelevantCol_0000002`, `irrelevantCol_0000003`, `irrelevantCol_0000004`, `irrelevantCol_0000005`, `irrelevantCol_0000006`, `irrelevantCol_0000007`, `irrelevantCol_0000008`, `irrelevantCol_0000009`, `irrelevantCol_0000010`, `irrelevantCol_0000011`, `irrelevantCol_0000012`, `irrelevantCol_0000013`, `irrelevantCol_0000014`, `irrelevantCol_0000015`, `irrelevantCol_0000016`, `irrelevantCol_0000017`, `irrelevantCol_0000018`, `irrelevantCol_0000019`, `irrelevantCol_0000020`, `irrelevantCol_0000021`, `irrelevantCol_0000022`, `irrelevantCol_0000023`, `irrelevantCol_0000024`, `irrelevantCol_0000025`, `irrelevantCol_0000026`, `irrelevantCol_0000027`, `irrelevantCol_0000028`, `irrelevantCol_0000029`, `irrelevantCol_0000030`, `irrelevantCol_0000031`, `irrelevantCol_0000032`, `irrelevantCol_0000033`, `irrelevantCol_0000034`, `irrelevantCol_0000035`, `irrelevantCol_0000036`, `irrelevantCol_0000037`, `irrelevantCol_0000038`, `irrelevantCol_0000039`, `irrelevantCol_0000040`, `irrelevantCol_0000041`, `irrelevantCol_0000042`, `irrelevantCol_0000043`, `irrelevantCol_0000044`, `irrelevantCol_0000045`, `irrelevantCol_0000046`, `irrelevantCol_0000047`, `irrelevantCol_0000048`, `irrelevantCol_0000049`, `irrelevantCol_0000050`, `irrelevantCol_0000051`, `irrelevantCol_0000052`, `irrelevantCol_0000053`, `irrelevantCol_0000054`, `irrelevantCol_0000055`, `irrelevantCol_0000056`, `irrelevantCol_0000057`, `irrelevantCol_0000058`, `irrelevantCol_0000059`, `irrelevantCol_0000060`, `irrelevantCol_0000061`, `irrelevantCol_0000062`, `irrelevantCol_0000063`, `irrelevantCol_0000064`, `irrelevantCol_0000065`, `irrelevantCol_0000066`, `irrelevantCol_0000067`, `irrelevantCol_0000068`, `irrelevantCol_0000069`, `irrelevantCol_0000070`, `irrelevantCol_0000071`, `irrelevantCol_0000072`, `irrelevantCol_0000073`, `irrelevantCol_0000074`, `irrelevantCol_0000075`, `irrelevantCol_0000076`, `irrelevantCol_0000077`, `irrelevantCol_0000078`, `irrelevantCol_0000079`, `irrelevantCol_0000080`, `irrelevantCol_0000081`, `irrelevantCol_0000082`, `irrelevantCol_0000083`, `irrelevantCol_0000084`, `irrelevantCol_0000085`, `irrelevantCol_0000086`, `irrelevantCol_0000087`, `irrelevantCol_0000088`, `irrelevantCol_0000089`, `irrelevantCol_0000090`, `irrelevantCol_0000091`, `irrelevantCol_0000092`, `irrelevantCol_0000093`, `irrelevantCol_0000094`, `irrelevantCol_0000095`, `irrelevantCol_0000096`, `irrelevantCol_0000097`, `irrelevantCol_0000098`, `irrelevantCol_0000099`, `irrelevantCol_0000100`, `probability`
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol_0000001`, `irrelevantCol_0000002`, `irrelevantCol_0000003`, `irrelevantCol_0000004`, `irrelevantCol_0000005`, `irrelevantCol_0000006`, `irrelevantCol_0000007`, `irrelevantCol_0000008`, `irrelevantCol_0000009`, `irrelevantCol_0000010`, `irrelevantCol_0000011`, `irrelevantCol_0000012`, `irrelevantCol_0000013`, `irrelevantCol_0000014`, `irrelevantCol_0000015`, `irrelevantCol_0000016`, `irrelevantCol_0000017`, `irrelevantCol_0000018`, `irrelevantCol_0000019`, `irrelevantCol_0000020`, `irrelevantCol_0000021`, `irrelevantCol_0000022`, `irrelevantCol_0000023`, `irrelevantCol_0000024`, `irrelevantCol_0000025`, `irrelevantCol_0000026`, `irrelevantCol_0000027`, `irrelevantCol_0000028`, `irrelevantCol_0000029`, `irrelevantCol_0000030`, `irrelevantCol_0000031`, `irrelevantCol_0000032`, `irrelevantCol_0000033`, `irrelevantCol_0000034`, `irrelevantCol_0000035`, `irrelevantCol_0000036`, `irrelevantCol_0000037`, `irrelevantCol_0000038`, `irrelevantCol_0000039`, `irrelevantCol_0000040`, `irrelevantCol_0000041`, `irrelevantCol_0000042`, `irrelevantCol_0000043`, `irrelevantCol_0000044`, `irrelevantCol_0000045`, `irrelevantCol_0000046`, `irrelevantCol_0000047`, `irrelevantCol_0000048`, `irrelevantCol_0000049`, `irrelevantCol_0000050`, `irrelevantCol_0000051`, `irrelevantCol_0000052`, `irrelevantCol_0000053`, `irrelevantCol_0000054`, `irrelevantCol_0000055`, `irrelevantCol_0000056`, `irrelevantCol_0000057`, `irrelevantCol_0000058`, `irrelevantCol_0000059`, `irrelevantCol_0000060`, `irrelevantCol_0000061`, `irrelevantCol_0000062`, `irrelevantCol_0000063`, `irrelevantCol_0000064`, `irrelevantCol_0000065`, `irrelevantCol_0000066`, `irrelevantCol_0000067`, `irrelevantCol_0000068`, `irrelevantCol_0000069`, `irrelevantCol_0000070`, `irrelevantCol_0000071`, `irrelevantCol_0000072`, `irrelevantCol_0000073`, `irrelevantCol_0000074`, `irrelevantCol_0000075`, `irrelevantCol_0000076`, `irrelevantCol_0000077`, `irrelevantCol_0000078`, `irrelevantCol_0000079`, `irrelevantCol_0000080`, `irrelevantCol_0000081`, `irrelevantCol_0000082`, `irrelevantCol_0000083`, `irrelevantCol_0000084`, `irrelevantCol_0000085`, `irrelevantCol_0000086`, `irrelevantCol_0000087`, `irrelevantCol_0000088`, `irrelevantCol_0000089`, `irrelevantCol_0000090`, `irrelevantCol_0000091`, `irrelevantCol_0000092`, `irrelevantCol_0000093`, `irrelevantCol_0000094`, `irrelevantCol_0000095`, `irrelevantCol_0000096`, `irrelevantCol_0000097`, `irrelevantCol_0000098`, `irrelevantCol_0000099`, `irrelevantCol_0000100`, `probability`
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol_0000001`, `irrelevantCol_0000002`, `irrelevantCol_0000003`, `irrelevantCol_0000004`, `irrelevantCol_0000005`, `irrelevantCol_0000006`, `irrelevantCol_0000007`, `irrelevantCol_0000008`, `irrelevantCol_0000009`, `irrelevantCol_0000010`, `irrelevantCol_0000011`, `irrelevantCol_0000012`, `irrelevantCol_0000013`, `irrelevantCol_0000014`, `irrelevantCol_0000015`, `irrelevantCol_0000016`, `irrelevantCol_0000017`, `irrelevantCol_0000018`, `irrelevantCol_0000019`, `irrelevantCol_0000020`, `irrelevantCol_0000021`, `irrelevantCol_0000022`, `irrelevantCol_0000023`, `irrelevantCol_0000024`, `irrelevantCol_0000025`, `irrelevantCol_0000026`, `irrelevantCol_0000027`, `irrelevantCol_0000028`, `irrelevantCol_0000029`, `irrelevantCol_0000030`, `irrelevantCol_0000031`, `irrelevantCol_0000032`, `irrelevantCol_0000033`, `irrelevantCol_0000034`, `irrelevantCol_0000035`, `irrelevantCol_0000036`, `irrelevantCol_0000037`, `irrelevantCol_0000038`, `irrelevantCol_0000039`, `irrelevantCol_0000040`, `irrelevantCol_0000041`, `irrelevantCol_0000042`, `irrelevantCol_0000043`, `irrelevantCol_0000044`, `irrelevantCol_0000045`, `irrelevantCol_0000046`, `irrelevantCol_0000047`, `irrelevantCol_0000048`, `irrelevantCol_0000049`, `irrelevantCol_0000050`, `irrelevantCol_0000051`, `irrelevantCol_0000052`, `irrelevantCol_0000053`, `irrelevantCol_0000054`, `irrelevantCol_0000055`, `irrelevantCol_0000056`, `irrelevantCol_0000057`, `irrelevantCol_0000058`, `irrelevantCol_0000059`, `irrelevantCol_0000060`, `irrelevantCol_0000061`, `irrelevantCol_0000062`, `irrelevantCol_0000063`, `irrelevantCol_0000064`, `irrelevantCol_0000065`, `irrelevantCol_0000066`, `irrelevantCol_0000067`, `irrelevantCol_0000068`, `irrelevantCol_0000069`, `irrelevantCol_0000070`, `irrelevantCol_0000071`, `irrelevantCol_0000072`, `irrelevantCol_0000073`, `irrelevantCol_0000074`, `irrelevantCol_0000075`, `irrelevantCol_0000076`, `irrelevantCol_0000077`, `irrelevantCol_0000078`, `irrelevantCol_0000079`, `irrelevantCol_0000080`, `irrelevantCol_0000081`, `irrelevantCol_0000082`, `irrelevantCol_0000083`, `irrelevantCol_0000084`, `irrelevantCol_0000085`, `irrelevantCol_0000086`, `irrelevantCol_0000087`, `irrelevantCol_0000088`, `irrelevantCol_0000089`, `irrelevantCol_0000090`, `irrelevantCol_0000091`, `irrelevantCol_0000092`, `irrelevantCol_0000093`, `irrelevantCol_0000094`, `irrelevantCol_0000095`, `irrelevantCol_0000096`, `irrelevantCol_0000097`, `irrelevantCol_0000098`, `irrelevantCol_0000099`, `irrelevantCol_0000100`, `probability`, row_number() OVER (PARTITION BY `subjectID` ORDER BY `probability`, `surveyCategory`) AS `zzz4`, COUNT(*) OVER (PARTITION BY `subjectID`) AS `zzz5`
    ## FROM (SELECT *
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol_0000001`, `irrelevantCol_0000002`, `irrelevantCol_0000003`, `irrelevantCol_0000004`, `irrelevantCol_0000005`, `irrelevantCol_0000006`, `irrelevantCol_0000007`, `irrelevantCol_0000008`, `irrelevantCol_0000009`, `irrelevantCol_0000010`, `irrelevantCol_0000011`, `irrelevantCol_0000012`, `irrelevantCol_0000013`, `irrelevantCol_0000014`, `irrelevantCol_0000015`, `irrelevantCol_0000016`, `irrelevantCol_0000017`, `irrelevantCol_0000018`, `irrelevantCol_0000019`, `irrelevantCol_0000020`, `irrelevantCol_0000021`, `irrelevantCol_0000022`, `irrelevantCol_0000023`, `irrelevantCol_0000024`, `irrelevantCol_0000025`, `irrelevantCol_0000026`, `irrelevantCol_0000027`, `irrelevantCol_0000028`, `irrelevantCol_0000029`, `irrelevantCol_0000030`, `irrelevantCol_0000031`, `irrelevantCol_0000032`, `irrelevantCol_0000033`, `irrelevantCol_0000034`, `irrelevantCol_0000035`, `irrelevantCol_0000036`, `irrelevantCol_0000037`, `irrelevantCol_0000038`, `irrelevantCol_0000039`, `irrelevantCol_0000040`, `irrelevantCol_0000041`, `irrelevantCol_0000042`, `irrelevantCol_0000043`, `irrelevantCol_0000044`, `irrelevantCol_0000045`, `irrelevantCol_0000046`, `irrelevantCol_0000047`, `irrelevantCol_0000048`, `irrelevantCol_0000049`, `irrelevantCol_0000050`, `irrelevantCol_0000051`, `irrelevantCol_0000052`, `irrelevantCol_0000053`, `irrelevantCol_0000054`, `irrelevantCol_0000055`, `irrelevantCol_0000056`, `irrelevantCol_0000057`, `irrelevantCol_0000058`, `irrelevantCol_0000059`, `irrelevantCol_0000060`, `irrelevantCol_0000061`, `irrelevantCol_0000062`, `irrelevantCol_0000063`, `irrelevantCol_0000064`, `irrelevantCol_0000065`, `irrelevantCol_0000066`, `irrelevantCol_0000067`, `irrelevantCol_0000068`, `irrelevantCol_0000069`, `irrelevantCol_0000070`, `irrelevantCol_0000071`, `irrelevantCol_0000072`, `irrelevantCol_0000073`, `irrelevantCol_0000074`, `irrelevantCol_0000075`, `irrelevantCol_0000076`, `irrelevantCol_0000077`, `irrelevantCol_0000078`, `irrelevantCol_0000079`, `irrelevantCol_0000080`, `irrelevantCol_0000081`, `irrelevantCol_0000082`, `irrelevantCol_0000083`, `irrelevantCol_0000084`, `irrelevantCol_0000085`, `irrelevantCol_0000086`, `irrelevantCol_0000087`, `irrelevantCol_0000088`, `irrelevantCol_0000089`, `irrelevantCol_0000090`, `irrelevantCol_0000091`, `irrelevantCol_0000092`, `irrelevantCol_0000093`, `irrelevantCol_0000094`, `irrelevantCol_0000095`, `irrelevantCol_0000096`, `irrelevantCol_0000097`, `irrelevantCol_0000098`, `irrelevantCol_0000099`, `irrelevantCol_0000100`, EXP(`assessmentTotal` * 0.237) / sum(EXP(`assessmentTotal` * 0.237)) OVER (PARTITION BY `subjectID`) AS `probability`
    ## FROM `dparq`) `ezoytataey`
    ## ORDER BY `probability`, `surveyCategory`) `blprncmeyf`) `gdczvxolag`
    ## WHERE (`zzz4` = `zzz5`)) `hrxwetkmrw`) `leetwerned`) `xlalvgpatl`
    ## ORDER BY `subjectID`) `irknbybkkk`

``` r
dplyr_run <- function(narrow) {
  dR <- dT
  if(narrow) {
    dR <- dR %>%
      select(subjectID, surveyCategory, assessmentTotal)
  }
  dR <- dR %>%
    group_by(subjectID) %>%
    mutate(probability =
             exp(assessmentTotal * scale)/
             sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
    arrange(probability, surveyCategory) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    rename(diagnosis = surveyCategory) %>%
    select(subjectID, diagnosis, probability) %>%
    arrange(subjectID) %>%
    tally()
  res <- dR %>% collect() %>% as.data.frame()
  as.numeric(res[[1]][[1]])
}


dplyr_run(narrow=FALSE)
```

    ## [1] 5e+05

``` r
dplyr_run(narrow=TRUE)
```

    ## [1] 5e+05

``` r
optree <- dR %.>%
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
  orderby(., 'subjectID') %.>%
  sql_node(., "n" := "COUNT(1)", orig_columns = FALSE)

cat(to_sql(optree, my_db))
```

    ## SELECT
    ##  COUNT(1) AS `n`
    ## FROM (
    ##  SELECT * FROM (
    ##   SELECT
    ##    `subjectID`,
    ##    `diagnosis`,
    ##    `probability`
    ##   FROM (
    ##    SELECT * FROM (
    ##     SELECT
    ##      `count` AS `count`,
    ##      `probability` AS `probability`,
    ##      `rank` AS `rank`,
    ##      `subjectID` AS `subjectID`,
    ##      `surveyCategory` AS `diagnosis`
    ##     FROM (
    ##      SELECT
    ##       `count`,
    ##       `probability`,
    ##       `subjectID`,
    ##       `surveyCategory`,
    ##       rank ( ) OVER (  PARTITION BY `subjectID` ORDER BY `probability`, `surveyCategory` ) AS `rank`
    ##      FROM (
    ##       SELECT
    ##        `subjectID`,
    ##        `surveyCategory`,
    ##        `assessmentTotal`,
    ##        exp ( `assessmentTotal` * 0.237 ) / sum ( exp ( `assessmentTotal` * 0.237 ) ) OVER (  PARTITION BY `subjectID` ) AS `probability`,
    ##        count ( 1 ) OVER (  PARTITION BY `subjectID` ) AS `count`
    ##       FROM (
    ##        SELECT
    ##         `dparq`.`subjectID`,
    ##         `dparq`.`surveyCategory`,
    ##         `dparq`.`assessmentTotal`
    ##        FROM
    ##         `dparq`
    ##        ) tsql_80769245101744464578_0000000000
    ##       ) tsql_80769245101744464578_0000000001
    ##     ) tsql_80769245101744464578_0000000002
    ##    ) tsql_80769245101744464578_0000000003
    ##    WHERE `rank` = `count`
    ##   ) tsql_80769245101744464578_0000000004
    ##  ) tsql_80769245101744464578_0000000005 ORDER BY `subjectID`
    ## ) tsql_80769245101744464578_0000000006

``` r
rquery_run <- function() {
  optree <- dR %.>%
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
    orderby(., 'subjectID') %.>%
    sql_node(., "n" := "COUNT(1)", orig_columns = FALSE)
  res <- execute(my_db, optree)
  as.numeric(res[[1]][[1]])
}

rquery_run()
```

    ## [1] 5e+05

We can get timings for variations of the function:

``` r
library("microbenchmark")

timings <- microbenchmark(dplyr_run(narrow=FALSE), 
                          dplyr_run(narrow=TRUE),
                          rquery_run(),
                          times = 20)
```

And then present the results:

``` r
print(timings)
```

    ## Unit: seconds
    ##                       expr      min       lq     mean   median       uq
    ##  dplyr_run(narrow = FALSE) 2.712947 2.762358 2.887901 2.879607 2.965803
    ##   dplyr_run(narrow = TRUE) 2.632515 2.707642 2.913471 2.871682 3.065905
    ##               rquery_run() 2.196372 2.274019 2.457174 2.401239 2.588241
    ##       max neval
    ##  3.194005    20
    ##  3.423903    20
    ##  2.898329    20

``` r
tdf <- as.data.frame(timings)

# order the data
tdf <- tdf %>%
  group_by(., expr) %>%
  mutate(., mtime = median(time)) %>%
  ungroup(.)

tdf$expr <- reorder(tdf$expr, tdf$mtime)
WVPlots::ScatterBoxPlotH(tdf, "time", "expr",  
                         pt_alpha=0.2,
                         title="Execution times in NS")
```

![](NarrowEffect_files/figure-markdown_github/present-1.png)

The effect seems to be present, but weaker than last time I measured it. Though the effect would certainly be there if one inspected an intermediate table.

Of course, narrowing to the exact columns used can be difficult: it can involve inspecting an arbitrarily long pipeline for column uses. That is part of why we are developing a new `R` query generator that automates that procedure: [`rquery`](https://winvector.github.io/rquery/).
