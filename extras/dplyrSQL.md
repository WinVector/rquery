dplyrSQL
================
Win-Vector LLC
12/11/2017

`dplyr` SQL for the [`rquery` example](https://winvector.github.io/rquery/). Notice the irrelevant columns live a few steps into the query sequence. Also notice the `dplyr` `SQL` does have less nesting than the `rquery` `SQL`.

``` r
suppressPackageStartupMessages(library("dplyr"))
packageVersion("dplyr")
```

    ## [1] '0.7.4'

``` r
my_db <- sparklyr::spark_connect(version='2.2.0', 
                                 master = "local")

d <- dplyr::copy_to(my_db,
                    data.frame(
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
                      irrelevantCol1 = "irrel1",
                      irrelevantCol2 = "irrel2",
                      stringsAsFactors = FALSE),
                    name =  'd',
                    temporary = TRUE,
                    overwrite = FALSE)

scale <- 0.237

d %>%
  group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale))) %>%
  arrange(probability, surveyCategory) %>%
  mutate(isDiagnosis = row_number() == n()) %>%
  filter(isDiagnosis) %>%
  ungroup() %>%
  select(subjectID, surveyCategory, probability) %>%
  rename(diagnosis = surveyCategory) %>%
  arrange(subjectID) %>%
  show_query()
```

    ## <SQL>
    ## SELECT `subjectID` AS `subjectID`, `surveyCategory` AS `diagnosis`, `probability` AS `probability`
    ## FROM (SELECT `subjectID` AS `subjectID`, `surveyCategory` AS `surveyCategory`, `probability` AS `probability`
    ## FROM (SELECT *
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, `probability`, row_number() OVER (PARTITION BY `subjectID` ORDER BY `probability`, `surveyCategory`) = COUNT(*) OVER (PARTITION BY `subjectID`) AS `isDiagnosis`
    ## FROM (SELECT *
    ## FROM (SELECT `subjectID`, `surveyCategory`, `assessmentTotal`, `irrelevantCol1`, `irrelevantCol2`, EXP(`assessmentTotal` * 0.237) / sum(EXP(`assessmentTotal` * 0.237)) OVER (PARTITION BY `subjectID`) AS `probability`
    ## FROM `d`) `ncflniwzya`
    ## ORDER BY `probability`, `surveyCategory`) `mhrqixblyt`) `huvezogdam`
    ## WHERE (`isDiagnosis`)) `ezmnqgukzq`) `xjuzsjzoqo`
    ## ORDER BY `subjectID`

``` r
sparklyr::spark_disconnect(my_db)
```
