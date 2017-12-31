DifferentDBs
================

`rquery` operator trees do not hold a reference to a database. This means the same tree can be serialized/de-serialized or saved/loaded and also used with multiple databases.

``` r
library("rquery")
```

    ## Loading required package: wrapr

    ## Loading required package: cdata

``` r
d <- rquery::table_source(
  "d",
  c("subjectID", "surveyCategory", "assessmentTotal"))

scale <- 0.237

dq <- d %.>%
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
```

Printing the pipeline.

``` r
cat(format(dq))
```

    table('d') %.>%
     extend(.,
      probability := exp(assessmentTotal * scale) / sum(exp(assessmentTotal * scale)),
      count := count(1),
      p= subjectID) %.>%
     extend(.,
      rank := rank(),
      p= subjectID,
      o= probability, surveyCategory) %.>%
     rename(.,
      c('diagnosis' := 'surveyCategory')) %.>%
     select_rows(., rank = count) %.>%
     select_columns(., subjectID, diagnosis, probability) %.>%
     orderby(., subjectID)

Spark example.

``` r
spark <- sparklyr::spark_connect(version='2.2.0', 
                                   master = "local")
```

    Warning in yaml.load(readLines(con), error.label = error.label, ...): R
    expressions in yaml.load will not be auto-evaluated by default in the near
    future

    Warning in yaml.load(readLines(con), error.label = error.label, ...): R
    expressions in yaml.load will not be auto-evaluated by default in the near
    future

    Warning in yaml.load(readLines(con), error.label = error.label, ...): R
    expressions in yaml.load will not be auto-evaluated by default in the near
    future

``` r
class(spark)
```

    [1] "spark_connection"       "spark_shell_connection"
    [3] "DBIConnection"         

``` r
cat(to_sql(dq, 
           db = spark, 
           source_limit = 1000))
```

    SELECT * FROM (
     SELECT
      `subjectID`,
      `diagnosis`,
      `probability`
     FROM (
      SELECT * FROM (
       SELECT
        `count` AS `count`,
        `probability` AS `probability`,
        `rank` AS `rank`,
        `subjectID` AS `subjectID`,
        `surveyCategory` AS `diagnosis`
       FROM (
        SELECT
         `count`,
         `probability`,
         `subjectID`,
         `surveyCategory`,
         rank ( ) OVER (  PARTITION BY `subjectID` ORDER BY `probability`, `surveyCategory` ) AS `rank`
        FROM (
         SELECT
          `subjectID`,
          `surveyCategory`,
          `assessmentTotal`,
          exp ( `assessmentTotal` * 0.237 ) / sum ( exp ( `assessmentTotal` * 0.237 ) ) OVER (  PARTITION BY `subjectID` ) AS `probability`,
          count ( 1 ) OVER (  PARTITION BY `subjectID` ) AS `count`
         FROM (
          SELECT
           `d`.`subjectID`,
           `d`.`surveyCategory`,
           `d`.`assessmentTotal`
          FROM
           `d` LIMIT 1000
          ) tsql_0000
         ) tsql_0001
       ) tsql_0002
      ) tsql_0003
      WHERE `rank` = `count`
     ) tsql_0004
    ) tsql_0005 ORDER BY `subjectID`

``` r
sparklyr::spark_disconnect(spark)
```

PostgreSQL example.

``` r
rpostgres <- DBI::dbConnect(RPostgres::Postgres(),
                            host = 'localhost',
                            port = 5432,
                            user = 'postgres',
                            password = 'pg')
class(rpostgres)
```

    [1] "PqConnection"
    attr(,"package")
    [1] "RPostgres"

``` r
cat(to_sql(dq, 
           db = rpostgres, 
           source_limit = 1000))
```

    SELECT * FROM (
     SELECT
      "subjectID",
      "diagnosis",
      "probability"
     FROM (
      SELECT * FROM (
       SELECT
        "count" AS "count",
        "probability" AS "probability",
        "rank" AS "rank",
        "subjectID" AS "subjectID",
        "surveyCategory" AS "diagnosis"
       FROM (
        SELECT
         "count",
         "probability",
         "subjectID",
         "surveyCategory",
         rank ( ) OVER (  PARTITION BY "subjectID" ORDER BY "probability", "surveyCategory" ) AS "rank"
        FROM (
         SELECT
          "subjectID",
          "surveyCategory",
          "assessmentTotal",
          exp ( "assessmentTotal" * 0.237 ) / sum ( exp ( "assessmentTotal" * 0.237 ) ) OVER (  PARTITION BY "subjectID" ) AS "probability",
          count ( 1 ) OVER (  PARTITION BY "subjectID" ) AS "count"
         FROM (
          SELECT
           "d"."subjectID",
           "d"."surveyCategory",
           "d"."assessmentTotal"
          FROM
           "d" LIMIT 1000
          ) tsql_0000
         ) tsql_0001
       ) tsql_0002
      ) tsql_0003
      WHERE "rank" = "count"
     ) tsql_0004
    ) tsql_0005 ORDER BY "subjectID"

``` r
DBI::dbDisconnect(rpostgres)
```

SQLite example.

``` r
rsqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

class(rsqlite)
```

    [1] "SQLiteConnection"
    attr(,"package")
    [1] "RSQLite"

``` r
cat(to_sql(dq, 
           db = rsqlite, 
           source_limit = 1000))
```

    SELECT * FROM (
     SELECT
      `subjectID`,
      `diagnosis`,
      `probability`
     FROM (
      SELECT * FROM (
       SELECT
        `count` AS `count`,
        `probability` AS `probability`,
        `rank` AS `rank`,
        `subjectID` AS `subjectID`,
        `surveyCategory` AS `diagnosis`
       FROM (
        SELECT
         `count`,
         `probability`,
         `subjectID`,
         `surveyCategory`,
         rank ( ) OVER (  PARTITION BY `subjectID` ORDER BY `probability`, `surveyCategory` ) AS `rank`
        FROM (
         SELECT
          `subjectID`,
          `surveyCategory`,
          `assessmentTotal`,
          exp ( `assessmentTotal` * 0.237 ) / sum ( exp ( `assessmentTotal` * 0.237 ) ) OVER (  PARTITION BY `subjectID` ) AS `probability`,
          count ( 1 ) OVER (  PARTITION BY `subjectID` ) AS `count`
         FROM (
          SELECT
           `d`.`subjectID`,
           `d`.`surveyCategory`,
           `d`.`assessmentTotal`
          FROM
           `d` LIMIT 1000
          ) tsql_0000
         ) tsql_0001
       ) tsql_0002
      ) tsql_0003
      WHERE `rank` = `count`
     ) tsql_0004
    ) tsql_0005 ORDER BY `subjectID`

``` r
DBI::dbDisconnect(rsqlite)
```
