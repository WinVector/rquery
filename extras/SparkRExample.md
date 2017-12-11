SparkR
================
Win-Vector LLC
12/11/2017

Show our `SparkR` setup.

``` r
library("wrapr")
library("rquery")
library("SparkR")
packageVersion("SparkR")
```

    ## [1] '2.2.0'

``` r
print(sr)
```

    ## Java ref type org.apache.spark.sql.SparkSession id 1

``` r
print(dSparkR)
```

    ## SparkDataFrame[subjectID:double, surveyCategory:string, assessmentTotal:double, irrelevantCol1:string, irrelevantCol2:string]

``` r
SparkR::createOrReplaceTempView(dSparkR, 
                                "dSparkR")

SparkR::sql("SELECT * from dSparkR") %.>%
  head(.) %.>%
  knitr::kable(.)
```

|  subjectID| surveyCategory      |  assessmentTotal| irrelevantCol1 | irrelevantCol2 |
|----------:|:--------------------|----------------:|:---------------|:---------------|
|          1| withdrawal behavior |                5| irrel1         | irrel2         |
|          1| positive re-framing |                2| irrel1         | irrel2         |
|          2| withdrawal behavior |                3| irrel1         | irrel2         |
|          2| positive re-framing |                4| irrel1         | irrel2         |

Run the same query as the [`rquery` example](https://winvector.github.io/rquery/).

``` r
scale <- 0.237
d <- rquery::table_source(table_name = "dSparkR",
                          columns = colnames(dSparkR),
                          dbqi = function(id) { paste0("`", id, "`") },
                          dbqs = function(s) { paste0('"', s, '"') })
                          

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
             orderby = 'probability')  %.>%
  extend_nse(.,
             isdiagnosis := rank == count,
             diagnosis := surveyCategory) %.>%
  select_rows_nse(., isdiagnosis) %.>%
  select_columns(., c("subjectID", 
                      "diagnosis", 
                      "probability")) %.>%
  order_by(., 'subjectID')

sql <- rquery::to_sql(dq)

# run query through SparkR
SparkR::sql(sql) %.>%
  head(.) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|
