SparkR Example
================
Nina Zumel and John Mount, Win-Vector LLC
07/03/2018

The What and Why of `rquery`
----------------------------

`rquery` is a query generator for R. It is based on [Edgar F. Codd’s relational algebra](https://en.wikipedia.org/wiki/Relational_algebra), informed by our experience using SQL and R packages such as `dplyr` at big data scale.

The design represents an attempt to make SQL more teachable by denoting composition by a sequential pipeline notation instead of nested queries or functions.

A great benefit of Codd's relational algebra is it gives one concepts to decompose complex data transformations into sequences of simpler transformations.

Some reasons SQL seems complicated include:

    SQL's realization of sequencing as nested function composition.
    SQL uses some relational concepts as steps, others as modifiers and predicates.

A lot of the grace of the Codd theory can be recovered through the usual trick changing function composition notation from g(f(x)) to x . f() . g(). This experiment is asking (and not for the first time): "what if SQL were piped (expressed composition as a left to right flow, instead of a right to left nesting)?"

Design Choices for SQL implementation
-------------------------------------

Stay close to Cod's definitions. In contrast to other R data manipulation packages, rquery does not rely on db-unsafe annotations such as row number or "group by" annotations. Instead, grouped aggregations are implemented via window functions in SQL.

How Spark & R developers can benefit from `rquery`
--------------------------------------------------

Domain independent query language that runs on Spark in R via either SparkR or sparklyr, as well as on Postgres and other large data systems.

R developers can run analyses and perform data transformations in Spark using an easier to read (and to write) sequential pipeline notation instead of nested sql queries.

Superior error checking -- verifies column names before going to database

Some query optimization; in particular rquery checks "column liveness" : it checks which columns from a table are involved in a given query, and proactively issues the approprate SELECT statements to narrow the tables being manipulated. Helps with excessively wide tables.

Well-formatted textual as well as graphicl presentation of query plans.

Example
-------

Connect to a `SparkR` cluster and work a small example.

To install a practice version of `Spark`/`SparkR` v2.3.0 on a stand-alone workstation:

-   First download Spark 2.3.0 Pre-built for Apache Hadoop 2.7 or later ([spark-2.3.0-bin-hadoop2.7.tgz](https://www.apache.org/dyn/closer.lua/spark/spark-2.3.0/spark-2.3.0-bin-hadoop2.7.tgz)) from [Apache Spark Downloads](https://spark.apache.org/downloads.html).
-   Uncompress this into a directory named `spark-2.3.0-bin-hadoop2.7`.
-   Install `SparkR` from `spark-2.3.0-bin-hadoop2.7/spark-2.3.0-bin-hadoop2.7/R/lib/SparkR`: `install.packages("~/Downloads/spark-2.3.0-bin-hadoop2.7/R/lib/SparkR/", repos = NULL, type = "source")`.
-   Use `SparkR` package to install its own local `Spark`: `SparkR::install.spark()` (based on [sparkr-vignettes.Rmd](https://github.com/apache/spark/blob/master/R/pkg/vignettes/sparkr-vignettes.Rmd)).

Let's imagine that we run a food delivery business, and we are interested in what types of cuisines ('Mexican', 'Chinese', etc) our customers prefer. We a

[`rquery`](https://winvector.github.io/rquery/) example.

Let's assume that we already have the data

``` r
library("rquery")

print(db_hdl) # rquery handle into Spark
```

    ## [1] "rquery_db_info(is_dbi=FALSE, SparkR, <environment: 0x7f9bb09c1610>)"

``` r
order_f <- SparkR::tableToDF("order_table")
print(order_f)
```

    ## SparkDataFrame[custID:string, restaurant_type:string, orderID:int]

``` r
#
# create an rquery table description of the SparkDataFrame
#

# get the table's column names
cols <- SparkR::colnames(order_f)
# create the rquery table description
table_description <- mk_td("order_table", cols)

print(table_description)
```

    ## [1] "table('order_table'; custID, restaurant_type, orderID)"

``` r
print(column_names(table_description))
```

    ## [1] "custID"          "restaurant_type" "orderID"

``` r
rquery_pipeline <- table_description %.>%
  extend_nse(., one = 1) %.>%  # a column to help count
  project_nse(., groupby=c("custID", "restaurant_type"),
              total_orders = sum(one)) %.>%
  normalize_cols(.,   # normalize the total_order counts
                 "total_orders",
                 partitionby = 'custID') %.>%
  rename_columns(.,  # rename the column
                 c('fraction_of_orders' = 'total_orders')) %.>% 
  pick_top_k(.,  # get the most frequent cuisine type
             k = 1,
             partitionby = 'custID',
             orderby = c('fraction_of_orders', 'restaurant_type'),
             reverse = c('fraction_of_orders')) %.>% 
  rename_columns(., c('favorite_cuisine' = 'restaurant_type')) %.>%
  select_columns(., c('custID', 
                      'favorite_cuisine', 
                      'fraction_of_orders')) %.>%
  orderby(., cols = 'custID')
```

``` r
cat(format(rquery_pipeline))
```

    ## table('order_table'; 
    ##   custID,
    ##   restaurant_type,
    ##   orderID) %.>%
    ##  extend(.,
    ##   one := 1) %.>%
    ##  project(., total_orders := sum(one),
    ##   g= custID, restaurant_type) %.>%
    ##  extend(.,
    ##   total_orders := total_orders / sum(total_orders),
    ##   p= custID) %.>%
    ##  rename(.,
    ##   c('fraction_of_orders' = 'total_orders')) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   p= custID,
    ##   o= "fraction_of_orders" DESC, "restaurant_type") %.>%
    ##  select_rows(.,
    ##    row_number <= 1) %.>%
    ##  rename(.,
    ##   c('favorite_cuisine' = 'restaurant_type')) %.>%
    ##  select_columns(.,
    ##    custID, favorite_cuisine, fraction_of_orders) %.>%
    ##  orderby(., custID)

``` r
rquery_pipeline %.>%
  op_diagram(.) %.>% 
  DiagrammeR::DiagrammeR(diagram = ., type = "grViz") %.>% 
  DiagrammeRsvg::export_svg(.) %.>% 
  charToRaw(.) %.>%
  rsvg::rsvg_png(., file = "Sparkr_files/diagram1.png")
```

![](Sparkr_files/diagram1.png)

Note that rquery knows not to use orderID

``` r
columns_used(rquery_pipeline)
```

    ## $order_table
    ## [1] "custID"          "restaurant_type"

``` r
execute(db_hdl, rquery_pipeline) %.>%
  knitr::kable(.)
```

| custID  | favorite\_cuisine |  fraction\_of\_orders|
|:--------|:------------------|---------------------:|
| cust\_1 | Italian           |             0.3225806|
| cust\_2 | Italian           |             0.3125000|
| cust\_3 | Indian            |             0.2857143|
| cust\_4 | American          |             0.2916667|
| cust\_5 | American          |             0.2857143|
| cust\_6 | Italian           |             0.2800000|
| cust\_7 | American          |             0.2400000|
| cust\_8 | Indian            |             0.2903226|
| cust\_9 | Chinese           |             0.3000000|

Print the SQL. Notice that orderID is already eliminated in the initial select (tsql\_\*\_0000000000)

``` r
cat(to_sql(rquery_pipeline, db_hdl))
```

    ## SELECT * FROM (
    ##  SELECT
    ##   `custID`,
    ##   `favorite_cuisine`,
    ##   `fraction_of_orders`
    ##  FROM (
    ##   SELECT
    ##    `custID` AS `custID`,
    ##    `fraction_of_orders` AS `fraction_of_orders`,
    ##    `restaurant_type` AS `favorite_cuisine`
    ##   FROM (
    ##    SELECT * FROM (
    ##     SELECT
    ##      `custID`,
    ##      `restaurant_type`,
    ##      `fraction_of_orders`,
    ##      row_number ( ) OVER (  PARTITION BY `custID` ORDER BY `fraction_of_orders` DESC, `restaurant_type` ) AS `row_number`
    ##     FROM (
    ##      SELECT
    ##       `custID` AS `custID`,
    ##       `restaurant_type` AS `restaurant_type`,
    ##       `total_orders` AS `fraction_of_orders`
    ##      FROM (
    ##       SELECT
    ##        `custID`,
    ##        `restaurant_type`,
    ##        `total_orders` / sum ( `total_orders` ) OVER (  PARTITION BY `custID` ) AS `total_orders`
    ##       FROM (
    ##        SELECT `custID`, `restaurant_type`, sum ( `one` ) AS `total_orders` FROM (
    ##         SELECT
    ##          `custID`,
    ##          `restaurant_type`,
    ##          1  AS `one`
    ##         FROM (
    ##          SELECT
    ##           `custID`,
    ##           `restaurant_type`
    ##          FROM
    ##           `order_table`
    ##          ) tsql_61541625243242887727_0000000000
    ##         ) tsql_61541625243242887727_0000000001
    ##        GROUP BY
    ##         `custID`, `restaurant_type`
    ##        ) tsql_61541625243242887727_0000000002
    ##      ) tsql_61541625243242887727_0000000003
    ##      ) tsql_61541625243242887727_0000000004
    ##    ) tsql_61541625243242887727_0000000005
    ##    WHERE `row_number` <= 1
    ##   ) tsql_61541625243242887727_0000000006
    ##  ) tsql_61541625243242887727_0000000007
    ## ) tsql_61541625243242887727_0000000008 ORDER BY `custID`

``` r
library(dplyr)
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
order_f %>% 
  SparkR::as.data.frame() %>%
  group_by(custID, restaurant_type) %>%
  summarize(total_orders = n()) %>%
  ungroup() %>%
  group_by(custID) %>%
  mutate(fraction_of_orders = total_orders/sum(total_orders)) %>%
  ungroup() %>%
  group_by(custID) %>%
  arrange(desc(fraction_of_orders), restaurant_type) %>%
  mutate(rnk = row_number()) %>%
  ungroup() %>%
  filter(rnk == 1) %>%
  rename(favorite_cuisine = restaurant_type) %>%
  select(custID, favorite_cuisine, fraction_of_orders) %>%
  arrange(custID) %>%
  knitr::kable()
```

| custID  | favorite\_cuisine |  fraction\_of\_orders|
|:--------|:------------------|---------------------:|
| cust\_1 | Italian           |             0.3225806|
| cust\_2 | Italian           |             0.3125000|
| cust\_3 | Indian            |             0.2857143|
| cust\_4 | American          |             0.2916667|
| cust\_5 | American          |             0.2857143|
| cust\_6 | Italian           |             0.2800000|
| cust\_7 | American          |             0.2400000|
| cust\_8 | Indian            |             0.2903226|
| cust\_9 | Chinese           |             0.3000000|

Note you MUST use "%.&gt;%" (aka wrapr dot-arrow) rather than the magrittr pipe for this next step.

``` r
library(rqdatatable)  # automatically registers rqdatatable as the default executor for rquery pipelines

SparkR::as.data.frame(order_f) %.>%
  rquery_pipeline %.>%
  knitr::kable(.)
```

| custID  | favorite\_cuisine |  fraction\_of\_orders|
|:--------|:------------------|---------------------:|
| cust\_1 | Italian           |             0.3225806|
| cust\_2 | Italian           |             0.3125000|
| cust\_3 | Indian            |             0.2857143|
| cust\_4 | American          |             0.2916667|
| cust\_5 | American          |             0.2857143|
| cust\_6 | Italian           |             0.2800000|
| cust\_7 | American          |             0.2400000|
| cust\_8 | Indian            |             0.2903226|
| cust\_9 | Chinese           |             0.3000000|
