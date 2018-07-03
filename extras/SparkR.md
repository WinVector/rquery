SparkR Example
================
John Mount, Win-Vector LLC
06/02/2018

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

TODO: switch to number of times ordering from different restaurants. Looking for the favorite and fraction of orders from orders from that restaunt (or quisine). Slow down and define problem and show data, before any work.

Introduce the data here

[`rquery`](https://winvector.github.io/rquery/) example.

Let's assume that we already have the data

``` r
library("rquery")

print(db_hdl) # rquery handle into Spark
```

    ## [1] "rquery_db_info(is_dbi=FALSE, SparkR, <environment: 0x7f869b7e72e8>)"

``` r
survey_f <- SparkR::tableToDF("survey_table")
print(survey_f)
```

    ## SparkDataFrame[custID:int, restaurantType:string, totalOrders:int]

``` r
#
# create an rquery table description of the SparkDataFrame
#

# get the table's column names
cols <- SparkR::colnames(survey_f)
# create the rquery table description
table_description <- mk_td("survey_table", cols)

print(table_description)
```

    ## [1] "table('survey_table'; custID, restaurantType, totalOrders)"

``` r
print(column_names(table_description))
```

    ## [1] "custID"         "restaurantType" "totalOrders"

``` r
rquery_pipeline <- table_description %.>%
  normalize_cols(.,   # normalize the restaurantType counts
                 "totalOrders",
                 partitionby = 'custID') %.>%
  rename_columns(.,  # rename the column
                 c('fraction_of_orders' = 'totalOrders')) %.>% 
  pick_top_k(.,
             k = 1,
             partitionby = 'custID',
             orderby = c('fraction_of_orders', 'restaurantType'),
             reverse = c('fraction_of_orders')) %.>% 
  rename_columns(., c('favorite_cuisine' = 'restaurantType')) %.>%
  select_columns(., c('custID', 
                      'favorite_cuisine', 
                      'fraction_of_orders')) %.>%
  orderby(., cols = 'custID')
```

``` r
cat(format(rquery_pipeline))
```

    ## table('survey_table'; 
    ##   custID,
    ##   restaurantType,
    ##   totalOrders) %.>%
    ##  extend(.,
    ##   totalOrders := totalOrders / sum(totalOrders),
    ##   p= custID) %.>%
    ##  rename(.,
    ##   c('fraction_of_orders' = 'totalOrders')) %.>%
    ##  extend(.,
    ##   row_number := row_number(),
    ##   p= custID,
    ##   o= "fraction_of_orders" DESC, "restaurantType") %.>%
    ##  select_rows(.,
    ##    row_number <= 1) %.>%
    ##  rename(.,
    ##   c('favorite_cuisine' = 'restaurantType')) %.>%
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

``` r
columns_used(rquery_pipeline)
```

    ## $survey_table
    ## [1] "custID"         "restaurantType" "totalOrders"

``` r
execute(db_hdl, rquery_pipeline) %.>%
  knitr::kable(.)
```

|  custID| favorite\_cuisine |  fraction\_of\_orders|
|-------:|:------------------|---------------------:|
|       1| American          |             0.3709677|
|       2| Mexican           |             0.3478261|
|       3| Indian            |             0.4411765|
|       4| American          |             0.2686567|
|       5| Indian            |             0.2531646|
|       6| American          |             0.2948718|
|       7| Indian            |             0.3281250|
|       8| Italian           |             0.3064516|
|       9| American          |             0.2526316|
|      10| Italian           |             0.2823529|
