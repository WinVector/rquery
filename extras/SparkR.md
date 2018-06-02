SparkR Example
================
John Mount, Win-Vector LLC
06/02/2018

Connect to a `SparkR` cluster and work a small example.

To install a practice version of `Spark`/`SparkR` v2.3.0 on a stand-alone workstation:

-   First download Spark 2.3.0 Pre-built for Apache Hadoop 2.7 or later ([spark-2.3.0-bin-hadoop2.7.tgz](https://www.apache.org/dyn/closer.lua/spark/spark-2.3.0/spark-2.3.0-bin-hadoop2.7.tgz)) from [Apache Spark Downloads](https://spark.apache.org/downloads.html).
-   Uncompress this into a directory named `spark-2.3.0-bin-hadoop2.7`.
-   Install `SparkR` from `spark-2.3.0-bin-hadoop2.7/spark-2.3.0-bin-hadoop2.7/R/lib/SparkR`: `install.packages("~/Downloads/spark-2.3.0-bin-hadoop2.7/R/lib/SparkR/", repos = NULL, type = "source")`.
-   Use `SparkR` package to install its own local `Spark`: `SparkR::install.spark()` (based on [sparkr-vignettes.Rmd](https://github.com/apache/spark/blob/master/R/pkg/vignettes/sparkr-vignettes.Rmd)).

[`rquery`](https://winvector.github.io/rquery/) example.

``` r
library("rquery")
```

    ## Loading required package: wrapr

``` r
print(db_hdl)
```

    ## [1] "rquery_db_info(is_dbi=FALSE, SparkR, <environment: 0x7fbff5960448>)"

``` r
print(test_df)
```

    ## SparkDataFrame[subjectID:int, surveyCategory:string, assessmentTotal:int, irrelevantCol_0000001:double]

``` r
d_hdl <- sparkr_table(db_hdl, test_df)

print(d_hdl)
```

    ## [1] "table('rs_81899890864282959322_0000000000'; subjectID, surveyCategory, assessmentTotal, irrelevantCol_0000001)"

``` r
print(column_names(d_hdl))
```

    ## [1] "subjectID"             "surveyCategory"        "assessmentTotal"      
    ## [4] "irrelevantCol_0000001"

``` r
scale <- 0.237

rquery_pipeline <- d_hdl %.>%
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
  select_columns(., c('subjectID', 
                      'diagnosis', 
                      'probability')) %.>%
  orderby(., cols = 'subjectID')

rquery_pipeline %.>%
  op_diagram(.) %.>% 
  DiagrammeR::grViz(.)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-bd94130bfb3b243fc9cd">{"x":{"diagram":"\ndigraph rquery_optree {\n  graph [ layout = dot, rankdir = TB, overlap = prism, compound = true, nodesep = .5, ranksep = .25]\n  edge [decorate = true, arrowhead = normal]\n  node [style=filled, fillcolor=lightgrey]\n\nnode_1 [ shape = \"folder\" , label = \"table(rs_81899890864282959322_0000000000; \\l  subjectID,\\l  surveyCategory,\\l  assessmentTotal,\\l  irrelevantCol_0000001)\\l\"]\n\nnode_2 [ shape = \"tab\" , label = \"extend(.,\\l  probability := exp(assessmentTotal * scale))\\l\"]\n\nnode_3 [ shape = \"tab\" , label = \"extend(.,\\l  probability := probability / sum(probability),\\l  p= subjectID)\\l\"]\n\nnode_4 [ shape = \"tab\" , label = \"extend(.,\\l  row_rank := rank(),\\l  p= subjectID,\\l  o= probability DESC, surveyCategory DESC)\\l\"]\n\nnode_5 [ shape = \"tab\" , label = \"select_rows(.,\\l   row_rank <= 1)\\l\"]\n\nnode_6 [ shape = \"tab\" , label = \"rename(.,\\l  c(diagnosis = surveyCategory))\\l\"]\n\nnode_7 [ shape = \"tab\" , label = \"select_columns(.,\\l   subjectID, diagnosis, probability)\\l\"]\n\nnode_8 [ shape = \"tab\" , label = \"orderby(., subjectID)\\l\"]\nnode_1 -> node_2 [ label = \".\"]\nnode_2 -> node_3 [ label = \".\"]\nnode_3 -> node_4 [ label = \".\"]\nnode_4 -> node_5 [ label = \".\"]\nnode_5 -> node_6 [ label = \".\"]\nnode_6 -> node_7 [ label = \".\"]\nnode_7 -> node_8 [ label = \".\"]\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
execute(db_hdl, rquery_pipeline) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.5000000|
|          2| withdrawal behavior |    0.6706221|
|          3| withdrawal behavior |    0.5000000|
|          4| positive re-framing |    0.5589742|
|          5| withdrawal behavior |    0.5589742|
|          6| withdrawal behavior |    0.5000000|
|          7| positive re-framing |    0.6706221|
|          8| positive re-framing |    0.5589742|
|          9| positive re-framing |    0.6706221|
|         10| positive re-framing |    0.5589742|
