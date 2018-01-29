
<!-- README.md is generated from README.Rmd. Please edit that file -->
`rquery`
========

[`rquery`](https://winvector.github.io/rquery/) is a query generator based on [Codd's relational algebra](https://en.wikipedia.org/wiki/Relational_algebra) (updated to reflect lessons learned from working with [`R`](https://www.r-project.org), [`SQL`](https://en.wikipedia.org/wiki/SQL), and [`dplyr`](https://CRAN.R-project.org/package=dplyr) at big data scale in production). One goal of this experiment is to see if `SQL` would be more fun teachable if it had a sequential data-flow or pipe notation.

`rquery` is currently experimental, and not yet recommended for production use.

To install: `devtools::install_github("WinVector/rquery")`.

Discussion
==========

[`rquery`](https://github.com/WinVector/rquery) can be an excellent advanced `SQL` training tool (it shows how some very deep `SQL` by composing `rquery` operators). Currently `rquery` is biased towards the `Spark` and `PostgeSQL` `SQL` dialects.

There are many prior relational algebra inspired specialized query languages. Just a few include:

-   [`Alpha`](https://en.wikipedia.org/wiki/Alpha_(programming_language)) ~1971.
-   [`QUEL`](https://en.wikipedia.org/wiki/QUEL_query_languages) ~1974.
-   [`SQL`](https://en.wikipedia.org/wiki/SQL) ~1986.
-   [`Tutorial D`](https://en.wikipedia.org/wiki/D_(data_language_specification)#Tutorial_D) ~2000.
-   [`data.table`](http://r-datatable.com/) ~2006.
-   [`LINQ`](https://msdn.microsoft.com/en-us/library/bb308959.aspx) ~2007.
-   [`pandas`](http://pandas.pydata.org) ~2008.
-   [`dplyr`](http://dplyr.tidyverse.org) ~2014.

`rquery` is realized as a thin translation to an underlying `SQL` provider. We are trying to put the Codd relational operators front and center (using the original naming, and back-porting `SQL` progress such as window functions to the appropriate relational operator).

The primary relational operators include:

-   [`extend()`](https://winvector.github.io/rquery/reference/extend_nse.html). Extend adds derived columns to a relation table. With a sufficiently powerful `SQL` provider this includes ordered and partitioned window functions. This operator also includes built-in [`seplyr`](https://winvector.github.io/seplyr/)-style [assignment partitioning](https://winvector.github.io/seplyr/articles/MutatePartitioner.html).
-   [`project()`](https://winvector.github.io/rquery/reference/project_nse.html). Project is usually *portrayed* as the equivalent to column selection, though the original definition includes aggregation. In our opinion the original relational nature of the operator is best captured by moving `SQL`'s "`GROUP BY`" aggregation functionality.
-   [`natural_join()`](https://winvector.github.io/rquery/reference/natural_join.html). This a specialized relational join operator, using all common columns as an equi-join condition.
-   [`theta_join()`](https://winvector.github.io/rquery/reference/theta_join_nse.html). This is the relational join operator allowing an arbitrary matching predicate.
-   [`select_rows()`](https://winvector.github.io/rquery/reference/theta_join_nse.html). This is Codd's relational row selection. Obviously `select` alone is an over-used and now ambiguous term (for example: it is already used as the "doit" verb in `SQL` and the *column* selector in `dplyr`).
-   [`rename_columns()`](https://winvector.github.io/rquery/reference/rename_columns.html). This operator renames sets of columns.

The primary non-relational (traditional `SQL`) operators are:

-   [`select_columns()`](https://winvector.github.io/rquery/reference/select_columns.html). This allows choice of columns (central to `SQL`), but is not a relational operator as it can damage row-uniqueness.
-   [`orderby()`](https://winvector.github.io/rquery/reference/orderby.html). Row order is not a concept in the relational algebra (and also not maintained in most `SQL` implementations). This operator is only useful when used with its `limit=` option, or as the last step as data comes out of the relation store and is moved to `R` (where row-order is usually maintained).

The primary missing relational operators are:

-   Union.
-   Direct set difference, anti-join.
-   Division.

A great benefit of Codd's relational algebra is it gives one concepts to decompose complex data transformations into sequences of simpler transformations.

Some reasons `SQL` seems complicated include:

-   `SQL`'s realization of sequencing as nested function composition.
-   `SQL` uses some relational concepts as steps, others as modifiers and predicates.

A lot of the grace of the Codd theory can be recovered through the usual trick changing function composition notation from `g(f(x))` to `x . f() . g()`. This experiment is asking (and not for the first time): "what if `SQL` were piped (expressed composition as a left to right flow, instead of a right to left nesting)?"

Let's work a non-trivial example: the `dplyr` pipeline from [Let’s Have Some Sympathy For The Part-time R User](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

First we show the Spark/database version of the original example data:

``` r
class(my_db)
```

    ## [1] "spark_connection"       "spark_shell_connection"
    ## [3] "DBIConnection"

``` r
print(d)
```

    ## [1] "table('d')"

``` r
d %.>%
  rquery::to_sql(., my_db) %.>%
  DBI::dbGetQuery(my_db, .) %.>%
  knitr::kable(.)
```

|  subjectID| surveyCategory      |  assessmentTotal| irrelevantCol1 | irrelevantCol2 |
|----------:|:--------------------|----------------:|:---------------|:---------------|
|          1| withdrawal behavior |                5| irrel1         | irrel2         |
|          1| positive re-framing |                2| irrel1         | irrel2         |
|          2| withdrawal behavior |                3| irrel1         | irrel2         |
|          2| positive re-framing |                4| irrel1         | irrel2         |

Now we re-write the original calculation in terms of the `rquery` SQL generating operators.

``` r
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

We then generate our result:

``` r
dq %.>%
  to_sql(., my_db, source_limit = 1000) %.>%
  DBI::dbGetQuery(my_db, .) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|

We see we have quickly reproduced the original result using the new database operators. This means such a calculation could easily be performed at a "big data" scale (using a database or `Spark`; in this case we would not take the results back, but instead use `CREATE TABLE tname AS` to build a remote materialized view of the results).

The actual `SQL` query that produces the result is, in fact, quite involved:

``` r
cat(to_sql(dq, my_db, source_limit = 1000))
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

The query is large, but due to its regular structure it should be very amenable to query optimization.

A feature to notice is: the query was automatically restricted to just columns actually needed from the source table to complete the calculation. This has the possibility of decreasing data volume and greatly speeding up query performance. Our [initial experiments](https://github.com/WinVector/rquery/blob/master/extras/PerfTest.md) show `rquery` narrowed queries to be twice as fast as un-narrowed `dplyr` on a synthetic problem simulating large disk-based queries. We think if we connected directly to `Spark`'s relational operators (avoiding the `SQL` layer) we may be able to achieve even faster performance.

The above optimization is possible because the `rquery` representation is an intelligible tree of nodes, so we can interrogate the tree for facts about the query. For example:

``` r
column_names(dq)
```

    ## [1] "diagnosis"   "probability" "subjectID"

``` r
tables_used(dq)
```

    ## $d
    ## [1] "table('d')"

``` r
columns_used(dq)
```

    ## $d
    ## [1] "subjectID"       "surveyCategory"  "assessmentTotal"

Part of the plan is: the additional record-keeping in the operator nodes would let a potentially powerful query optimizer work over the flow before it gets translated to `SQL` (perhaps an extension of or successor to [`seplyr`](https://winvector.github.io/seplyr/), which re-plans over `dplyr::mutate()` expressions). At the very least restricting to columns later used and folding selects together would be achievable. One should have a good chance at optimization as the representation is fairly high-level, and many of the operators are relational (meaning there are known legal transforms a query optimizer can use). The flow itself is represented as follows:

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
      c('diagnosis' = 'surveyCategory')) %.>%
     select_rows(., rank = count) %.>%
     select_columns(., subjectID, diagnosis, probability) %.>%
     orderby(., subjectID)

We also can stand `rquery` up on non-`DBI` sources such as [`SparkR`](https://github.com/WinVector/rquery/blob/master/extras/SparkRExample.md) and perhaps even [`data.table`](https://github.com/WinVector/rquery/blob/master/extras/data_table.md).
