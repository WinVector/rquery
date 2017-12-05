rquery
================
2017-12-04

<!-- README.md is generated from README.Rmd. Please edit that file -->
`rquery` is a experimental/demonstration of a simplified sequenced query language based on [Codd's relational algebra](https://en.wikipedia.org/wiki/Relational_algebra) and not currently recommended for non-experimental use. `rquery` is something we whipped up in a singe weekend to see how small a scope such an adapter might have.

A few things `rquery` is missing include:

-   `rquery` does not have the table lifetime management tools.
-   Substitution of values from the environment into expressions.
-   Quoting of column names within complex expressions.

All of the above are easy to add, but not central to the one-off experiment. `rquery` is not for production use, but can be an excellent advanced `SQL` training tool (it shows how some very deep `SQL` by composing `rquery` operators). Right now `rquery` is biased towards `PostgeSQL` `SQL`.

There are many other relational algebra inspired specialized query languages.

-   [Alpha](https://en.wikipedia.org/wiki/Alpha_(programming_language))
-   [QUEL](https://en.wikipedia.org/wiki/QUEL_query_languages)
-   [Tutorial D](https://en.wikipedia.org/wiki/D_(data_language_specification)#Tutorial_D)
-   [LINQ](https://msdn.microsoft.com/en-us/library/bb308959.aspx)
-   [SQL](https://en.wikipedia.org/wiki/SQL)
-   (and possibly) [`dplyr`](http://dplyr.tidyverse.org)

`rquery` is a thin translation to `SQL` layer, but we are trying to put the Codd relational operators front and center (user their naming, and back-port `SQL` progress such as window functions to the appropriate relational operator). `rquery` differs from `dplyr` in that `rquery` is trying to stay near the Codd relational operators (in particular grouping is a transient state inside the `rquery::extend()` operator, not a durable user visible annotation as with `dplyr::group_by()`).

The primary relational operators are:

-   `extend()`. Extend adds derived columns to a relation table. With a sufficiently powerful `SQL` provider this includes ordered and partitioned window functions.
-   `project()`. Project is usually portrait as the equivalent to column selection. In our opinion the original relational nature of the operator is best captured by moving `SQL`'s "`GROUP BY`" aggregation functionality to this operator.
-   `natural_join()`. This is the relational join operator, using all common columns as the equi-join condition. The next operator to add would definitely be `theta-join` as that adds a lot more expressiveness to the grammar.
-   `select_rows()`. This is Codd's relational row selection. Obviously `select` alone is an over-used and now ambiguous term (it is the "doit" verb in `SQL` and the *column* selector in `dplyr`).

The primary non-relational (traditional `SQL`) operators are:

-   `select_columns()`. This allows choice of columns (central to `SQL`), but is not a relational operator as it can damage row-uniqueness.
-   `order_by()`. This is a non-relational "user presentation" verb. Row order is not well-defined in the relational algebra (and also not in most `SQL` implementations). If used it should be used last in a query (so it is not undone by later operations).

The primary missing relational operators are:

-   Direct rename.
-   Union.
-   Direct set difference, anti-join.
-   Theta-join.
-   Division.

Primary useful missing operators:

-   Deselect columns.

A great benefit of Codd's relational algebra is it decomposes data transformations into a sequence of operators. `SQL` loses a lot of the original invariants, and over-specifies how operations are strung together and insisting on a nesting function notation. `SQL` also realizes some of the Codd concepts as operators, some as expressions, and some as predicates (obscuring the uniformity of the original theory).

A lot of the grace of the Codd theory can be recovered through the usual trick changing function composition notation from `g(f(x))` as `x . f() . g()`.

The `rquery` operators are passive. They don't do anything other than collect a specification of the desired calculation. This data structure can then be printed in a friendly fashion, used to generate `SQL`, and (in principle) be the representational layer for a higher-order optimizer.

As an acid test we generate a query equivalent to the non-trivial `dplyr` pipeline demonstrated in [Let’s Have Some Sympathy For The Part-time R User](http://www.win-vector.com/blog/2017/08/lets-have-some-sympathy-for-the-part-time-r-user/).

First we set up the database and example data:

``` r
library("rquery")
```

    ## Loading required package: wrapr

``` r
library('RPostgreSQL')
```

    ## Loading required package: DBI

``` r
my_db <- DBI::dbConnect(dbDriver("PostgreSQL"), 
                        host = 'localhost',
                        port = 5432,
                        user = 'postgres',
                        password = 'pg')

d <- dbi_copy_to(my_db, 'd',
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
                   stringsAsFactors = FALSE),
                 temporary = TRUE, overwrite = TRUE)

print(d)
```

    ## [1] "dbi_table('d')"

``` r
d %.>%
  to_sql(.) %.>%
  DBI::dbGetQuery(my_db, .) %.>%
  knitr::kable(.)
```

| row.names |  subjectID| surveyCategory      |  assessmentTotal|
|:----------|----------:|:--------------------|----------------:|
| 1         |          1| withdrawal behavior |                5|
| 2         |          1| positive re-framing |                2|
| 3         |          2| withdrawal behavior |                3|
| 4         |          2| positive re-framing |                4|

Now we write the calculation in terms of our operators (we have not yet bothered to add the expression capture features to all of the `rquery` operators, so we are currently simulating it using the development version `1.0.2` of [`wrapr`](https://winvector.github.io/wrapr/); the package itself works with the production release version of `wrapr`).

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
             orderby = 'probability')  %.>%
  extend_nse(.,
             isdiagnosis := rank >= count,
             diagnosis := surveyCategory) %.>%
  select_rows(., qe(isdiagnosis)) %.>%
  select_columns(., qc(subjectID, 
                       diagnosis, 
                       probability)) %.>%
  order_by(., 'subjectID')
```

All of the non-standard expression capture (`wrapr::qc()`, `wrapr::qae()`, substituting in the `scale`, and avoiding the quoted `SQL` column names) could easily be incorporated into the relation nodes.

We then have our result:

``` r
dq %.>%
  to_sql(.) %.>%
  DBI::dbGetQuery(my_db, .) %.>%
  knitr::kable(.)
```

|  subjectID| diagnosis           |  probability|
|----------:|:--------------------|------------:|
|          1| withdrawal behavior |    0.6706221|
|          2| positive re-framing |    0.5589742|

We see we reproduced the result purely in terms of these database operators.

The actual `SQL` query that produces the result is quite involved:

``` r
cat(to_sql(dq))
```

    SELECT * FROM (
     SELECT
      "subjectID", "diagnosis", "probability"
     FROM (
      SELECT * FROM (
       SELECT
        "row.names",
        "subjectID",
        "surveyCategory",
        "assessmentTotal",
        "probability",
        "count",
        "rank",
        "rank" >= "count"  AS "isdiagnosis",
        "surveyCategory"  AS "diagnosis"
       FROM (
        SELECT
         "row.names",
         "subjectID",
         "surveyCategory",
         "assessmentTotal",
         "probability",
         "count",
         rank()  OVER (  PARTITION BY "subjectID" ORDER BY "probability" ) AS "rank"
        FROM (
         SELECT
          "row.names",
          "subjectID",
          "surveyCategory",
          "assessmentTotal",
          exp("assessmentTotal" * 0.237) / sum(exp("assessmentTotal" * 0.237))  OVER (  PARTITION BY "subjectID" ) AS "probability",
          count(1)  OVER (  PARTITION BY "subjectID" ) AS "count"
         FROM (
          SELECT * FROM "d"
         ) tsql_bkxufbzdpahhmgjnoifi_0000000000
        ) tsql_bkxufbzdpahhmgjnoifi_0000000001
       ) tsql_bkxufbzdpahhmgjnoifi_0000000002
      ) tsql_bkxufbzdpahhmgjnoifi_0000000003
      WHERE isdiagnosis
     ) tsql_bkxufbzdpahhmgjnoifi_0000000004
    ) tsql_bkxufbzdpahhmgjnoifi_0000000005 ORDER BY "subjectID"

Part of the hope is the additional record keeping in the operator nodes would let a very powerful query optimizer work over the flow before it gets translated to `SQL`. At the very least restricting to columns later used and folding selects together would be achievable. One should have a good chance at optimization as the representation is fairly high-level, and many of the operators are relational (meaning there are known legal transforms a query optimizer can use). The flow itself is represented as follows:

``` r
print(dq)
```

    [1] "dbi_table('d') %.>% extend(., probability := exp(\"assessmentTotal\" * 0.237) / sum(exp(\"assessmentTotal\" * 0.237)), count := count(1); p: subjectID) %.>% extend(., rank := rank(); p: subjectID; o: probability) %.>% extend(., isdiagnosis := \"rank\" >= \"count\", diagnosis := \"surveyCategory\") %.>% select_rows(., isdiagnosis) %.>% select_columns(., subjectID, diagnosis, probability) %.>% order_by(., subjectID)"

We can even pretty-format it:

``` r
cat(gsub("%.>%", "%.>%\n   ", format(dq), fixed = TRUE))
```

    dbi_table('d') %.>%
        extend(., probability := exp("assessmentTotal" * 0.237) / sum(exp("assessmentTotal" * 0.237)), count := count(1); p: subjectID) %.>%
        extend(., rank := rank(); p: subjectID; o: probability) %.>%
        extend(., isdiagnosis := "rank" >= "count", diagnosis := "surveyCategory") %.>%
        select_rows(., isdiagnosis) %.>%
        select_columns(., subjectID, diagnosis, probability) %.>%
        order_by(., subjectID)

And that is our weekend experiment.

``` r
DBI::dbDisconnect(my_db)
```

    ## [1] TRUE

Note: `rquery` is only an experimental package. Also, it is not currently checked, but all `rquery` operators should be only used in "zero dependency mode" (never using a value created in the same operator or writing the same value twice) in the sense of [`seplyr::partition_mutate_qt`](https://www.rdocumentation.org/packages/seplyr/versions/0.5.0/topics/partition_mutate_qt). Again, the point was to see how quickly one can get a workable data transform pipeline in terms of Codd-inspired operators.
