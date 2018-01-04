Assignment Partitioner
================
Win-Vector LLC
1/4/2018

`rquery::extend_se()` and `rquery::extend_nse()` each automatically partition a sequence of assignments so that no statement is using any value created in the same partition element or group. This is to eliminate potentially dangerous ambiguity in statements.

For such a partition: the evaluation result does not depend on the order of execution of the statements in each group (as they are all independent of each other's left-hand-sides). A no-dependency small number of groups partition is *very* helpful when executing expressions on `SQL` based data interfaces (such as `Apache Spark`).

The method used to partition expressions is to scan the remaining expressions in order taking any that: have all their values available from earlier groups, do not use a value formed in the current group, and do not overwrite a value formed in the current group.

This partitioning method ensures safe and correct results, and can lead to far fewer groups (and much more efficient queries) than the straightforward method of breaking up the sequence of expressions at each new-value use.

Here is a non-trivial database based example (demonstrating methods that might be used in big data work such as with `Spark`). We are going to assign pairs of items to complimentary treatment ("T") and control ("C") groups based on already populated pseudo-random numbers (pre-populating the pseudo-random numbers is to avoid [known issues in using `rand()` in `RSQlite`](https://winvector.github.io/rquery/reference/if_else_block.html), and would not be necessary with other databases).

``` r
library("rquery")
```

    Loading required package: wrapr

    Loading required package: cdata

``` r
# example data
set.seed(3463)
d <- data.frame(id = seq_len(8))
for(group in c('a', 'b', 'c', 'd', 'e')) {
  d[[paste0("rand_", group)]] <- runif(nrow(d))
}
my_db <- DBI::dbConnect(RSQLite::SQLite(), 
                        ":memory:")
d1 <- dbi_copy_to(my_db, "example_table", d)
d1 %.>%
  to_sql(., my_db) %.>%
  DBI::dbGetQuery(my_db, .) %.>%
  knitr::kable(.)
```

|   id|    rand\_a|    rand\_b|    rand\_c|    rand\_d|    rand\_e|
|----:|----------:|----------:|----------:|----------:|----------:|
|    1|  0.8438177|  0.2941489|  0.3038159|  0.2995439|  0.5480026|
|    2|  0.9045364|  0.4654982|  0.9056346|  0.8120420|  0.9372185|
|    3|  0.5496617|  0.6989960|  0.1683751|  0.4343531|  0.8829880|
|    4|  0.6545816|  0.9678277|  0.0597492|  0.9255471|  0.9309038|
|    5|  0.9459773|  0.1054046|  0.5010538|  0.0601368|  0.7485002|
|    6|  0.4839231|  0.6617276|  0.3697797|  0.8623212|  0.5131579|
|    7|  0.6112306|  0.6536909|  0.2838900|  0.6700042|  0.9088256|
|    8|  0.6593733|  0.8316179|  0.7254980|  0.3012025|  0.4575715|

``` r
# design the experiment 
plan <- d1 %.>%
  extend_nse(.,
   choice_a = rand_a>=0.5, 
    a_1 = ifelse(choice_a, 
                  'T', 
                  'C'),
    a_2 = ifelse(choice_a, 
                  'C', 
                  'T'),
   choice_b = rand_b>=0.5, 
    b_1 = ifelse(choice_b, 
                  'T', 
                  'C'),
    b_2 = ifelse(choice_b, 
                  'C', 
                  'T'),
   choice_c = rand_c>=0.5, 
    c_1 = ifelse(choice_c, 
                  'T', 
                  'C'),
    c_2 = ifelse(choice_c, 
                  'C', 
                  'T'),
   choice_d = rand_d>=0.5, 
    d_1 = ifelse(choice_d, 
                  'T', 
                  'C'),
    d_2 = ifelse(choice_d, 
                  'C', 
                  'T'),
   choice_e = rand_e>=0.5, 
    e_1 = ifelse(choice_e, 
                  'T', 
                  'C'),
    e_2 = ifelse(choice_e, 
                  'C', 
                  'T')
  ) %.>%
  select_columns(., 
                 qc(a_1, a_2, b_1, b_2,
                    c_1, c_2, d_1, d_2,
                    e_1, e_2))

cat(format(plan))
```

    table('example_table') %.>%
     extend(.,
      choice_a := rand_a >= 0.5,
      choice_b := rand_b >= 0.5,
      choice_c := rand_c >= 0.5,
      choice_d := rand_d >= 0.5,
      choice_e := rand_e >= 0.5) %.>%
     extend(.,
      a_1 := ifelse(choice_a, "T", "C"),
      a_2 := ifelse(choice_a, "C", "T"),
      b_1 := ifelse(choice_b, "T", "C"),
      b_2 := ifelse(choice_b, "C", "T"),
      c_1 := ifelse(choice_c, "T", "C"),
      c_2 := ifelse(choice_c, "C", "T"),
      d_1 := ifelse(choice_d, "T", "C"),
      d_2 := ifelse(choice_d, "C", "T"),
      e_1 := ifelse(choice_e, "T", "C"),
      e_2 := ifelse(choice_e, "C", "T")) %.>%
     select_columns(., a_1, a_2, b_1, b_2, c_1, c_2, d_1, d_2, e_1, e_2)

Notice `rquery::extend_se()` split the work into 3 unambiguous groups. The statements inside each group can now be executed in any order (or even in parallel) with no ambiguity of meaning or risk of error.

``` r
sql <- to_sql(plan, my_db)
cat(sql)
```

    SELECT
     `a_1`,
     `a_2`,
     `b_1`,
     `b_2`,
     `c_1`,
     `c_2`,
     `d_1`,
     `d_2`,
     `e_1`,
     `e_2`
    FROM (
     SELECT
      `choice_a`,
      `choice_b`,
      `choice_c`,
      `choice_d`,
      `choice_e`,
      ( CASE WHEN ( `choice_a` ) THEN ( 'T' ) ELSE ( 'C' ) END )  AS `a_1`,
      ( CASE WHEN ( `choice_a` ) THEN ( 'C' ) ELSE ( 'T' ) END )  AS `a_2`,
      ( CASE WHEN ( `choice_b` ) THEN ( 'T' ) ELSE ( 'C' ) END )  AS `b_1`,
      ( CASE WHEN ( `choice_b` ) THEN ( 'C' ) ELSE ( 'T' ) END )  AS `b_2`,
      ( CASE WHEN ( `choice_c` ) THEN ( 'T' ) ELSE ( 'C' ) END )  AS `c_1`,
      ( CASE WHEN ( `choice_c` ) THEN ( 'C' ) ELSE ( 'T' ) END )  AS `c_2`,
      ( CASE WHEN ( `choice_d` ) THEN ( 'T' ) ELSE ( 'C' ) END )  AS `d_1`,
      ( CASE WHEN ( `choice_d` ) THEN ( 'C' ) ELSE ( 'T' ) END )  AS `d_2`,
      ( CASE WHEN ( `choice_e` ) THEN ( 'T' ) ELSE ( 'C' ) END )  AS `e_1`,
      ( CASE WHEN ( `choice_e` ) THEN ( 'C' ) ELSE ( 'T' ) END )  AS `e_2`
     FROM (
      SELECT
       `rand_a`,
       `rand_b`,
       `rand_c`,
       `rand_d`,
       `rand_e`,
       `rand_a` >= 0.5  AS `choice_a`,
       `rand_b` >= 0.5  AS `choice_b`,
       `rand_c` >= 0.5  AS `choice_c`,
       `rand_d` >= 0.5  AS `choice_d`,
       `rand_e` >= 0.5  AS `choice_e`
      FROM (
       SELECT
        `example_table`.`rand_a`,
        `example_table`.`rand_b`,
        `example_table`.`rand_c`,
        `example_table`.`rand_d`,
        `example_table`.`rand_e`
       FROM
        `example_table`
       ) tsql_0000
      ) tsql_0001
    ) tsql_0002

``` r
DBI::dbGetQuery(my_db, sql) %.>%
  knitr::kable(.)
```

| a\_1 | a\_2 | b\_1 | b\_2 | c\_1 | c\_2 | d\_1 | d\_2 | e\_1 | e\_2 |
|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
| T    | C    | C    | T    | C    | T    | C    | T    | T    | C    |
| T    | C    | C    | T    | T    | C    | T    | C    | T    | C    |
| T    | C    | T    | C    | C    | T    | C    | T    | T    | C    |
| T    | C    | T    | C    | C    | T    | T    | C    | T    | C    |
| T    | C    | C    | T    | T    | C    | C    | T    | T    | C    |
| C    | T    | T    | C    | C    | T    | T    | C    | T    | C    |
| T    | C    | T    | C    | C    | T    | T    | C    | T    | C    |
| T    | C    | T    | C    | T    | C    | C    | T    | C    | T    |

A straightforward method (with no statement re-ordering) of splitting into non-dependent groups would have to split the mutate at each first use of a new value: yielding more mutate stages. For why a low number of execution stages is important please see [here](http://winvector.github.io/FluidData/partition_mutate.html).

Note: re-using variable variable names does limit the planner's ability to efficiently partition the the statement. The planner still emits safe and correct code, but unless it were to be allowed to introduce new variable names it must break sequences in more places. We show this effect below:

``` r
plan2 <- d1 %.>%
  extend_nse(.,
   choice = rand_a>=0.5, 
    a_1 = ifelse(choice, 
                  'T', 
                  'C'),
    a_2 = ifelse(choice, 
                  'C', 
                  'T'),
   choice = rand_b>=0.5, 
    b_1 = ifelse(choice, 
                  'T', 
                  'C'),
    b_2 = ifelse(choice, 
                  'C', 
                  'T'),
   choice = rand_c>=0.5, 
    c_1 = ifelse(choice, 
                  'T', 
                  'C'),
    c_2 = ifelse(choice, 
                  'C', 
                  'T'),
   choice = rand_d>=0.5, 
    d_1 = ifelse(choice, 
                  'T', 
                  'C'),
    d_2 = ifelse(choice, 
                  'C', 
                  'T'),
   choice = rand_e>=0.5, 
    e_1 = ifelse(choice, 
                  'T', 
                  'C'),
    e_2 = ifelse(choice, 
                  'C', 
                  'T')
  ) %.>%
  select_columns(., 
                 qc(a_1, a_2, b_1, b_2,
                    c_1, c_2, d_1, d_2,
                    e_1, e_2))

cat(format(plan2))
```

    table('example_table') %.>%
     extend(.,
      choice := rand_a >= 0.5) %.>%
     extend(.,
      a_1 := ifelse(choice, "T", "C"),
      a_2 := ifelse(choice, "C", "T")) %.>%
     extend(.,
      choice := rand_b >= 0.5) %.>%
     extend(.,
      b_1 := ifelse(choice, "T", "C"),
      b_2 := ifelse(choice, "C", "T")) %.>%
     extend(.,
      choice := rand_c >= 0.5) %.>%
     extend(.,
      c_1 := ifelse(choice, "T", "C"),
      c_2 := ifelse(choice, "C", "T")) %.>%
     extend(.,
      choice := rand_d >= 0.5) %.>%
     extend(.,
      d_1 := ifelse(choice, "T", "C"),
      d_2 := ifelse(choice, "C", "T")) %.>%
     extend(.,
      choice := rand_e >= 0.5) %.>%
     extend(.,
      e_1 := ifelse(choice, "T", "C"),
      e_2 := ifelse(choice, "C", "T")) %.>%
     select_columns(., a_1, a_2, b_1, b_2, c_1, c_2, d_1, d_2, e_1, e_2)

``` r
sql2 <- to_sql(plan2, my_db)
DBI::dbGetQuery(my_db, sql2) %.>%
  knitr::kable(.)
```

| a\_1 | a\_2 | b\_1 | b\_2 | c\_1 | c\_2 | d\_1 | d\_2 | e\_1 | e\_2 |
|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
| T    | C    | C    | T    | C    | T    | C    | T    | T    | C    |
| T    | C    | C    | T    | T    | C    | T    | C    | T    | C    |
| T    | C    | T    | C    | C    | T    | C    | T    | T    | C    |
| T    | C    | T    | C    | C    | T    | T    | C    | T    | C    |
| T    | C    | C    | T    | T    | C    | C    | T    | T    | C    |
| C    | T    | T    | C    | C    | T    | T    | C    | T    | C    |
| T    | C    | T    | C    | C    | T    | T    | C    | T    | C    |
| T    | C    | T    | C    | T    | C    | C    | T    | C    | T    |

Notice the returned tables are identical (as they should be).

The current (as of 2018-01-04) `CRAN` version of `dplyr` (even with the new `dbplyr` version `1.2.0`), however gives an *incorrect* result on databases. Notice the *exact same choice* is (incorrectly) used across all groups (`a` through `e`).

``` r
library("dplyr")
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
packageVersion("dplyr")
```

    ## [1] '0.7.4'

``` r
packageVersion("dbplyr")
```

    ## [1] '1.2.0'

``` r
dR <- dplyr::tbl(my_db, "example_table")

pipeline <- . %>%
  mutate(
   choice = rand_a>=0.5, 
    a_1 = ifelse(choice, 
                  'T', 
                  'C'),
    a_2 = ifelse(choice, 
                  'C', 
                  'T'),
   choice = rand_b>=0.5, 
    b_1 = ifelse(choice, 
                  'T', 
                  'C'),
    b_2 = ifelse(choice, 
                  'C', 
                  'T'),
   choice = rand_c>=0.5, 
    c_1 = ifelse(choice, 
                  'T', 
                  'C'),
    c_2 = ifelse(choice, 
                  'C', 
                  'T'),
   choice = rand_d>=0.5, 
    d_1 = ifelse(choice, 
                  'T', 
                  'C'),
    d_2 = ifelse(choice, 
                  'C', 
                  'T'),
   choice = rand_e>=0.5, 
    e_1 = ifelse(choice, 
                  'T', 
                  'C'),
    e_2 = ifelse(choice, 
                  'C', 
                  'T')
  ) %>%
  select(a_1, a_2, b_1, b_2,
         c_1, c_2, d_1, d_2,
         e_1, e_2)
 

dR %>% 
  pipeline %>%
  knitr::kable()
```

| a\_1 | a\_2 | b\_1 | b\_2 | c\_1 | c\_2 | d\_1 | d\_2 | e\_1 | e\_2 |
|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
| T    | C    | T    | C    | T    | C    | T    | C    | T    | C    |
| T    | C    | T    | C    | T    | C    | T    | C    | T    | C    |
| T    | C    | T    | C    | T    | C    | T    | C    | T    | C    |
| T    | C    | T    | C    | T    | C    | T    | C    | T    | C    |
| T    | C    | T    | C    | T    | C    | T    | C    | T    | C    |
| C    | T    | C    | T    | C    | T    | C    | T    | C    | T    |
| T    | C    | T    | C    | T    | C    | T    | C    | T    | C    |
| T    | C    | T    | C    | T    | C    | T    | C    | T    | C    |

In-memory `dplyr` appears to reproduce our original correct result.

``` r
d %>% 
  pipeline %>%
  knitr::kable()
```

| a\_1 | a\_2 | b\_1 | b\_2 | c\_1 | c\_2 | d\_1 | d\_2 | e\_1 | e\_2 |
|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
| T    | C    | C    | T    | C    | T    | C    | T    | T    | C    |
| T    | C    | C    | T    | T    | C    | T    | C    | T    | C    |
| T    | C    | T    | C    | C    | T    | C    | T    | T    | C    |
| T    | C    | T    | C    | C    | T    | T    | C    | T    | C    |
| T    | C    | C    | T    | T    | C    | C    | T    | T    | C    |
| C    | T    | T    | C    | C    | T    | T    | C    | T    | C    |
| T    | C    | T    | C    | C    | T    | T    | C    | T    | C    |
| T    | C    | T    | C    | T    | C    | C    | T    | C    | T    |

Inspecting the `dplyr` generated `SQL` shows what was in fact executed on the database. For some reason `choice` is repeatedly set to `rand_a >= 0.5` (never using the other specified groups).

``` r
dR %>% 
  pipeline %>%
  show_query()
```

    <SQL>
    SELECT `a_1`, `a_2`, `b_1`, `b_2`, `c_1`, `c_2`, `d_1`, `d_2`, `e_1`, `e_2`
    FROM (SELECT `id`, `rand_a`, `rand_b`, `rand_c`, `rand_d`, `rand_e`, `choice`, `a_1`, `a_2`, `b_1`, `b_2`, `c_1`, `c_2`, `d_1`, `d_2`, CASE WHEN (`choice`) THEN ('T') WHEN NOT(`choice`) THEN ('C') END AS `e_1`, CASE WHEN (`choice`) THEN ('C') WHEN NOT(`choice`) THEN ('T') END AS `e_2`
    FROM (SELECT `id`, `rand_a`, `rand_b`, `rand_c`, `rand_d`, `rand_e`, `rand_a` >= 0.5 AS `choice`, `a_1`, `a_2`, `b_1`, `b_2`, `c_1`, `c_2`, CASE WHEN (`choice`) THEN ('T') WHEN NOT(`choice`) THEN ('C') END AS `d_1`, CASE WHEN (`choice`) THEN ('C') WHEN NOT(`choice`) THEN ('T') END AS `d_2`
    FROM (SELECT `id`, `rand_a`, `rand_b`, `rand_c`, `rand_d`, `rand_e`, `rand_a` >= 0.5 AS `choice`, `a_1`, `a_2`, `b_1`, `b_2`, CASE WHEN (`choice`) THEN ('T') WHEN NOT(`choice`) THEN ('C') END AS `c_1`, CASE WHEN (`choice`) THEN ('C') WHEN NOT(`choice`) THEN ('T') END AS `c_2`
    FROM (SELECT `id`, `rand_a`, `rand_b`, `rand_c`, `rand_d`, `rand_e`, `rand_a` >= 0.5 AS `choice`, `a_1`, `a_2`, CASE WHEN (`choice`) THEN ('T') WHEN NOT(`choice`) THEN ('C') END AS `b_1`, CASE WHEN (`choice`) THEN ('C') WHEN NOT(`choice`) THEN ('T') END AS `b_2`
    FROM (SELECT `id`, `rand_a`, `rand_b`, `rand_c`, `rand_d`, `rand_e`, `rand_a` >= 0.5 AS `choice`, CASE WHEN (`choice`) THEN ('T') WHEN NOT(`choice`) THEN ('C') END AS `a_1`, CASE WHEN (`choice`) THEN ('C') WHEN NOT(`choice`) THEN ('T') END AS `a_2`
    FROM (SELECT `id`, `rand_a`, `rand_b`, `rand_c`, `rand_d`, `rand_e`, `rand_a` >= 0.5 AS `choice`
    FROM `example_table`))))))

``` r
DBI::dbDisconnect(my_db)
```
