Summary Example
================
John Mount, Win-Vector LLC
3/13/2018

``` r
library("rquery")
```

    ## Loading required package: wrapr

``` r
run_example <- function(db) {
  print("***********")
  print(db)
  d <- dbi_copy_to(db, "d",
                   data.frame(v = NA_real_,
                              w = 1,
                              x = c(rev(1:10), NA, NA),
                              y = c(NA, NA, sin(1:10)),
                              z = rev(letters[1:12]),
                              stringsAsFactors = FALSE),
                   temporary = TRUE,
                   overwrite = TRUE)
  
  optree <- d %.>%
    quantile_node(.) 
  print(optree)
  
  execute(db, optree) %.>%
    print(.)
  
  optrees <- d %.>%
    rsummary_node(., quartiles = TRUE) 
  print(optrees)
  
  execute(db, optrees) %.>%
    print(.)
  
  dbi_remove_table(db, "d")
}

db <- sparklyr::spark_connect(version='2.2.0', 
                                master = "local")
run_example(db)
```

    ## [1] "***********"
    ## $master
    ## [1] "local[4]"
    ## 
    ## $method
    ## [1] "shell"
    ## 
    ## $app_name
    ## [1] "sparklyr"
    ## 
    ## $config
    ## $config$spark.env.SPARK_LOCAL_IP.local
    ## [1] "127.0.0.1"
    ## 
    ## $config$sparklyr.csv.embedded
    ## [1] "^1.*"
    ## 
    ## $config$sparklyr.cores.local
    ## [1] 4
    ## 
    ## $config$spark.sql.shuffle.partitions.local
    ## [1] 4
    ## 
    ## attr(,"config")
    ## [1] "default"
    ## attr(,"file")
    ## [1] "/Library/Frameworks/R.framework/Versions/3.4/Resources/library/sparklyr/conf/config-template.yml"
    ## 
    ## $spark_home
    ## [1] "/Users/johnmount/spark/spark-2.2.0-bin-hadoop2.7"
    ## 
    ## $backend
    ## A connection with                               
    ## description "->localhost:52313"
    ## class       "sockconn"         
    ## mode        "wb"               
    ## text        "binary"           
    ## opened      "opened"           
    ## can read    "yes"              
    ## can write   "yes"              
    ## 
    ## $monitor
    ## A connection with                               
    ## description "->localhost:52310"
    ## class       "sockconn"         
    ## mode        "rb"               
    ## text        "binary"           
    ## opened      "opened"           
    ## can read    "yes"              
    ## can write   "yes"              
    ## 
    ## $output_file
    ## [1] "/var/folders/7q/h_jp2vj131g5799gfnpzhdp80000gn/T//Rtmp4RmSTG/file133a949fb8ee_spark.log"
    ## 
    ## $spark_context
    ## <jobj[7]>
    ##   org.apache.spark.SparkContext
    ##   org.apache.spark.SparkContext@1f1a7b2e
    ## 
    ## $java_context
    ## <jobj[8]>
    ##   org.apache.spark.api.java.JavaSparkContext
    ##   org.apache.spark.api.java.JavaSparkContext@127e55
    ## 
    ## attr(,"class")
    ## [1] "spark_connection"       "spark_shell_connection"
    ## [3] "DBIConnection"         
    ## [1] "table('d') %.>% non_sql_node(., quantile_node(., qin_59384997029121443357_0000000000, qout_93686542606907208240_0000000000))"
    ##   probs     v w  x          y z
    ## 1  0.00 FALSE 1  1 -0.9589243 a
    ## 2  0.25 FALSE 1  3 -0.5440211 c
    ## 3  0.50 FALSE 1  5  0.1411200 f
    ## 4  0.75 FALSE 1  8  0.8414710 i
    ## 5  1.00 FALSE 1 10  0.9893582 l
    ## [1] "table('d') %.>% non_sql_node(., rsummary_node(., rin_31808163576146474994_0000000000, rout_43147808544274825915_0000000000))"
    ##   column index     class nrows nna nunique        min        max      mean
    ## 1      v     1   numeric    12  12       0        NaN        NaN       NaN
    ## 2      w     2   numeric    12   0     NaN  1.0000000  1.0000000 1.0000000
    ## 3      x     3   integer    12   2     NaN  1.0000000 10.0000000 5.5000000
    ## 4      y     4   numeric    12   2     NaN -0.9589243  0.9893582 0.1411188
    ## 5      z     5 character    12   0      12        NaN        NaN       NaN
    ##          sd lexmin lexmax         Q1  median       Q3
    ## 1       NaN     NA     NA        NaN     NaN      NaN
    ## 2 0.0000000     NA     NA  1.0000000 1.00000 1.000000
    ## 3 3.0276504     NA     NA  3.0000000 5.00000 8.000000
    ## 4 0.7304706     NA     NA -0.5440211 0.14112 0.841471
    ## 5       NaN      a      l        NaN     NaN      NaN

    ## [1] 0

``` r
sparklyr::spark_disconnect(db)



db <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')
run_example(db)
```

    ## [1] "***********"
    ## <PostgreSQLConnection>
    ## [1] "table('d') %.>% non_sql_node(., quantile_node(., qin_44472513068635314670_0000000000, qout_19567394717705106244_0000000000))"
    ##   probs  v w  x          y z
    ## 1  0.00 NA 1  1 -0.9589243 a
    ## 2  0.25 NA 1  3 -0.5440211 c
    ## 3  0.50 NA 1  5  0.1411200 f
    ## 4  0.75 NA 1  8  0.8414710 i
    ## 5  1.00 NA 1 10  0.9893582 l
    ## [1] "table('d') %.>% non_sql_node(., rsummary_node(., rin_89301739172462436350_0000000000, rout_50200085935595930633_0000000000))"
    ##   column index     class nrows nna nunique        min        max      mean
    ## 1      v     1   numeric    12  12       0         NA         NA        NA
    ## 2      w     2   numeric    12   0      NA  1.0000000  1.0000000 1.0000000
    ## 3      x     3   integer    12   2      NA  1.0000000 10.0000000 5.5000000
    ## 4      y     4   numeric    12   2      NA -0.9589243  0.9893582 0.1411188
    ## 5      z     5 character    12   0      12         NA         NA        NA
    ##          sd lexmin lexmax         Q1  median       Q3
    ## 1        NA   <NA>   <NA>         NA      NA       NA
    ## 2 0.0000000   <NA>   <NA>  1.0000000 1.00000 1.000000
    ## 3 3.0276504   <NA>   <NA>  3.0000000 5.00000 8.000000
    ## 4 0.7304706   <NA>   <NA> -0.5440211 0.14112 0.841471
    ## 5        NA      a      l         NA      NA       NA

    ## [1] 0

``` r
DBI::dbDisconnect(db)
```

    ## [1] TRUE

``` r
db <- DBI::dbConnect(RPostgres::Postgres(),
                     host = 'localhost',
                     port = 5432,
                     user = 'johnmount',
                     password = '')
```

    ## Warning: multiple methods tables found for 'dbQuoteLiteral'

``` r
run_example(db)
```

    ## [1] "***********"
    ## <PqConnection> johnmount@localhost:5432
    ## [1] "table('d') %.>% non_sql_node(., quantile_node(., qin_53259857491590815270_0000000000, qout_65244103743863484273_0000000000))"
    ##   probs  v w  x         y z
    ## 1  0.00 NA 1  1 -0.958924 a
    ## 2  0.25 NA 1  3 -0.544021 c
    ## 3  0.50 NA 1  5  0.141120 f
    ## 4  0.75 NA 1  8  0.841471 i
    ## 5  1.00 NA 1 10  0.989358 l
    ## [1] "table('d') %.>% non_sql_node(., rsummary_node(., rin_13112075491964140955_0000000000, rout_33032872339545543398_0000000000))"
    ##   column index     class nrows nna nunique       min       max     mean
    ## 1      v     1   numeric    12  12       0        NA        NA       NA
    ## 2      w     2   numeric    12   0      NA  1.000000  1.000000 1.000000
    ## 3      x     3   integer    12   2      NA  1.000000 10.000000 5.500000
    ## 4      y     4   numeric    12   2      NA -0.958924  0.989358 0.141119
    ## 5      z     5 character    12   0      12        NA        NA       NA
    ##         sd lexmin lexmax        Q1  median       Q3
    ## 1       NA   <NA>   <NA>        NA      NA       NA
    ## 2 0.000000   <NA>   <NA>  1.000000 1.00000 1.000000
    ## 3 3.027650   <NA>   <NA>  3.000000 5.00000 8.000000
    ## 4 0.730471   <NA>   <NA> -0.544021 0.14112 0.841471
    ## 5       NA      a      l        NA      NA       NA

    ## [1] 0

``` r
DBI::dbDisconnect(db)
```
