join\_example
================

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
library("rqdatatable")
```

    ## Loading required package: rquery

    ## Loading required package: wrapr

    ## 
    ## Attaching package: 'wrapr'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     coalesce

``` r
library("microbenchmark")

batting <- Lahman::Batting


players <- data.frame(playerID = sort(unique(c(batting$playerID, paste0("np_", seq_len(1000000))))),
                      stringsAsFactors = FALSE)
players$player_name_rank <- seq_len(nrow(players))
for(i in 1:20) {
  players[[paste0("pd_", i)]] <- runif(nrow(players))
}
# try a large example (where delays hurt)
batting <- data.table::rbindlist(rep(list(batting), 10))
# get a cannonical order for columns
cols <- unique(c(colnames(batting), colnames(players)))


# Example dplyr pipeline
fn_dplyr <- function() {
  left_join(batting, players, by = "playerID")
}

system.time(
  res1 <- fn_dplyr()
)
```

    ##    user  system elapsed 
    ##   1.214   0.153   1.387

``` r
res1 <- res1 %>%
  select(!!!cols) %>%
  arrange(playerID, yearID, stint, teamID)

  
# translation of above example into an rquery pipeline
fn_rquery <- function() {
  rq_pipeline <- natural_join(local_td(batting), local_td(players), 
                            by = "playerID",
                            jointype = "LEFT")
  ex_data_table(rq_pipeline)
}

system.time({
  res2 <- fn_rquery()
})
```

    ##    user  system elapsed 
    ##   1.449   0.215   1.227

``` r
oderq <- local_td(res2) %.>% 
  select_columns(., cols) %.>% 
  orderby(., qc(playerID, yearID, stint, teamID))
res2 <- ex_data_table(oderq)


assertthat::are_equal(as.data.frame(res1),
                      as.data.frame(res2))
```

    ## [1] TRUE

``` r
microbenchmark(
  dplyr = nrow(fn_dplyr()),
  rquery = nrow(fn_rquery())
)
```

    ## Unit: milliseconds
    ##    expr      min       lq     mean   median       uq      max neval
    ##   dplyr 985.4968 1032.250 1132.759 1079.941 1193.747 2159.046   100
    ##  rquery 974.2718 1040.978 1143.286 1127.294 1223.117 2129.621   100
