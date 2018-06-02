data.table backend for rquery replot
================
John Mount, Win-Vector LLC
06/02/2018

Re-plot some of the measurements from [here](https://github.com/WinVector/rquery/blob/master/extras/data_table.md).

``` r
library("ggplot2")
# https://github.com/WinVector/rqdatatable
library("rqdatatable") # devtools::install.packages("WinVector/rqdatatable")
```

    ## Loading required package: rquery

    ## Loading required package: wrapr

``` r
all_timings <- readRDS("all_timings.RDS")
all_timings$seconds <- all_timings$time/1e9

summary_pipeline <- local_td(all_timings) %.>%
  project_nse(., groupby = c("nrows", "expr"), mean_time_seconds = mean(seconds)) %.>%
  orderby(., c("nrows", "expr"))
all_means <- as.data.frame(ex_data_table(summary_pipeline))
knitr::kable(all_means)
```

|  nrows| expr                          |  mean\_time\_seconds|
|------:|:------------------------------|--------------------:|
|  2e+00| rquery\_database\_round\_trip |            0.0703325|
|  2e+00| rquery\_data.table            |            0.0096857|
|  2e+00| data.table                    |            0.0030508|
|  2e+00| dplyr\_data\_frame            |            0.0135261|
|  2e+00| dplyr\_tbl                    |            0.0108139|
|  2e+00| dplyr\_database\_round\_trip  |            0.2047052|
|  2e+00| base\_r\_stats\_aggregate     |            0.0051542|
|  4e+00| rquery\_database\_round\_trip |            0.0878100|
|  4e+00| rquery\_data.table            |            0.0131219|
|  4e+00| data.table                    |            0.0023351|
|  4e+00| dplyr\_data\_frame            |            0.0126025|
|  4e+00| dplyr\_tbl                    |            0.0115099|
|  4e+00| dplyr\_database\_round\_trip  |            0.1821005|
|  4e+00| base\_r\_stats\_aggregate     |            0.0054051|
|  1e+01| rquery\_database\_round\_trip |            0.0744409|
|  1e+01| rquery\_data.table            |            0.0089338|
|  1e+01| data.table                    |            0.0026941|
|  1e+01| dplyr\_data\_frame            |            0.0099450|
|  1e+01| dplyr\_tbl                    |            0.0088911|
|  1e+01| dplyr\_database\_round\_trip  |            0.2053992|
|  1e+01| base\_r\_stats\_aggregate     |            0.0033595|
|  2e+01| rquery\_database\_round\_trip |            0.0832583|
|  2e+01| rquery\_data.table            |            0.0095478|
|  2e+01| data.table                    |            0.0041614|
|  2e+01| dplyr\_data\_frame            |            0.0133272|
|  2e+01| dplyr\_tbl                    |            0.0112278|
|  2e+01| dplyr\_database\_round\_trip  |            0.1873735|
|  2e+01| base\_r\_stats\_aggregate     |            0.0052560|
|  4e+01| rquery\_database\_round\_trip |            0.0858668|
|  4e+01| rquery\_data.table            |            0.0200639|
|  4e+01| data.table                    |            0.0039966|
|  4e+01| dplyr\_data\_frame            |            0.0136767|
|  4e+01| dplyr\_tbl                    |            0.0132299|
|  4e+01| dplyr\_database\_round\_trip  |            0.1823177|
|  4e+01| base\_r\_stats\_aggregate     |            0.0059831|
|  1e+02| rquery\_database\_round\_trip |            0.1197727|
|  1e+02| rquery\_data.table            |            0.0121455|
|  1e+02| data.table                    |            0.0029779|
|  1e+02| dplyr\_data\_frame            |            0.0138996|
|  1e+02| dplyr\_tbl                    |            0.0117382|
|  1e+02| dplyr\_database\_round\_trip  |            0.1859998|
|  1e+02| base\_r\_stats\_aggregate     |            0.0058052|
|  2e+02| rquery\_database\_round\_trip |            0.0815498|
|  2e+02| rquery\_data.table            |            0.0117515|
|  2e+02| data.table                    |            0.0026316|
|  2e+02| dplyr\_data\_frame            |            0.0139964|
|  2e+02| dplyr\_tbl                    |            0.0109423|
|  2e+02| dplyr\_database\_round\_trip  |            0.1885674|
|  2e+02| base\_r\_stats\_aggregate     |            0.0053333|
|  4e+02| rquery\_database\_round\_trip |            0.0764473|
|  4e+02| rquery\_data.table            |            0.0105428|
|  4e+02| data.table                    |            0.0030793|
|  4e+02| dplyr\_data\_frame            |            0.0163067|
|  4e+02| dplyr\_tbl                    |            0.0181373|
|  4e+02| dplyr\_database\_round\_trip  |            0.1911515|
|  4e+02| base\_r\_stats\_aggregate     |            0.0060530|
|  1e+03| rquery\_database\_round\_trip |            0.1129037|
|  1e+03| rquery\_data.table            |            0.0130051|
|  1e+03| data.table                    |            0.0058056|
|  1e+03| dplyr\_data\_frame            |            0.0318945|
|  1e+03| dplyr\_tbl                    |            0.0359882|
|  1e+03| dplyr\_database\_round\_trip  |            0.2040048|
|  1e+03| base\_r\_stats\_aggregate     |            0.0123077|
|  2e+03| rquery\_database\_round\_trip |            0.0951743|
|  2e+03| rquery\_data.table            |            0.0142652|
|  2e+03| data.table                    |            0.0063695|
|  2e+03| dplyr\_data\_frame            |            0.0701281|
|  2e+03| dplyr\_tbl                    |            0.0498358|
|  2e+03| dplyr\_database\_round\_trip  |            0.2033026|
|  2e+03| base\_r\_stats\_aggregate     |            0.0203487|
|  4e+03| rquery\_database\_round\_trip |            0.1475338|
|  4e+03| rquery\_data.table            |            0.0187106|
|  4e+03| data.table                    |            0.0100284|
|  4e+03| dplyr\_data\_frame            |            0.1188543|
|  4e+03| dplyr\_tbl                    |            0.0890460|
|  4e+03| dplyr\_database\_round\_trip  |            0.2429075|
|  4e+03| base\_r\_stats\_aggregate     |            0.0341621|
|  1e+04| rquery\_database\_round\_trip |            0.2140592|
|  1e+04| rquery\_data.table            |            0.0278973|
|  1e+04| data.table                    |            0.0212909|
|  1e+04| dplyr\_data\_frame            |            0.2362265|
|  1e+04| dplyr\_tbl                    |            0.2283054|
|  1e+04| dplyr\_database\_round\_trip  |            0.3239990|
|  1e+04| base\_r\_stats\_aggregate     |            0.1015219|
|  2e+04| rquery\_database\_round\_trip |            0.3499170|
|  2e+04| rquery\_data.table            |            0.0439004|
|  2e+04| data.table                    |            0.0410845|
|  2e+04| dplyr\_data\_frame            |            0.4343037|
|  2e+04| dplyr\_tbl                    |            0.3862575|
|  2e+04| dplyr\_database\_round\_trip  |            0.5041247|
|  2e+04| base\_r\_stats\_aggregate     |            0.1719530|
|  4e+04| rquery\_database\_round\_trip |            0.5611411|
|  4e+04| rquery\_data.table            |            0.0821118|
|  4e+04| data.table                    |            0.0777648|
|  4e+04| dplyr\_data\_frame            |            0.8703418|
|  4e+04| dplyr\_tbl                    |            0.7970521|
|  4e+04| dplyr\_database\_round\_trip  |            0.8726638|
|  4e+04| base\_r\_stats\_aggregate     |            0.4051961|
|  1e+05| rquery\_database\_round\_trip |            1.3340817|
|  1e+05| rquery\_data.table            |            0.1941148|
|  1e+05| data.table                    |            0.1979971|
|  1e+05| dplyr\_data\_frame            |            2.0322712|
|  1e+05| dplyr\_tbl                    |            2.0227748|
|  1e+05| dplyr\_database\_round\_trip  |            1.9124696|
|  1e+05| base\_r\_stats\_aggregate     |            1.1517713|
|  2e+05| rquery\_database\_round\_trip |            2.5995955|
|  2e+05| rquery\_data.table            |            0.3549038|
|  2e+05| data.table                    |            0.3465581|
|  2e+05| dplyr\_data\_frame            |            4.1943811|
|  2e+05| dplyr\_tbl                    |            4.1771784|
|  2e+05| dplyr\_database\_round\_trip  |            3.9360320|
|  2e+05| base\_r\_stats\_aggregate     |            2.6166716|
|  4e+05| rquery\_database\_round\_trip |            5.3581625|
|  4e+05| rquery\_data.table            |            0.6014588|
|  4e+05| data.table                    |            0.6365327|
|  4e+05| dplyr\_data\_frame            |            8.6649357|
|  4e+05| dplyr\_tbl                    |            8.4680607|
|  4e+05| dplyr\_database\_round\_trip  |            8.1813729|
|  4e+05| base\_r\_stats\_aggregate     |            4.6742564|
|  1e+06| rquery\_database\_round\_trip |           13.9203899|
|  1e+06| rquery\_data.table            |            1.4084696|
|  1e+06| data.table                    |            1.4632299|
|  1e+06| dplyr\_data\_frame            |           20.7676020|
|  1e+06| dplyr\_tbl                    |           20.0745484|
|  1e+06| dplyr\_database\_round\_trip  |           20.6013084|
|  1e+06| base\_r\_stats\_aggregate     |           10.8978805|
|  2e+06| rquery\_database\_round\_trip |           28.2170268|
|  2e+06| rquery\_data.table            |            2.8262867|
|  2e+06| data.table                    |            2.7987888|
|  2e+06| dplyr\_data\_frame            |           41.9015499|
|  2e+06| dplyr\_tbl                    |           39.9879179|
|  2e+06| dplyr\_database\_round\_trip  |           43.2652332|
|  2e+06| base\_r\_stats\_aggregate     |           24.4619783|
|  4e+06| rquery\_database\_round\_trip |           55.6668174|
|  4e+06| rquery\_data.table            |            5.1934182|
|  4e+06| data.table                    |            5.0682226|
|  4e+06| dplyr\_data\_frame            |           77.8899823|
|  4e+06| dplyr\_tbl                    |           77.6862649|
|  4e+06| dplyr\_database\_round\_trip  |           84.9865783|
|  4e+06| base\_r\_stats\_aggregate     |           46.7380972|
|  1e+07| rquery\_database\_round\_trip |          145.6941668|
|  1e+07| rquery\_data.table            |           12.2358251|
|  1e+07| data.table                    |           14.0780015|
|  1e+07| dplyr\_data\_frame            |          189.8347662|
|  1e+07| dplyr\_tbl                    |          187.5360824|
|  1e+07| dplyr\_database\_round\_trip  |          222.8468867|
|  1e+07| base\_r\_stats\_aggregate     |          125.0071230|

``` r
# get a shared lvel ordering
means <- all_means[all_means$nrows == max(all_means$nrows), , drop = FALSE]
means <- means[order(means$mean_time_seconds), , drop = FALSE]
levels <-  means$expr

# plot scatter plots for each experiment
for(target_size in sort(unique(all_timings$nrows))) {
  timings <- all_timings[all_timings$nrows == target_size, , drop = FALSE]
  timings$implementation <- factor(timings$expr, levels)
  cutpt <- median(timings$seconds[timings$expr=="base_r_stats_aggregate"])
  plt <- WVPlots::ScatterBoxPlotH(timings, 
                                  "seconds", "implementation", 
                                  paste0("task time in seconds by implementation\n(",
                                         timings$nrows[[1]], " row by ", timings$ncols[[1]], " column task)")) +
    geom_hline(yintercept = cutpt, linetype=2, alpha = 0.5) 
  print(plt)
}
```

![](data_table_replot_files/figure-markdown_github/presenttimings-1.png)![](data_table_replot_files/figure-markdown_github/presenttimings-2.png)![](data_table_replot_files/figure-markdown_github/presenttimings-3.png)![](data_table_replot_files/figure-markdown_github/presenttimings-4.png)![](data_table_replot_files/figure-markdown_github/presenttimings-5.png)![](data_table_replot_files/figure-markdown_github/presenttimings-6.png)![](data_table_replot_files/figure-markdown_github/presenttimings-7.png)![](data_table_replot_files/figure-markdown_github/presenttimings-8.png)![](data_table_replot_files/figure-markdown_github/presenttimings-9.png)![](data_table_replot_files/figure-markdown_github/presenttimings-10.png)![](data_table_replot_files/figure-markdown_github/presenttimings-11.png)![](data_table_replot_files/figure-markdown_github/presenttimings-12.png)![](data_table_replot_files/figure-markdown_github/presenttimings-13.png)![](data_table_replot_files/figure-markdown_github/presenttimings-14.png)![](data_table_replot_files/figure-markdown_github/presenttimings-15.png)![](data_table_replot_files/figure-markdown_github/presenttimings-16.png)![](data_table_replot_files/figure-markdown_github/presenttimings-17.png)![](data_table_replot_files/figure-markdown_github/presenttimings-18.png)![](data_table_replot_files/figure-markdown_github/presenttimings-19.png)![](data_table_replot_files/figure-markdown_github/presenttimings-20.png)![](data_table_replot_files/figure-markdown_github/presenttimings-21.png)

``` r
# plot as a function of problem size
cmap <- 
  c(dplyr_data_frame = "#a63603",
    dplyr_tbl = "#e6550d",
    dplyr_database_round_trip = "#fd8d3c",
    data.table = "#7851a9",
    rquery_database_round_trip = "#31a354",
    rquery_data.table = "#006d2c")
lmap <- c(dplyr_data_frame = 1,
          dplyr_tbl = 3,
          dplyr_database_round_trip = 4,
          data.table = 1,
          rquery_database_round_trip = 4,
          rquery_data.table = 2)
all_timings$implementation <- factor(all_timings$expr, rev(levels))
ex1 <- all_timings[all_timings$expr != 'base_r_stats_aggregate', , drop = FALSE]
exb <- all_timings[all_timings$expr == 'base_r_stats_aggregate', , drop = FALSE]
sm <- loess(seconds ~ nrows, data= exb)
smf <- data.frame(nrows = exp(seq(log(min(ex1$nrows)), log(max(ex1$nrows)), length.out = 100)))
smf$seconds <- predict(sm, newdata=smf)
ymin = min(all_timings$seconds)
ggplot(mapping = aes(x = nrows, y = seconds, ymax = seconds, ymin = ymin)) +
  geom_ribbon(data = smf, alpha = 0.3) +
  geom_line(data = ex1, se = FALSE, aes(color = implementation, linetype = implementation), 
            stat = "smooth", method = "loess", alpha = 0.7, size = 1) + 
  scale_x_log10() + scale_y_log10() +
  scale_color_manual(values = cmap) +
  scale_linetype_manual(values = lmap) +
  ggtitle("task time in seconds by nrows and implementation",
          subtitle = "shading boundary time taken by base R stats::aggregate() solution")
```

![](data_table_replot_files/figure-markdown_github/presenttimings-22.png)

``` r
# look at slopes
summaries <- split(all_timings, all_timings$expr) %.>%
  lapply(., 
         function(gi) {
           model <- lm(seconds ~ nrows, data= gi)
           si <- as.data.frame(summary(model)$coefficients)
           si$coef <- rownames(si)
           si$impementation <- as.character(gi$expr[[1]])
           si
         }) %.>%
  data.table::rbindlist(.)
colnames(summaries) <- gsub("Pr(>|t|)", "P[g.t. abs(t)]", colnames(summaries), fixed = TRUE)
knitr::kable(summaries)
```

|    Estimate|  Std. Error|      t value|  P\[g.t. abs(t)\]| coef        | impementation                 |
|-----------:|-----------:|------------:|-----------------:|:------------|:------------------------------|
|  -0.1155771|   0.1786283|   -0.6470254|         0.5190549| (Intercept) | rquery\_database\_round\_trip |
|   0.0000145|   0.0000001|  194.7180823|         0.0000000| nrows       | rquery\_database\_round\_trip |
|   0.0561448|   0.0265336|    2.1159877|         0.0367562| (Intercept) | rquery\_data.table            |
|   0.0000012|   0.0000000|  111.6993510|         0.0000000| nrows       | rquery\_data.table            |
|   0.0037459|   0.0409711|    0.0914270|         0.9273308| (Intercept) | data.table                    |
|   0.0000014|   0.0000000|   81.4646731|         0.0000000| nrows       | data.table                    |
|   0.3822043|   0.2450147|    1.5599239|         0.1218453| (Intercept) | dplyr\_data\_frame            |
|   0.0000191|   0.0000001|  187.0500347|         0.0000000| nrows       | dplyr\_data\_frame            |
|   0.3086954|   0.1257861|    2.4541285|         0.0157997| (Intercept) | dplyr\_tbl                    |
|   0.0000189|   0.0000001|  360.0779267|         0.0000000| nrows       | dplyr\_tbl                    |
|  -0.1868047|   0.1928281|   -0.9687629|         0.3349328| (Intercept) | dplyr\_database\_round\_trip  |
|   0.0000221|   0.0000001|  275.8004412|         0.0000000| nrows       | dplyr\_database\_round\_trip  |
|  -0.1942426|   0.1344143|   -1.4451035|         0.1514644| (Intercept) | base\_r\_stats\_aggregate     |
|   0.0000124|   0.0000001|  221.6057649|         0.0000000| nrows       | base\_r\_stats\_aggregate     |
