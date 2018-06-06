data.table backend for rquery replot
================
John Mount, Win-Vector LLC
06/05/2018

Re-plot some of the measurements from [here](https://github.com/WinVector/rquery/blob/master/extras/data_table.md).

``` r
library("ggplot2")
# https://github.com/WinVector/rqdatatable
library("rqdatatable") # devtools::install.packages("WinVector/rqdatatable")
```

    ## Loading required package: rquery

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
|  2e+00| rquery\_database\_round\_trip |            0.0735940|
|  2e+00| rquery\_data.table            |            0.0088502|
|  2e+00| data.table                    |            0.0026631|
|  2e+00| dplyr\_data\_frame            |            0.0097414|
|  2e+00| dplyr\_tbl                    |            0.0100797|
|  2e+00| dplyr\_database\_round\_trip  |            0.2004109|
|  2e+00| base\_r\_stats\_aggregate     |            0.0036763|
|  4e+00| rquery\_database\_round\_trip |            0.1052138|
|  4e+00| rquery\_data.table            |            0.0107554|
|  4e+00| data.table                    |            0.0055235|
|  4e+00| dplyr\_data\_frame            |            0.0103754|
|  4e+00| dplyr\_tbl                    |            0.0093522|
|  4e+00| dplyr\_database\_round\_trip  |            0.2005218|
|  4e+00| base\_r\_stats\_aggregate     |            0.0040211|
|  1e+01| rquery\_database\_round\_trip |            0.0821814|
|  1e+01| rquery\_data.table            |            0.0119303|
|  1e+01| data.table                    |            0.0044524|
|  1e+01| dplyr\_data\_frame            |            0.0115410|
|  1e+01| dplyr\_tbl                    |            0.0227645|
|  1e+01| dplyr\_database\_round\_trip  |            0.2347240|
|  1e+01| base\_r\_stats\_aggregate     |            0.0043242|
|  2e+01| rquery\_database\_round\_trip |            0.0957816|
|  2e+01| rquery\_data.table            |            0.0082644|
|  2e+01| data.table                    |            0.0052425|
|  2e+01| dplyr\_data\_frame            |            0.0364902|
|  2e+01| dplyr\_tbl                    |            0.0181596|
|  2e+01| dplyr\_database\_round\_trip  |            0.2006642|
|  2e+01| base\_r\_stats\_aggregate     |            0.0045568|
|  4e+01| rquery\_database\_round\_trip |            0.0788114|
|  4e+01| rquery\_data.table            |            0.0075416|
|  4e+01| data.table                    |            0.0032363|
|  4e+01| dplyr\_data\_frame            |            0.0127650|
|  4e+01| dplyr\_tbl                    |            0.0120887|
|  4e+01| dplyr\_database\_round\_trip  |            0.1696972|
|  4e+01| base\_r\_stats\_aggregate     |            0.0041775|
|  1e+02| rquery\_database\_round\_trip |            0.0872256|
|  1e+02| rquery\_data.table            |            0.0071304|
|  1e+02| data.table                    |            0.0029328|
|  1e+02| dplyr\_data\_frame            |            0.0110497|
|  1e+02| dplyr\_tbl                    |            0.0116840|
|  1e+02| dplyr\_database\_round\_trip  |            0.1686268|
|  1e+02| base\_r\_stats\_aggregate     |            0.0049990|
|  2e+02| rquery\_database\_round\_trip |            0.0737478|
|  2e+02| rquery\_data.table            |            0.0111590|
|  2e+02| data.table                    |            0.0034280|
|  2e+02| dplyr\_data\_frame            |            0.0137958|
|  2e+02| dplyr\_tbl                    |            0.0129798|
|  2e+02| dplyr\_database\_round\_trip  |            0.1807050|
|  2e+02| base\_r\_stats\_aggregate     |            0.0054096|
|  4e+02| rquery\_database\_round\_trip |            0.0755233|
|  4e+02| rquery\_data.table            |            0.0110641|
|  4e+02| data.table                    |            0.0040219|
|  4e+02| dplyr\_data\_frame            |            0.0177309|
|  4e+02| dplyr\_tbl                    |            0.0198690|
|  4e+02| dplyr\_database\_round\_trip  |            0.1878072|
|  4e+02| base\_r\_stats\_aggregate     |            0.0071780|
|  1e+03| rquery\_database\_round\_trip |            0.0969069|
|  1e+03| rquery\_data.table            |            0.0117654|
|  1e+03| data.table                    |            0.0049791|
|  1e+03| dplyr\_data\_frame            |            0.0281672|
|  1e+03| dplyr\_tbl                    |            0.0324365|
|  1e+03| dplyr\_database\_round\_trip  |            0.1847616|
|  1e+03| base\_r\_stats\_aggregate     |            0.0105049|
|  2e+03| rquery\_database\_round\_trip |            0.1058461|
|  2e+03| rquery\_data.table            |            0.0112331|
|  2e+03| data.table                    |            0.0067975|
|  2e+03| dplyr\_data\_frame            |            0.0628235|
|  2e+03| dplyr\_tbl                    |            0.0484870|
|  2e+03| dplyr\_database\_round\_trip  |            0.1945843|
|  2e+03| base\_r\_stats\_aggregate     |            0.0194684|
|  4e+03| rquery\_database\_round\_trip |            0.1487440|
|  4e+03| rquery\_data.table            |            0.0129495|
|  4e+03| data.table                    |            0.0126866|
|  4e+03| dplyr\_data\_frame            |            0.1039042|
|  4e+03| dplyr\_tbl                    |            0.0851695|
|  4e+03| dplyr\_database\_round\_trip  |            0.2127242|
|  4e+03| base\_r\_stats\_aggregate     |            0.0339421|
|  1e+04| rquery\_database\_round\_trip |            0.2189354|
|  1e+04| rquery\_data.table            |            0.0208661|
|  1e+04| data.table                    |            0.0203851|
|  1e+04| dplyr\_data\_frame            |            0.2021059|
|  1e+04| dplyr\_tbl                    |            0.2188574|
|  1e+04| dplyr\_database\_round\_trip  |            0.3059211|
|  1e+04| base\_r\_stats\_aggregate     |            0.1009194|
|  2e+04| rquery\_database\_round\_trip |            0.3626423|
|  2e+04| rquery\_data.table            |            0.0316752|
|  2e+04| data.table                    |            0.0412680|
|  2e+04| dplyr\_data\_frame            |            0.4298286|
|  2e+04| dplyr\_tbl                    |            0.3740222|
|  2e+04| dplyr\_database\_round\_trip  |            0.5177351|
|  2e+04| base\_r\_stats\_aggregate     |            0.1728017|
|  4e+04| rquery\_database\_round\_trip |            0.6235583|
|  4e+04| rquery\_data.table            |            0.0555413|
|  4e+04| data.table                    |            0.0763822|
|  4e+04| dplyr\_data\_frame            |            0.9089249|
|  4e+04| dplyr\_tbl                    |            0.8362493|
|  4e+04| dplyr\_database\_round\_trip  |            0.8667807|
|  4e+04| base\_r\_stats\_aggregate     |            0.4180211|
|  1e+05| rquery\_database\_round\_trip |            1.3153172|
|  1e+05| rquery\_data.table            |            0.1685633|
|  1e+05| data.table                    |            0.2126016|
|  1e+05| dplyr\_data\_frame            |            1.8673461|
|  1e+05| dplyr\_tbl                    |            1.9121245|
|  1e+05| dplyr\_database\_round\_trip  |            1.8623119|
|  1e+05| base\_r\_stats\_aggregate     |            1.1056743|
|  2e+05| rquery\_database\_round\_trip |            2.5527376|
|  2e+05| rquery\_data.table            |            0.2663358|
|  2e+05| data.table                    |            0.3574519|
|  2e+05| dplyr\_data\_frame            |            3.8803727|
|  2e+05| dplyr\_tbl                    |            3.7922792|
|  2e+05| dplyr\_database\_round\_trip  |            3.8047415|
|  2e+05| base\_r\_stats\_aggregate     |            2.3595769|
|  4e+05| rquery\_database\_round\_trip |            5.3362696|
|  4e+05| rquery\_data.table            |            0.4618315|
|  4e+05| data.table                    |            0.6136022|
|  4e+05| dplyr\_data\_frame            |            8.5200641|
|  4e+05| dplyr\_tbl                    |            7.9428823|
|  4e+05| dplyr\_database\_round\_trip  |            7.6183346|
|  4e+05| base\_r\_stats\_aggregate     |            4.5755169|
|  1e+06| rquery\_database\_round\_trip |           13.3413013|
|  1e+06| rquery\_data.table            |            0.9975369|
|  1e+06| data.table                    |            1.4659730|
|  1e+06| dplyr\_data\_frame            |           18.4520479|
|  1e+06| dplyr\_tbl                    |           19.4442478|
|  1e+06| dplyr\_database\_round\_trip  |           19.3586955|
|  1e+06| base\_r\_stats\_aggregate     |           10.5433614|
|  2e+06| rquery\_database\_round\_trip |           27.6402608|
|  2e+06| rquery\_data.table            |            2.0972969|
|  2e+06| data.table                    |            2.6446177|
|  2e+06| dplyr\_data\_frame            |           39.5308118|
|  2e+06| dplyr\_tbl                    |           39.6343081|
|  2e+06| dplyr\_database\_round\_trip  |           41.5584997|
|  2e+06| base\_r\_stats\_aggregate     |           24.1842380|
|  4e+06| rquery\_database\_round\_trip |           54.0779727|
|  4e+06| rquery\_data.table            |            3.9097193|
|  4e+06| data.table                    |            5.1956744|
|  4e+06| dplyr\_data\_frame            |           74.5310565|
|  4e+06| dplyr\_tbl                    |           75.4946504|
|  4e+06| dplyr\_database\_round\_trip  |           82.2158456|
|  4e+06| base\_r\_stats\_aggregate     |           45.4672270|
|  1e+07| rquery\_database\_round\_trip |          137.0005252|
|  1e+07| rquery\_data.table            |            9.4190169|
|  1e+07| data.table                    |           13.0897663|
|  1e+07| dplyr\_data\_frame            |          183.6192758|
|  1e+07| dplyr\_tbl                    |          176.8674611|
|  1e+07| dplyr\_database\_round\_trip  |          221.3450944|
|  1e+07| base\_r\_stats\_aggregate     |          117.8542225|

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
|   0.0193731|   0.1490505|    0.1299768|         0.8968383| (Intercept) | rquery\_database\_round\_trip |
|   0.0000137|   0.0000001|  220.4006504|         0.0000000| nrows       | rquery\_database\_round\_trip |
|   0.0344385|   0.0165737|    2.0779073|         0.0402043| (Intercept) | rquery\_data.table            |
|   0.0000009|   0.0000000|  137.1315448|         0.0000000| nrows       | rquery\_data.table            |
|   0.0264378|   0.0328785|    0.8041084|         0.4231864| (Intercept) | data.table                    |
|   0.0000013|   0.0000000|   95.4351770|         0.0000000| nrows       | data.table                    |
|   0.2325424|   0.4583607|    0.5073350|         0.6130044| (Intercept) | dplyr\_data\_frame            |
|   0.0000184|   0.0000002|   96.5220451|         0.0000000| nrows       | dplyr\_data\_frame            |
|   0.4357641|   0.2709463|    1.6083043|         0.1108296| (Intercept) | dplyr\_tbl                    |
|   0.0000179|   0.0000001|  158.4347297|         0.0000000| nrows       | dplyr\_tbl                    |
|  -0.3755020|   0.2144341|   -1.7511296|         0.0829006| (Intercept) | dplyr\_database\_round\_trip  |
|   0.0000219|   0.0000001|  245.4546742|         0.0000000| nrows       | dplyr\_database\_round\_trip  |
|  -0.0891950|   0.1246918|   -0.7153239|         0.4760273| (Intercept) | base\_r\_stats\_aggregate     |
|   0.0000117|   0.0000001|  226.2502197|         0.0000000| nrows       | base\_r\_stats\_aggregate     |
