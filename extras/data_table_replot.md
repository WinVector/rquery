data.table backend for rquery replot
================
John Mount, Win-Vector LLC
06/06/2018

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
|  2e+00| rquery\_database\_round\_trip |            0.0544305|
|  2e+00| rquery\_data.table            |            0.0077846|
|  2e+00| data.table                    |            0.0025338|
|  2e+00| dplyr\_data\_frame            |            0.0102417|
|  2e+00| dplyr\_tbl                    |            0.0079551|
|  2e+00| dplyr\_database\_round\_trip  |            0.1734428|
|  2e+00| base\_r\_stats\_aggregate     |            0.0040589|
|  4e+00| rquery\_database\_round\_trip |            0.0489115|
|  4e+00| rquery\_data.table            |            0.0085008|
|  4e+00| data.table                    |            0.0028600|
|  4e+00| dplyr\_data\_frame            |            0.0101816|
|  4e+00| dplyr\_tbl                    |            0.0113961|
|  4e+00| dplyr\_database\_round\_trip  |            0.1774541|
|  4e+00| base\_r\_stats\_aggregate     |            0.0044127|
|  1e+01| rquery\_database\_round\_trip |            0.0544821|
|  1e+01| rquery\_data.table            |            0.0084185|
|  1e+01| data.table                    |            0.0033111|
|  1e+01| dplyr\_data\_frame            |            0.0116332|
|  1e+01| dplyr\_tbl                    |            0.0111590|
|  1e+01| dplyr\_database\_round\_trip  |            0.2067033|
|  1e+01| base\_r\_stats\_aggregate     |            0.0049851|
|  2e+01| rquery\_database\_round\_trip |            0.0683689|
|  2e+01| rquery\_data.table            |            0.0090616|
|  2e+01| data.table                    |            0.0027719|
|  2e+01| dplyr\_data\_frame            |            0.0104407|
|  2e+01| dplyr\_tbl                    |            0.0091254|
|  2e+01| dplyr\_database\_round\_trip  |            0.1656487|
|  2e+01| base\_r\_stats\_aggregate     |            0.0037504|
|  4e+01| rquery\_database\_round\_trip |            0.0623084|
|  4e+01| rquery\_data.table            |            0.0081068|
|  4e+01| data.table                    |            0.0030216|
|  4e+01| dplyr\_data\_frame            |            0.0106211|
|  4e+01| dplyr\_tbl                    |            0.0093909|
|  4e+01| dplyr\_database\_round\_trip  |            0.1951139|
|  4e+01| base\_r\_stats\_aggregate     |            0.0047196|
|  1e+02| rquery\_database\_round\_trip |            0.0580286|
|  1e+02| rquery\_data.table            |            0.0081105|
|  1e+02| data.table                    |            0.0030966|
|  1e+02| dplyr\_data\_frame            |            0.0156022|
|  1e+02| dplyr\_tbl                    |            0.0136052|
|  1e+02| dplyr\_database\_round\_trip  |            0.1921026|
|  1e+02| base\_r\_stats\_aggregate     |            0.0049411|
|  2e+02| rquery\_database\_round\_trip |            0.0504702|
|  2e+02| rquery\_data.table            |            0.0082312|
|  2e+02| data.table                    |            0.0029331|
|  2e+02| dplyr\_data\_frame            |            0.0170942|
|  2e+02| dplyr\_tbl                    |            0.0133551|
|  2e+02| dplyr\_database\_round\_trip  |            0.1627279|
|  2e+02| base\_r\_stats\_aggregate     |            0.0051107|
|  4e+02| rquery\_database\_round\_trip |            0.0923261|
|  4e+02| rquery\_data.table            |            0.0080434|
|  4e+02| data.table                    |            0.0050171|
|  4e+02| dplyr\_data\_frame            |            0.0200114|
|  4e+02| dplyr\_tbl                    |            0.0203912|
|  4e+02| dplyr\_database\_round\_trip  |            0.1906036|
|  4e+02| base\_r\_stats\_aggregate     |            0.0079933|
|  1e+03| rquery\_database\_round\_trip |            0.0847101|
|  1e+03| rquery\_data.table            |            0.0108609|
|  1e+03| data.table                    |            0.0065159|
|  1e+03| dplyr\_data\_frame            |            0.0434853|
|  1e+03| dplyr\_tbl                    |            0.0374161|
|  1e+03| dplyr\_database\_round\_trip  |            0.2141453|
|  1e+03| base\_r\_stats\_aggregate     |            0.0149427|
|  2e+03| rquery\_database\_round\_trip |            0.0913010|
|  2e+03| rquery\_data.table            |            0.0146866|
|  2e+03| data.table                    |            0.0072479|
|  2e+03| dplyr\_data\_frame            |            0.0662970|
|  2e+03| dplyr\_tbl                    |            0.0592858|
|  2e+03| dplyr\_database\_round\_trip  |            0.2498846|
|  2e+03| base\_r\_stats\_aggregate     |            0.0185800|
|  4e+03| rquery\_database\_round\_trip |            0.1196270|
|  4e+03| rquery\_data.table            |            0.0167533|
|  4e+03| data.table                    |            0.0250085|
|  4e+03| dplyr\_data\_frame            |            0.1263373|
|  4e+03| dplyr\_tbl                    |            0.1047428|
|  4e+03| dplyr\_database\_round\_trip  |            0.2766276|
|  4e+03| base\_r\_stats\_aggregate     |            0.0384374|
|  1e+04| rquery\_database\_round\_trip |            0.1887802|
|  1e+04| rquery\_data.table            |            0.0195267|
|  1e+04| data.table                    |            0.0220937|
|  1e+04| dplyr\_data\_frame            |            0.2286047|
|  1e+04| dplyr\_tbl                    |            0.2605140|
|  1e+04| dplyr\_database\_round\_trip  |            0.3359768|
|  1e+04| base\_r\_stats\_aggregate     |            0.0926682|
|  2e+04| rquery\_database\_round\_trip |            0.3312724|
|  2e+04| rquery\_data.table            |            0.0347890|
|  2e+04| data.table                    |            0.0411335|
|  2e+04| dplyr\_data\_frame            |            0.4420988|
|  2e+04| dplyr\_tbl                    |            0.4521559|
|  2e+04| dplyr\_database\_round\_trip  |            0.5325640|
|  2e+04| base\_r\_stats\_aggregate     |            0.1647641|
|  4e+04| rquery\_database\_round\_trip |            0.5790534|
|  4e+04| rquery\_data.table            |            0.0604554|
|  4e+04| data.table                    |            0.0847904|
|  4e+04| dplyr\_data\_frame            |            0.8502205|
|  4e+04| dplyr\_tbl                    |            0.8477920|
|  4e+04| dplyr\_database\_round\_trip  |            0.9056985|
|  4e+04| base\_r\_stats\_aggregate     |            0.3949479|
|  1e+05| rquery\_database\_round\_trip |            1.4948821|
|  1e+05| rquery\_data.table            |            0.1600433|
|  1e+05| data.table                    |            0.2044758|
|  1e+05| dplyr\_data\_frame            |            2.1908895|
|  1e+05| dplyr\_tbl                    |            2.0716274|
|  1e+05| dplyr\_database\_round\_trip  |            2.2482865|
|  1e+05| base\_r\_stats\_aggregate     |            1.3015021|
|  2e+05| rquery\_database\_round\_trip |            2.5980946|
|  2e+05| rquery\_data.table            |            0.2786324|
|  2e+05| data.table                    |            0.3507589|
|  2e+05| dplyr\_data\_frame            |            4.6025374|
|  2e+05| dplyr\_tbl                    |            4.0179720|
|  2e+05| dplyr\_database\_round\_trip  |            4.3119449|
|  2e+05| base\_r\_stats\_aggregate     |            2.5779963|
|  4e+05| rquery\_database\_round\_trip |            5.6701757|
|  4e+05| rquery\_data.table            |            0.5033231|
|  4e+05| data.table                    |            0.6106021|
|  4e+05| dplyr\_data\_frame            |            8.6769992|
|  4e+05| dplyr\_tbl                    |            8.7002172|
|  4e+05| dplyr\_database\_round\_trip  |            8.3569160|
|  4e+05| base\_r\_stats\_aggregate     |            4.4617937|
|  1e+06| rquery\_database\_round\_trip |           13.8366887|
|  1e+06| rquery\_data.table            |            1.0137469|
|  1e+06| data.table                    |            1.5689056|
|  1e+06| dplyr\_data\_frame            |           21.4911658|
|  1e+06| dplyr\_tbl                    |           21.9845446|
|  1e+06| dplyr\_database\_round\_trip  |           21.7432639|
|  1e+06| base\_r\_stats\_aggregate     |           12.4321457|
|  2e+06| rquery\_database\_round\_trip |           25.9233928|
|  2e+06| rquery\_data.table            |            2.1941419|
|  2e+06| data.table                    |            2.6639513|
|  2e+06| dplyr\_data\_frame            |           38.1560471|
|  2e+06| dplyr\_tbl                    |           40.7591322|
|  2e+06| dplyr\_database\_round\_trip  |           40.9970053|
|  2e+06| base\_r\_stats\_aggregate     |           22.6237078|
|  4e+06| rquery\_database\_round\_trip |           54.7535797|
|  4e+06| rquery\_data.table            |            4.3209810|
|  4e+06| data.table                    |            5.4655644|
|  4e+06| dplyr\_data\_frame            |           92.4387709|
|  4e+06| dplyr\_tbl                    |           76.6970000|
|  4e+06| dplyr\_database\_round\_trip  |           88.3057586|
|  4e+06| base\_r\_stats\_aggregate     |           49.9654796|
|  1e+07| rquery\_database\_round\_trip |          149.1899965|
|  1e+07| rquery\_data.table            |            9.4738980|
|  1e+07| data.table                    |           13.4880605|
|  1e+07| dplyr\_data\_frame            |          192.8889550|
|  1e+07| dplyr\_tbl                    |          192.4934256|
|  1e+07| dplyr\_database\_round\_trip  |          212.4090573|
|  1e+07| base\_r\_stats\_aggregate     |          130.9217071|

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
|  -0.3073147|   0.4215335|   -0.7290399|         0.4676318| (Intercept) | rquery\_database\_round\_trip |
|   0.0000147|   0.0000002|   83.9326609|         0.0000000| nrows       | rquery\_database\_round\_trip |
|   0.0492167|   0.0254448|    1.9342523|         0.0558260| (Intercept) | rquery\_data.table            |
|   0.0000010|   0.0000000|   91.0036792|         0.0000000| nrows       | rquery\_data.table            |
|   0.0279674|   0.0534897|    0.5228553|         0.6021978| (Intercept) | data.table                    |
|   0.0000013|   0.0000000|   60.5782771|         0.0000000| nrows       | data.table                    |
|   0.5516532|   0.7190134|    0.7672363|         0.4446962| (Intercept) | dplyr\_data\_frame            |
|   0.0000197|   0.0000003|   65.9194963|         0.0000000| nrows       | dplyr\_data\_frame            |
|   0.2945281|   0.5191300|    0.5673495|         0.5717113| (Intercept) | dplyr\_tbl                    |
|   0.0000193|   0.0000002|   89.1332868|         0.0000000| nrows       | dplyr\_tbl                    |
|   0.1706909|   0.3416773|    0.4995675|         0.6184452| (Intercept) | dplyr\_database\_round\_trip  |
|   0.0000213|   0.0000001|  149.8101768|         0.0000000| nrows       | dplyr\_database\_round\_trip  |
|  -0.2773955|   0.5595655|   -0.4957337|         0.6211384| (Intercept) | base\_r\_stats\_aggregate     |
|   0.0000130|   0.0000002|   55.7583561|         0.0000000| nrows       | base\_r\_stats\_aggregate     |
