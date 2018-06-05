data.table backend for rquery replot
================
John Mount, Win-Vector LLC
06/04/2018

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
|  2e+00| rquery\_database\_round\_trip |            0.0649073|
|  2e+00| rquery\_data.table            |            0.0078371|
|  2e+00| data.table                    |            0.0028475|
|  2e+00| dplyr\_data\_frame            |            0.0101308|
|  2e+00| dplyr\_tbl                    |            0.0089867|
|  2e+00| dplyr\_database\_round\_trip  |            0.1682212|
|  2e+00| base\_r\_stats\_aggregate     |            0.0039236|
|  4e+00| rquery\_database\_round\_trip |            0.1130579|
|  4e+00| rquery\_data.table            |            0.0104997|
|  4e+00| data.table                    |            0.0094750|
|  4e+00| dplyr\_data\_frame            |            0.0121207|
|  4e+00| dplyr\_tbl                    |            0.0123274|
|  4e+00| dplyr\_database\_round\_trip  |            0.2249833|
|  4e+00| base\_r\_stats\_aggregate     |            0.0045256|
|  1e+01| rquery\_database\_round\_trip |            0.0673495|
|  1e+01| rquery\_data.table            |            0.0085403|
|  1e+01| data.table                    |            0.0028385|
|  1e+01| dplyr\_data\_frame            |            0.0123538|
|  1e+01| dplyr\_tbl                    |            0.0103893|
|  1e+01| dplyr\_database\_round\_trip  |            0.1952793|
|  1e+01| base\_r\_stats\_aggregate     |            0.0032864|
|  2e+01| rquery\_database\_round\_trip |            0.0845266|
|  2e+01| rquery\_data.table            |            0.0075481|
|  2e+01| data.table                    |            0.0036693|
|  2e+01| dplyr\_data\_frame            |            0.0106921|
|  2e+01| dplyr\_tbl                    |            0.0104718|
|  2e+01| dplyr\_database\_round\_trip  |            0.1701978|
|  2e+01| base\_r\_stats\_aggregate     |            0.0040236|
|  4e+01| rquery\_database\_round\_trip |            0.0777367|
|  4e+01| rquery\_data.table            |            0.0074640|
|  4e+01| data.table                    |            0.0028862|
|  4e+01| dplyr\_data\_frame            |            0.0126486|
|  4e+01| dplyr\_tbl                    |            0.0105235|
|  4e+01| dplyr\_database\_round\_trip  |            0.1710284|
|  4e+01| base\_r\_stats\_aggregate     |            0.0039526|
|  1e+02| rquery\_database\_round\_trip |            0.0843180|
|  1e+02| rquery\_data.table            |            0.0070526|
|  1e+02| data.table                    |            0.0027870|
|  1e+02| dplyr\_data\_frame            |            0.0123134|
|  1e+02| dplyr\_tbl                    |            0.0109164|
|  1e+02| dplyr\_database\_round\_trip  |            0.1727961|
|  1e+02| base\_r\_stats\_aggregate     |            0.0052377|
|  2e+02| rquery\_database\_round\_trip |            0.0746008|
|  2e+02| rquery\_data.table            |            0.0092542|
|  2e+02| data.table                    |            0.0032619|
|  2e+02| dplyr\_data\_frame            |            0.0151535|
|  2e+02| dplyr\_tbl                    |            0.0122756|
|  2e+02| dplyr\_database\_round\_trip  |            0.1779851|
|  2e+02| base\_r\_stats\_aggregate     |            0.0052803|
|  4e+02| rquery\_database\_round\_trip |            0.0752570|
|  4e+02| rquery\_data.table            |            0.0079909|
|  4e+02| data.table                    |            0.0047024|
|  4e+02| dplyr\_data\_frame            |            0.0162655|
|  4e+02| dplyr\_tbl                    |            0.0177483|
|  4e+02| dplyr\_database\_round\_trip  |            0.1797361|
|  4e+02| base\_r\_stats\_aggregate     |            0.0070549|
|  1e+03| rquery\_database\_round\_trip |            0.0941370|
|  1e+03| rquery\_data.table            |            0.0118472|
|  1e+03| data.table                    |            0.0053236|
|  1e+03| dplyr\_data\_frame            |            0.0279269|
|  1e+03| dplyr\_tbl                    |            0.0310280|
|  1e+03| dplyr\_database\_round\_trip  |            0.1807755|
|  1e+03| base\_r\_stats\_aggregate     |            0.0104224|
|  2e+03| rquery\_database\_round\_trip |            0.1008779|
|  2e+03| rquery\_data.table            |            0.0113288|
|  2e+03| data.table                    |            0.0063787|
|  2e+03| dplyr\_data\_frame            |            0.0630888|
|  2e+03| dplyr\_tbl                    |            0.0492784|
|  2e+03| dplyr\_database\_round\_trip  |            0.1930912|
|  2e+03| base\_r\_stats\_aggregate     |            0.0191342|
|  4e+03| rquery\_database\_round\_trip |            0.1352459|
|  4e+03| rquery\_data.table            |            0.0134270|
|  4e+03| data.table                    |            0.0121296|
|  4e+03| dplyr\_data\_frame            |            0.1072565|
|  4e+03| dplyr\_tbl                    |            0.0871459|
|  4e+03| dplyr\_database\_round\_trip  |            0.2188664|
|  4e+03| base\_r\_stats\_aggregate     |            0.0323976|
|  1e+04| rquery\_database\_round\_trip |            0.2144200|
|  1e+04| rquery\_data.table            |            0.0227190|
|  1e+04| data.table                    |            0.0206780|
|  1e+04| dplyr\_data\_frame            |            0.2069342|
|  1e+04| dplyr\_tbl                    |            0.2139504|
|  1e+04| dplyr\_database\_round\_trip  |            0.3058645|
|  1e+04| base\_r\_stats\_aggregate     |            0.0986916|
|  2e+04| rquery\_database\_round\_trip |            0.3188168|
|  2e+04| rquery\_data.table            |            0.0311858|
|  2e+04| data.table                    |            0.0400527|
|  2e+04| dplyr\_data\_frame            |            0.4148661|
|  2e+04| dplyr\_tbl                    |            0.3807144|
|  2e+04| dplyr\_database\_round\_trip  |            0.4891545|
|  2e+04| base\_r\_stats\_aggregate     |            0.1672741|
|  4e+04| rquery\_database\_round\_trip |            0.5538255|
|  4e+04| rquery\_data.table            |            0.0545196|
|  4e+04| data.table                    |            0.0766362|
|  4e+04| dplyr\_data\_frame            |            0.7971585|
|  4e+04| dplyr\_tbl                    |            0.7693824|
|  4e+04| dplyr\_database\_round\_trip  |            0.8168026|
|  4e+04| base\_r\_stats\_aggregate     |            0.3471680|
|  1e+05| rquery\_database\_round\_trip |            1.2811239|
|  1e+05| rquery\_data.table            |            0.1415530|
|  1e+05| data.table                    |            0.1885807|
|  1e+05| dplyr\_data\_frame            |            1.8764616|
|  1e+05| dplyr\_tbl                    |            1.8951409|
|  1e+05| dplyr\_database\_round\_trip  |            1.8417333|
|  1e+05| base\_r\_stats\_aggregate     |            1.0809947|
|  2e+05| rquery\_database\_round\_trip |            2.5190491|
|  2e+05| rquery\_data.table            |            0.2446987|
|  2e+05| data.table                    |            0.3554993|
|  2e+05| dplyr\_data\_frame            |            4.0468353|
|  2e+05| dplyr\_tbl                    |            4.0254267|
|  2e+05| dplyr\_database\_round\_trip  |            3.7220950|
|  2e+05| base\_r\_stats\_aggregate     |            2.3109714|
|  4e+05| rquery\_database\_round\_trip |            5.1137443|
|  4e+05| rquery\_data.table            |            0.4884806|
|  4e+05| data.table                    |            0.6126887|
|  4e+05| dplyr\_data\_frame            |            7.6176059|
|  4e+05| dplyr\_tbl                    |            7.6047584|
|  4e+05| dplyr\_database\_round\_trip  |            7.5356660|
|  4e+05| base\_r\_stats\_aggregate     |            4.0474981|
|  1e+06| rquery\_database\_round\_trip |           12.7160322|
|  1e+06| rquery\_data.table            |            1.0012875|
|  1e+06| data.table                    |            1.3360819|
|  1e+06| dplyr\_data\_frame            |           19.2944260|
|  1e+06| dplyr\_tbl                    |           18.7301739|
|  1e+06| dplyr\_database\_round\_trip  |           18.9309185|
|  1e+06| base\_r\_stats\_aggregate     |            9.8023726|
|  2e+06| rquery\_database\_round\_trip |           25.5952836|
|  2e+06| rquery\_data.table            |            1.9244170|
|  2e+06| data.table                    |            2.5596263|
|  2e+06| dplyr\_data\_frame            |           37.4853399|
|  2e+06| dplyr\_tbl                    |           44.4424479|
|  2e+06| dplyr\_database\_round\_trip  |           38.5996283|
|  2e+06| base\_r\_stats\_aggregate     |           21.4574387|
|  4e+06| rquery\_database\_round\_trip |           54.1635428|
|  4e+06| rquery\_data.table            |            3.8523687|
|  4e+06| data.table                    |            5.0816987|
|  4e+06| dplyr\_data\_frame            |           76.6911901|
|  4e+06| dplyr\_tbl                    |           75.4668688|
|  4e+06| dplyr\_database\_round\_trip  |           81.8405695|
|  4e+06| base\_r\_stats\_aggregate     |           44.4686566|
|  1e+07| rquery\_database\_round\_trip |          132.6272294|
|  1e+07| rquery\_data.table            |            9.3987317|
|  1e+07| data.table                    |           12.8387892|
|  1e+07| dplyr\_data\_frame            |          175.5231451|
|  1e+07| dplyr\_tbl                    |          179.1429390|
|  1e+07| dplyr\_database\_round\_trip  |          217.1214245|
|  1e+07| base\_r\_stats\_aggregate     |          111.2737348|

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
|   0.0004859|   0.1130836|    0.0042972|         0.9965796| (Intercept) | rquery\_database\_round\_trip |
|   0.0000133|   0.0000000|  282.1093691|         0.0000000| nrows       | rquery\_database\_round\_trip |
|   0.0254542|   0.0216557|    1.1754045|         0.2425428| (Intercept) | rquery\_data.table            |
|   0.0000009|   0.0000000|  104.3909398|         0.0000000| nrows       | rquery\_data.table            |
|   0.0194980|   0.0295800|    0.6591610|         0.5112634| (Intercept) | data.table                    |
|   0.0000013|   0.0000000|  103.9696784|         0.0000000| nrows       | data.table                    |
|   0.4024921|   0.2047644|    1.9656349|         0.0520331| (Intercept) | dplyr\_data\_frame            |
|   0.0000178|   0.0000001|  208.4230698|         0.0000000| nrows       | dplyr\_data\_frame            |
|   0.5183865|   0.4367638|    1.1868807|         0.2380049| (Intercept) | dplyr\_tbl                    |
|   0.0000181|   0.0000002|   99.6456322|         0.0000000| nrows       | dplyr\_tbl                    |
|  -0.4287226|   0.3862210|   -1.1100447|         0.2695646| (Intercept) | dplyr\_database\_round\_trip  |
|   0.0000215|   0.0000002|  133.7546498|         0.0000000| nrows       | dplyr\_database\_round\_trip  |
|  -0.1191434|   0.1006702|   -1.1835023|         0.2393344| (Intercept) | base\_r\_stats\_aggregate     |
|   0.0000111|   0.0000000|  265.3354167|         0.0000000| nrows       | base\_r\_stats\_aggregate     |
