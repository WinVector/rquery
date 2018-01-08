
plotbenchmark = function(timings, colormap, title, subtitle=NULL) {
  timings = timings %>%
    mutate(microseconds = time/1e6)

  means = timings %>%
    group_by(expr) %>%
    summarize(mean = mean(microseconds))

  ggplot(timings, aes(x = expr, y=microseconds)) +
    geom_violin(aes(fill=expr, color=expr)) +
    geom_point(data=means, aes(x=expr, y=mean), shape=3) +
    scale_color_manual(values=colormap) +
    scale_fill_manual(values=colormap) +
    coord_flip() +
    theme(legend.position="none") +
    ggtitle(title, subtitle=subtitle)


}
