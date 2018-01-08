
plotbenchmark = function(timings, title) {
  timings = timings %>%
    mutate(microseconds = time/1e6)

  means = timings %>%
    group_by(expr) %>%
    summarize(mean = mean(microseconds))

  ggplot(timings, aes(x = expr, y=microseconds)) +
    geom_violin(fill="darkolivegreen3", color="darkolivegreen3") +
    geom_point(data=means, aes(x=expr, y=mean), shape=3) +
    coord_flip() + ggtitle(title)

}
