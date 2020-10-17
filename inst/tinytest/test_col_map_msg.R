
test_col_map_msg <- function() {
  if(requireNamespace('rqdatatable', quietly = TRUE)) {
    library("rqdatatable")
    d <- data.frame(x = c('a', 'b', 'b'),
                    stringsAsFactors = FALSE)
    mp <- c(a = 1, b = 2)
    y <- 2
    v <- TRUE
    n <- NA
    d %.>% extend(.,
                  xv = y)
    d %.>%
      extend(.,
             xv = 7)
    d %.>%
      extend(.,
             xv = TRUE)
    d %.>%
      extend(.,
             xv = v)
    d %.>%
      extend(.,
             xv = n)
    # # would like this to error-out but
    # # have to leave it as a free-symbol due to
    # # issue mentioned in tokenize_for_sql
    expect_error({
      d %.>%
        extend(.,
               xv = mp[x])},
      silent = TRUE)
  }
  invisible(NULL)
}
