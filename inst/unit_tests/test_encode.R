
test_encode <- function() {
  ops0 <- mk_td("test_table", c("a", "b", "c", "d")) %.>%
    extend(., zz = a + b, dd = a - b) %.>%
    select_rows(., zz > 3) %.>%
    select_columns(., c('zz', 'a', 'c', 'd')) %.>%
    project(., d = max(d), m = min(c), groupby = c("a", "c")) %.>%
    drop_columns(., 'd') %.>%
    rename_columns(., c('qq' = 'a')) %.>%
    order_rows(., c('c')) %.>%
    natural_join(., mk_td("t2", c("a", "b", "c", "d")), by="c")
  ops <- mk_td("test_table", c("a", "b", "c", "d")) %.>%
    extend(., zz = a + b, dd = a - b) %.>%
    select_rows(., zz > 3) %.>%
    select_columns(., c('zz', 'a', 'c', 'd')) %.>%
    project(., d = max(d), m = min(c), groupby = c("a", "c")) %.>%
    select_columns(., c("a", "c", "m")) %.>%
    rename_columns(., c('qq' = 'a')) %.>%
    order_rows(., c('c')) %.>%
    natural_join(., mk_td("t2", c("a", "b", "c", "d")), by="c")
  s1 <- format(ops)
  s1 <- gsub("%:=%", ":=", s1, fixed=TRUE)

  rep <- to_transport_representation(ops0)
  ops_back <- convert_yaml_to_pipeline(rep)
  s2 <- format(ops_back)
  s2 <- gsub("%:=%", ":=", s2, fixed=TRUE)
  RUnit::checkEquals(s1, s2)

  fmt <- format(ops)
  ops_p <- eval(parse(text=fmt))
  RUnit::checkTrue(is(ops_p, 'relop'))
  s3 <- format(ops_p)
  s3 <- gsub("%:=%", ":=", s3, fixed=TRUE)
  RUnit::checkEquals(s1, s3)

  if(requireNamespace('yaml', quietly = TRUE)) {
    yaml_str <- yaml::as.yaml(rep)
    rep_back <- yaml::read_yaml(text=yaml_str)
    ops_back_y <- convert_yaml_to_pipeline(rep)
    s4 <- format(ops_back)
    s4 <- gsub("%:=%", ":=", s4, fixed=TRUE)
    RUnit::checkEquals(s1, s4)
  }

  invisible(NULL)
}

test_encode_small <- function() {
  ops <- mk_td("test_table", c("a", "b")) %.>%
    extend(., zz = a + b, dd = a - b)
  s1 <- format(ops)
  s1 <- gsub("%:=%", ":=", s1, fixed=TRUE)

  rep <- to_transport_representation(ops)
  ops_back <- convert_yaml_to_pipeline(rep)
  s2 <- format(ops_back)
  s2 <- gsub("%:=%", ":=", s2, fixed=TRUE)
  RUnit::checkEquals(s1, s2)

  fmt <- format(ops)
  ops_p <- eval(parse(text=fmt))
  RUnit::checkTrue(is(ops_p, 'relop'))
  s3 <- format(ops_p)
  s3 <- gsub("%:=%", ":=", s3, fixed=TRUE)
  RUnit::checkEquals(s1, s3)

  if(requireNamespace('yaml', quietly = TRUE)) {
    yaml_str <- yaml::as.yaml(rep)
    rep_back <- yaml::read_yaml(text=yaml_str)
    ops_back_y <- convert_yaml_to_pipeline(rep)
    s4 <- format(ops_back)
    s4 <- gsub("%:=%", ":=", s4, fixed=TRUE)
    RUnit::checkEquals(s1, s4)
  }

  invisible(NULL)
}



