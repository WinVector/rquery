
test_encode <- function() {
  ops <- mk_td("test_table", c("a", "b", "c", "d")) %.>%
    extend(., zz = a + b, dd = a - b) %.>%
    select_rows(., zz > 3) %.>%
    select_columns(., c('zz', 'a', 'c', 'd')) %.>%
    project(., d = max(d), m = min(c), groupby = c("a", "c")) %.>%
    drop_columns(., 'd') %.>%
    rename_columns(., c('qq' = 'a')) %.>%
    order_rows(., c('c')) %.>%
    natural_join(., mk_td("t2", c("a", "b", "c", "d")), by="c")
  rep <- to_transport_representation(ops)
  ops_back <- convert_yaml_to_pipeline(rep)
  s1 <- format(ops)
  s1 <- gsub("%:=%", ":=", s1, fixed=TRUE)
  s2 <- format(ops_back)
  s2 <- gsub("%:=%", ":=", s2, fixed=TRUE)
  RUnit::checkEquals(s1, s2)
  invisible(NULL)
}

test_encode_small <- function() {
  ops <- mk_td("test_table", c("a", "b")) %.>%
    extend(., zz = a + b, dd = a - b)
  rep <- to_transport_representation(ops)
  ops_back <- convert_yaml_to_pipeline(rep)
  s1 <- format(ops)
  s1 <- gsub("%:=%", ":=", s1, fixed=TRUE)
  s2 <- format(ops_back)
  s2 <- gsub("%:=%", ":=", s2, fixed=TRUE)
  RUnit::checkEquals(s1, s2)
  invisible(NULL)
}

