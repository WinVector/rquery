
test_compare <- function() {
  ops <- mk_td("d", "id") %.>%
    extend(.,
           is_one %:=% id == 1,
           is_not_one %:=% id != 1)
  sql <- to_sql(ops, rquery::rquery_default_db_info())
  # data.frame(id=c(1,2)) %.>% ops
  # # id is_one is_not_one
  # # 1:  1   TRUE      FALSE
  # # 2:  2  FALSE       TRUE
  invisible(NULL)
}

test_compare()
