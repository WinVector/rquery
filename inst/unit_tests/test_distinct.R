test_distinct <- function() {

  table_rep <- mk_td("test_table", c("a", "b", "c", "d"))
  ops <- project(table_rep, groupby = c("a", "b", "c"))
  sql <- to_sql(ops, rquery::rquery_default_db_info())

  invisible(NULL)
}
