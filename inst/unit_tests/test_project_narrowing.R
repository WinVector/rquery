
test_project_narrowing <- function() {
  db <- rquery_db_info(identifier_quote_char = "`", string_quote_char = '"')
  d <- data.frame(group = c('a', 'a', 'b', 'b'),
                  val = 1:4,
                  stringsAsFactors = FALSE)

  op_tree <- local_td(d) %.>%
    project_se(., groupby = "group", "vmax" %:=% "max(val)")
  txt <- format(op_tree)
  txt <- to_sql(op_tree, db)
  RUnit::checkEquals(sort(column_names(op_tree)), c("group", "vmax"))

  op_tree <- local_td(d) %.>%
    project_se(., groupby = "group", "vmax" %:=% "max(val)") %.>%
    select_columns(., "group")
  txt <- format(op_tree)
  txt <- to_sql(op_tree, db)
  RUnit::checkEquals(sort(column_names(op_tree)), c("group"))

  op_tree <- local_td(d) %.>%
    project_se(., groupby = "group", "vmax" %:=% "max(val)") %.>%
    select_columns(., "vmax")
  txt <- format(op_tree)
  txt <- to_sql(op_tree, db)
  RUnit::checkEquals(sort(column_names(op_tree)), c("vmax"))

  invisible(NULL)
}
