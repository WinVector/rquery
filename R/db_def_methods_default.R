
# default to_sql implementations
# dispatching per handle instance (instead of per handle class)



#' Default to_sql method implementations.
#'
#' @return default implementation methods
#'
#' @keywords internal
#'
#' @export
#'
rquery_default_methods <- function() {
  list(
    "to_sql.relop_drop_columns" = to_sql_relop_drop_columns,
    "to_sql.relop_extend" = to_sql_relop_extend,
    "to_sql.relop_natural_join" = to_sql_relop_natural_join,
    "to_sql.relop_non_sql" = to_sql_relop_non_sql,
    "to_sql.relop_null_replace" = to_sql_relop_null_replace,
    "to_sql.relop_orderby" = to_sql_relop_orderby,
    "to_sql.relop_order_expr" = to_sql_relop_order_expr,
    "to_sql.relop_project" = to_sql_relop_project,
    "to_sql.relop_rename_columns" = to_sql_relop_rename_columns,
    "to_sql.relop_select_columns" = to_sql_relop_select_columns,
    "to_sql.relop_select_rows" = to_sql_relop_select_rows,
    "to_sql.relop_set_indicator" = to_sql_relop_set_indicator,
    "to_sql.relop_sql" = to_sql_relop_sql,
    "to_sql.relop_table_source" = to_sql_relop_table_source,
    "to_sql.relop_theta_join" = to_sql_relop_theta_join,
    "to_sql.relop_unionall" = to_sql_relop_unionall
  )
}




