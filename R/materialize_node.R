

#' Create a materialize node.
#'
#' Write results into a specified table.  Result is transient, lives only for the duration of the
#' pipeline calculation.  This node is only used to break up or un-nest calculations, not for value sharing or
#' re-use.
#'
#' Note this node can not be used in multiple paths in the same rel_op tree as it re-uses table names and
#' re-computes each time called.
#'
#'
#' @param source source to work from (relop node)
#' @param table_name character, name of caching table
#' @param ... not used, force later argument to bind by name
#' @param qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @return relop materialize_node
#'
#' @seealso \code{\link{rsummary_node}}, \code{\link{non_sql_node}}
#'
#' @export
#'
materialize_node <- function(source,
                             table_name = wrapr::mk_tmp_name_source("rquerymn")(),
                             ...,
                             qualifiers = NULL) {
  if("data.frame" %in% class(source)) {
    return(source)
  }
  if(!("relop" %in% class(source))) {
    stop("rquery::materialize_node requires source be of class relop")
  }
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::materialize_node")
  non_sql_node(source = source,
               f_db = NULL,
               f_df = function(x, nd = NULL) { x },
               f_dt = NULL,
               incoming_table_name = table_name,
               incoming_qualifiers = qualifiers,
               outgoing_table_name = table_name,
               outgoing_qualifiers = qualifiers,
               columns_produced = NULL,
               display_form = paste0("materialize_node(", table_name, ")"),
               orig_columns = TRUE,
               temporary = TRUE)
}

