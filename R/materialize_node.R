
# TODO: put in test confirming materialize_node can narrow columns (as mentioned in extras/DebugToolsForBigData*.Rmd).

#' Create a materialize node.
#'
#' Note this node can not be used in multiple paths in the same rel_op tree as it re-uses table names and
#' re-computes each time called.
#'
#' @param source incoming source (relop node or data.frame).
#' @param outgoing_table_name character, name of table to write.
#' @param ... force later arguments to be by name
#' @param overwrite logical, if TRUE overwrite tables
#' @param temporary logical, if TRUE use temporary tables
#' @return rsummary node
#'
#' @examples
#'
#'  d <- data.frame(p= c(TRUE, FALSE, NA),
#'                  s= NA,
#'                  w= 1:3,
#'                  x= c(NA,2,3),
#'                  y= factor(c(3,5,NA)),
#'                  z= c('a',NA,'a'),
#'                  stringsAsFactors=FALSE)
#'  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'  RSQLite::initExtension(db)
#'  DBI::dbWriteTable(db, "dRemote", d,
#'                    overwrite = TRUE,
#'                    temporary = TRUE)
#'
#'  ops <- dbi_table(db, "dRemote") %.>%
#'    extend_nse(., v := ifelse(x>2, "x", "y")) %.>%
#'    materialize_node(., outgoing_table_name = "intermediate") %.>%
#'    extend_nse(., v2 := ifelse(x>2, "x", "y"))
#'  cat(format(ops))
#'
#'  to_sql(ops, db)
#'
#'  reshdl <- materialize(ops, db)
#'  DBI::dbGetQuery(db, to_sql(reshdl, db))
#'
#'  DBI::dbGetQuery(db, "SELECT * FROM intermediate")
#'
#'  DBI::dbDisconnect(db)
#'
#' @export
#'
materialize_node <- function(source,
                             ...,
                             outgoing_table_name = mk_tmp_name_source("mout")(),
                             temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::materialize_node")
  if(is.data.frame(source)) {
    tmp_name <- mk_tmp_name_source("rquery_tmp")()
    dnode <- table_source(tmp_name, colnames(source))
    dnode$data <- source
    source <- dnode
  }
  columns_used <- column_names(source)
  columns_produced <- columns_used
  force(temporary)
  nd <- non_sql_node(source,
                     incoming_table_name = outgoing_table_name,
                     outgoing_table_name = outgoing_table_name,
                     columns_produced = columns_produced,
                     display_form = paste0("materialize_node(., ",
                                           outgoing_table_name,
                                           ")"),
                     orig_columns = FALSE,
                     temporary = temporary)
  nd$materialize_as <- outgoing_table_name
  nd
}

