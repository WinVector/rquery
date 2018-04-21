
#' Remap values in a set of columns.
#'
#' @param source optree relnode or data.frame.
#' @param colmap data.frame with columns column_name, old_value, new_value.
#' @param ... force later arguments to bind by name.
#' @param null_default logical, if TRUE map non-matching values to NULL (else they map to self).
#' @return implementing optree or altered data.frame
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'
#'   d <- dbi_copy_to(my_db, 'd',
#'                    data.frame(a = c("1", "2", "1", "3"),
#'                               b = c("1", "1", "3", "2"),
#'                               c = c("1", "2", "3", "4"),
#'                               stringsAsFactors = FALSE),
#'                    temporary = TRUE,
#'                    overwrite = TRUE)
#'   mp <- build_frame(
#'       "column_name", "old_value", "new_value" |
#'       "a"          , "1"        , "10"        |
#'       "a"          , "2"        , "20"        |
#'       "b"          , "1"        , "100"       |
#'       "b"          , "3"        , "300"       )
#'
#'   # example
#'   op_tree <- d %.>%
#'     map_column_values(., mp)
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   # cleanup
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
map_column_values <- function(source, colmap,
                              ...,
                              null_default = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::map_column_values")
  colmap_name <- as.character(substitute(colmap))[[1]]
  control_cols <- c("column_name", "old_value", "new_value")
  missing <- setdiff(control_cols, colnames(colmap))
  if(length(missing)>0) {
    stop(paste("rquery::map_column_value colmap missing column(s):",
               paste(missing, collapse = ", ")))
  }
  for(ci in control_cols) {
    if(is.factor(colmap[[ci]])) {
      colmap[[ci]] <- as.character(ci)
    }
  }
  cols <- column_names(source)
  targets <- intersect(cols,
                       sort(unique(colmap$column_name)))
  if(length(targets)<=0) {
    return(source)
  }
  terms <- lapply(targets,
                  function(ci) {
                    default <- "NULL"
                    if(!null_default) {
                      default <- as.name(ci)
                    }
                    cmpi <- colmap[colmap$column_name == ci, , drop = FALSE]
                    ni <- nrow(cmpi)
                    ti <- lapply(seq_len(ni),
                                 function(j) {
                                   tij <- list("WHEN ",
                                               as.name(ci),
                                               " = ",
                                               list(cmpi$old_value[[j]]),
                                               " THEN ",
                                               list(cmpi$new_value[[j]]))
                                 })
                    ti <- unlist(ti, recursive = FALSE)
                    c(list("CASE "), ti, list(" ELSE "), default, " END")
                  }
  )
  names(terms) <- targets
  nd <- sql_node(source, terms,
                 orig_columns = TRUE)
  if("relop" %in% class(nd)) {
    nd$display_form <- paste0("map_column_values(., ",
                              colmap_name,
                              ")")
  }
  nd
}
