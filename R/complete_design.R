

#' Cross product vectors in database.
#'
#' @param db database handle
#' @param values named list of value vectors.
#' @param ... force later arguments to bind by name.
#' @param temporary logical if TRUE try to make temporary table.
#' @param table_name name to land result as.
#' @return table handle.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   values <- list(nums = 1:3, lets = c("a", "b"))
#'   res <- expand_grid(my_db, values)
#'   print(res)
#'   execute(my_db, res)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
expand_grid <- function(db,
                        values,
                        ...,
                        temporary = TRUE,
                        table_name = wrapr::mk_tmp_name_source('eg')()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::expand_grid")
  temp_name_source <- wrapr::mk_tmp_name_source('egb')
  tabs <- lapply(names(values),
                 function(ni) {
                   di <- data.frame(x = values[[ni]], stringsAsFactors = FALSE)
                   colnames(di) <- ni
                   tni <- temp_name_source()
                   rq_copy_to(db, tni, di)
                 })
  qs <- vapply(tabs,
               function(tni) {
                 paste0("LEFT JOIN ", quote_identifier(db, tni$table_name), " WHERE 1=1")
               }, character(1))
  qs[[1]] <- paste0("SELECT * FROM ", quote_identifier(db, tabs[[1]]$table_name))
  sql <- paste(qs, collapse = " ")
  sql <- materialize_sql_statement(db, sql, table_name, temporary = temporary)
  rq_execute(db, sql)
  res <- db_td(db, table_name)
  for(tni in tabs) {
    rq_remove_table(db, tni$table_name)
  }
  res
}

#' Complete an experimental design.
#'
#' @param design_table optree or for experimental design.
#' @param data_table optree for data.
#' @return joined and annotated table optree.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#'   # example experimental design
#'   values <- list(nums = 1:3, lets = c("a", "b"))
#'   design <- expand_grid(my_db, values)
#'
#'   # not quite matching data
#'   data <- build_frame(
#'     "nums", "lets"   |
#'       1L    , "a"    |
#'       1L    , "b"    |
#'       77L   , "a"    |  # out of place ID
#'       2L    , "b"    |
#'       3L    , "a"    |
#'       3L    , "a"    | # duplicated
#'       3L    , "b"    )
#'   data$row_number <- seq_len(nrow(data))
#'   data <- rq_copy_to(my_db, "data", data)
#'
#'   # compare/augment
#'   res <- complete_design(design, data)
#'   cat(format(res))
#'   res <- materialize(my_db, res)
#'
#'   print("completed data design")
#'   print(execute(my_db, res))
#'
#'   # look for dups (can use extende_se(partation) on
#'   # databases with window fns.
#'   print("duplicate key rows:")
#'   res %.>%
#'     project_se(.,
#'                groupby = column_names(design),
#'                "count" %:=% "SUM(1)") %.>%
#'     select_rows_se(., "count>1") %.>%
#'     execute(my_db, .) %.>%
#'     print(.)
#'
#'   # look for data that was not in design
#'   print("data rows not in design:")
#'   data %.>%
#'     natural_join(., res,
#'                  jointype = "LEFT",
#'                  by = column_names(design)) %.>%
#'     select_rows_se(., "is.na(row_in_design_table)") %.>%
#'     execute(my_db, .) %.>%
#'     print(.)
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
complete_design <- function(design_table, data_table) {
  if(!("relop" %in% class(design_table))) {
    stop("rquery::complete_design: design_table table must be of class relop")
  }
  if(!("relop" %in% class(data_table))) {
    stop("rquery::complete_design: data_table table must be of class relop")
  }
  cols <- column_names(design_table)
  missing <- setdiff(cols, column_names(data_table))
  if(length(missing)>0) {
    stop(paste("rquery::complete_design data_table missing design_table columns",
               paste(missing, collapse = ", ")))
  }
  design_table <- design_table %.>%
    extend_se(., "row_in_design_table" %:=% 1)
  data_table <- data_table %.>%
    extend_se(., "row_in_data_table" %:=% 1)
  by = intersect(column_names(design_table), column_names(data_table))
  natural_join(design_table, data_table, jointype = "LEFT", by = by)  %.>%
    null_replace(., c("row_in_design_table", "row_in_data_table"), 0)
}
