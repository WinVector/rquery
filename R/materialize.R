
#' Materialize an optree as a table.
#'
#' Run the data query as a CREATE TABLE AS . Think of as a function
#' that can be applied to relop trees, not as a component to place
#' in pipelines.
#'
#' @param optree relop operation tree.
#' @param db DBI connecton.
#' @param table_name character, name of table to create.
#' @param ... force later arguments to bind by name.
#' @param overwrite logical if TRUE drop an previous table.
#' @param temporary logical if TRUE try to create a temporary table.
#' @return table handle
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' optree <- extend_se(d, c("v" := "AUC + R2", "x" := "pmax(AUC,v)"))
#' cat(format(optree))
#' res <- materialize(optree, my_db, "example")
#' cat(format(res))
#' sql <- to_sql(res, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
materialize <- function(optree,
                        db,
                        table_name = mkTempNameGenerator('rquery_mat'),
                        ...,
                        overwrite = FALSE,
                        temporary = TRUE) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  sql <- to_sql(optree, db)
  sql <- paste0("CREATE ",
                ifelse(temporary, "TEMPORARY ", ""),
                "TABLE ",
                quote_identifier(db, table_name),
                " AS ",
                sql)
  if(overwrite) {
    if(DBI::dbExistsTable(db, table_name)) {
      DBI::dbExecute(db,
                   paste0("DROP TABLE ",
                          quote_identifier(db, table_name)))
    }
  }
  DBI::dbExecute(db, sql)
  dbi_table(db, table_name)
}
