



#' Assign a value to a slice of data (set of rows meeting a condition, and specified set of columns).
#'
#' Uses \code{if_else_block}.
#'
#' Note: \code{ifebtest_*}
#' is a reserved column name for this procedure.
#'
#' @param testexpr character containing the test expression.
#' @param columns charactor vector of column names to alter.
#' @param value value to set in matching rows and columns (scalar).
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   # Example: clear one of a or b in any row where both are set.
#'   # Land random selections early to avoid SQLite bug.
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- dbi_copy_to(
#'     my_db,
#'     'd',
#'     data.frame(i = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'                a = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
#'                b = c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1),
#'                r = runif(10)),
#'     temporary=TRUE, overwrite=TRUE)
#'
#'   program <- assign_slice(
#'     testexpr = qe(r<0.5),
#'     columns = qc(a, b),
#'     value = 2)
#'   print(program)
#'
#'   trf <- extend_se(d, program)
#'   cat(format(trf))
#'
#'   sql <- to_sql(trf, my_db)
#'   cat(sql)
#'
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   # Why we need to land the random selection early
#'   # for SQLIte:
#'   q <- "SELECT r AS r1, r AS r2 FROM (
#'           SELECT random() AS r FROM (
#'              SELECT * from ( VALUES(1),(2) )
#'           ) a
#'        ) b"
#'   print(DBI::dbGetQuery(my_db, q))
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
assign_slice <- function(testexpr, columns, value) {
  if((!is.character(columns)) || (length(columns)<1)) {
    stop("rquery::assign_slice columns should be a non-empty character vector")
  }
  if(length(value)!=1) {
    stop("rquery::assign_slice value should be length 1.")
  }
  if_else_block(testexpr = testexpr,
                thenexprs = columns := rep(value, length(columns)))
}
