



#' Assign a value to a slice of data (set of rows meeting a condition, and specified set of columns).
#'
#' Uses \code{if_else_block}.
#'
#' Note: \code{ifebtest_*}
#' is a reserved column name for this procedure.
#'
#' @param source optree relop node or data.frame.
#' @param testexpr character containing the test expression.
#' @param columns character vector of column names to alter.
#' @param value value to set in matching rows and columns (scalar).
#' @param env environment to look to.
#' @return optree or data.frame.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(
#'     my_db,
#'     'd',
#'     data.frame(i = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'                a = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
#'                b = c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1),
#'                r = runif(10)),
#'     temporary=TRUE, overwrite=TRUE)
#'
#'   optree <- d %.>%
#'     assign_slice(.,
#'                  testexpr = qe(r<0.5),
#'                  columns = qc(a, b),
#'                  value = 2)
#'   cat(format(optree))
#'
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
assign_slice <- function(source, testexpr, columns, value,
                         env = parent.frame()) {
  force(env)
  if((!is.character(columns)) || (length(columns)<1)) {
    stop("rquery::assign_slice columns should be a non-empty character vector")
  }
  if(length(value)!=1) {
    stop("rquery::assign_slice value should be length 1.")
  }
  if_else_op(source = source,
             testexpr = testexpr,
             thenexprs = columns %:=% rep(value, length(columns)),
             env = env)
}
