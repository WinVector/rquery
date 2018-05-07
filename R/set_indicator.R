#' Build an in set indicator.
#'
#' @param source optree relnode or data.frame.
#' @param rescol name of column to land indicator in.
#' @param testcol name of column to check.
#' @param testvalues values to check for.
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
#'                               stringsAsFactors = FALSE),
#'                    temporary = TRUE,
#'                    overwrite = TRUE)
#'   # example
#'   set <- c("1", "2")
#'   op_tree <- d %.>%
#'     set_indicator(., "one_two", "a", set)
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
set_indicator <- function(source, rescol, testcol, testvalues) {
  testvname <- paste(as.character(substitute(testvalues)), collapse = "")
  if(length(testvalues)<1) {
    stop("rquery::set_indicator testvalues must not be empty")
  }
  terms = list(as.name(testcol), " IN ( ")
  for(i in seq_len(length(testvalues))) {
    if(i>1) {
      terms <- c(terms, " , ")
    }
    terms <- c(terms, list(list(testvalues[[i]])))
  }
  terms <- c(terms, list(" ) "))
  terms <- list(terms)
  names(terms) <- rescol
  nd <- sql_node(source, terms,
                 orig_columns = TRUE)
  if("relop" %in% class(nd)) {
    nd$display_form <- paste0("set_indicator(., ",
                              rescol,
                              " = ",
                              testcol,
                              " IN ",
                              testvname,
                              ")")
  }
  nd
}
