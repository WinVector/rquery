

#' Build a query that applies a SQL expression to a set of columns.
#'
#' @param source incoming rel_op tree or data.frame.
#' @param cols character, columns to operate in.  If a named array names are where results are landed, values names of value columns.
#' @param expr character or list of character and names, expression to apply to columns "." stands for column value to use.
#' @return rel_op node or data.frame (depending on input).
#'
#' @seealso \code{\link{count_null_cols}}, \code{\link{mark_null_cols}}
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   winvector_temp_db_handle <- list(
#'     db = my_db
#'   )
#'   RSQLite::initExtension(winvector_temp_db_handle$db)
#'
#'   d <- dbi_copy_to(my_db, 'd',
#'                    data.frame(AUC = c(NA, 0.5, NA),
#'                               R2 = c(1.0, 0.9, NA),
#'                               delta = 3,
#'                               cat = c("a", NA, "c"),
#'                               stringsAsFactors = FALSE))
#'
#'   # example
#'   op_tree <- d %.>% sql_expr_set(., qc(AUC, R2), ". + 1")
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   # ex2 names (but not marked as names)
#'   op_tree2 <- d %.>% sql_expr_set(., qc(AUC, R2),
#'      ". + 1 + delta")
#'   cat(to_sql(op_tree2, my_db))
#'
#'   # ex3 names (also so marked)
#'   op_tree3 <- d %.>% sql_expr_set(., qc(AUC, R2),
#'      list(". + 1 +", as.name("delta")))
#'   cat(to_sql(op_tree3, my_db))
#'
#'   # cleanup
#'   rm(list = "winvector_temp_db_handle")
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
sql_expr_set <- function(source, cols, expr) {
  nc <- length(cols)
  if(nc<1) {
    stop("rquery::replace_null_cols need at least one column name")
  }
  source_cols <- column_names(source)
  bad_cols <- setdiff(as.character(cols), source_cols)
  if(length(bad_cols)>0) {
    stop(paste("rquery::replace_null_cols unknown columns:",
               paste(bad_cols, collapse = ", ")))
  }
  if(is.null(names(cols))) {
    names(cols) <- cols
  }
  others <- as.list(setdiff(source_cols, names(cols)))
  names(others) <- others
  # refactor expr to be a list of names and lists
  expr_list <- lapply(expr,
                      function(ei) {
                        if(!is.character(ei)) {
                          return(list(ei))
                        }
                        pieces <- strsplit(ei, ".", fixed = TRUE)[[1]]
                        pieces <- vapply(pieces, trimws, character(1))
                        pieces <- as.list(pieces)
                        p2 <- rep(list(as.name(".")),
                                  2*length(pieces)-1)
                        p2[2*seq_len(length(pieces))-1] <- pieces
                        p2 <- p2[p2!=""]
                        p2
                      })
  expr_list <- unlist(expr_list, recursive = FALSE, use.names = FALSE)
  # re-write expression per-column
  terms <- lapply(cols,
                  function(ci) {
                    expri <- lapply(expr_list,
                                    function(ti) {
                                      if(is.name(ti) &&
                                         (as.character(ti)==".")) {
                                        return(as.name(ci))
                                      }
                                      ti
                                    })
                  })
  names(terms) <- names(cols)
  nd <- sql_node(source, c(terms, others),
                 orig_columns = FALSE)
  exprstr <- paste(as.character(unlist(expr)), collapse = " ")
  nd$display_form <- paste0("sql_expr_set(",
                            paste(cols, collapse = ", "),
                            "; ",
                            exprstr,
                            ")")
  nd
}
