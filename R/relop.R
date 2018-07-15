
# define relational op, basis for this package.
#
# Each node should be a list with named entries:
#   source: list of nodes supplying values to this node.
#   table_name: character name of table if a concrete table or view.
#   parsed: list of parsed expressions.




# add class info and helpers to a relop node.
relop_decorate <- function(class_name, r) {
  class(r) <- c(class_name, "relop")
  r
}

#' Return column names
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments
#' @return vector of column names
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d1 <- rq_copy_to(my_db, 'd1',
#'                     data.frame(AUC = 0.6, R2 = 0.2))
#'   d2 <- rq_copy_to(my_db, 'd2',
#'                     data.frame(AUC = 0.6, D = 0.3))
#'   optree <- natural_join(d1, d2, by = "AUC")
#'   cat(format(optree))
#'   print(column_names(optree))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
column_names <- function (x, ...) {
  UseMethod("column_names", x)
}



#' @export
column_names.relop <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::column_names.relop")
  inputs <- unlist(lapply(x$source,
                          column_names))
  inputs[!duplicated(inputs)]
}

#' @export
column_names.data.frame <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::column_names.data.frame")
  colnames(x)
}



#' Return columns used
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments (not used)
#' @param using character, if not NULL set of columns used from above.
#' @return vector of table qualified column names.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d1 <- rq_copy_to(my_db, 'd1',
#'                     data.frame(AUC = 0.6, R2 = 0.2))
#'   d2 <- rq_copy_to(my_db, 'd2',
#'                     data.frame(AUC = 0.6, D = 0.3))
#'   optree <- natural_join(d1, d2, by = "AUC")
#'   cat(format(optree))
#'   print(columns_used(optree))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
columns_used <- function (x,
                          ...,
                          using = NULL) {
  UseMethod("columns_used", x)
}

#' @export
columns_used.relop <- function (x,
                                ...,
                                using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::columns_used.relop")
  subs <- lapply(x$source,
                 columns_used)
  res <- list()
  nsubs <- length(subs)
  if(nsubs>0) {
    res <- subs[[1]]
    if(nsubs>1) {
      for(i in 2:nsubs) {
        res <- merge_columns_used(res, subs[[i]])
      }
    }
  }
  return(res)
}


#' Return vector of table names used.
#'
#' @param node rquery tree to examine.
#' @param ... (not used)
#' @return names of tables used.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d1 <- rq_copy_to(my_db, 'd1',
#'                     data.frame(AUC = 0.6, R2 = 0.2))
#'   d2 <- rq_copy_to(my_db, 'd2',
#'                     data.frame(AUC = 0.6, D = 0.3))
#'   optree <- natural_join(d1, d2, by = "AUC")
#'   cat(format(optree))
#'   print(tables_used(optree))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
tables_used <- function(node, ...) {
  UseMethod("tables_used", node)
}

#' @export
tables_used.relop <- function(node, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::tables_used.relop")
  tabs <- lapply(node$source,
                 function(si) {
                   tables_used(si)
                 })
  tabs <- sort(unique(unlist(tabs)))
  tabs
}


#' Return SQL implementation of operation tree.
#'
#' Add to last argument and pass all others through.
#'
#' @param x rquery operation tree.
#' @param db DBI database handle or rquery_db_info object.
#' @param ... generic additional arguments (not used).
#' @param limit numeric if not NULL limit result to this many rows.
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param indent_level level to indent.
#' @param tnum temp sub-query name generator.
#' @param append_cr logical if TRUE end with CR.
#' @param using character, if not NULL set of columns used from above.
#' @return SQL command
#'
#' @seealso \code{\link{db_td}}, \code{\link{materialize}}, \code{\link{execute}}, \code{\link{rq_copy_to}}, \code{\link{mk_td}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d1 <- rq_copy_to(my_db, 'd1',
#'                     data.frame(AUC = 0.6, R2 = 0.2))
#'   d2 <- rq_copy_to(my_db, 'd2',
#'                     data.frame(AUC = 0.6, D = 0.3))
#'   optree <- natural_join(d1, d2, by = "AUC")
#'   cat(format(optree))
#'   print(to_sql(optree, my_db))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
to_sql <- function (x,
                    db,
                    ...,
                    limit = NULL,
                    source_limit = NULL,
                    indent_level = 0,
                    tnum = mk_tmp_name_source('tsql'),
                    append_cr = TRUE,
                    using = NULL) {
  UseMethod("to_sql", x)
}

#' @export
#'
dimnames.relop <- function(x) {
  column_names(x)
}


#' @export
#'
dim.relop <- function(x) {
  # not populating number of rows, as that can be expensive
  c(NA_real_, length(column_names(x)))
}

#' Format a single node for printing.
#'
#' @param node node of operator tree to be formatted
#' @return character display form of the node
#'
#' @export
format_node <- function(node) {
  UseMethod("format_node", node)
}

#' @export
as.character.relop <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "as.character.relop")
  format_node(x)
}

#' @export
format.relop <- function(x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "format.relop")
  ndstr <- format_node(x)
  if(length(x$source)<1) {
    return(ndstr)
  }
  if(length(x$source)==1) {
    return(paste0(trimws(format(x$source[[1]]), which = "right"),
                  " %.>%\n ",
                  trimws(ndstr, which = "both"),
                  "\n"))
  }
  inputs <- vapply(x$source,
                   function(vi) {
                     trimws(format(vi), which = "both")
                   }, character(1))

  return(paste0("(\n ",
                paste(inputs, collapse = ",\n "),
                ") %.>%\n ",
                trimws(ndstr, which = "both"),
                "\n"))
}

