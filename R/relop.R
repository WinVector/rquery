
# define relational op, basis for this package.
# in addition to methods below, our nodes implement: format() and print().
#
# Each node should be a list with named entries:
#   source: list of nodes supplying values to this node.
#   table_name: character name of table if a concrete table or view.
#   parsed: list of parsed expressions.


# add class info and helpers to a relop node.
relop_decorate <- function(class_name, r) {
  class(r) <- c(class_name, "relop", "wrapr_applicable")
  r$wrapr_function <- rquery_apply_to_data_frame
  r
}

#' Return column names
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments
#' @return vector of column names
#'
#' @export
#'
column_names <- function (x, ...) {
  UseMethod("column_names", x)
}

#' @export
column_names.relop <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  subs <- lapply(x$source,
                 column_names)
  return(sort(unique(unlist(subs))))
}



#' Return columns used
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments (not used)
#' @param using character, if not NULL set of columns used from above.
#' @param contract logical, if TRUE perform unused value elimination.
#' @return vector of table qualified column names.
#'
#' @export
#'
columns_used <- function (x,
                          ...,
                          using = NULL,
                          contract = FALSE) {
  UseMethod("columns_used", x)
}

#' @export
columns_used.relop <- function (x,
                                ...,
                                using = NULL,
                                contract = FALSE) {
  if(length(list(...))>0) {
    stop("rquery:columns_used: unexpected arguemnts")
  }
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
#' @return named map of tables used.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d1 <- dbi_copy_to(my_db, 'd1',
#'                  data.frame(AUC = 0.6, R2 = 0.2))
#' d2 <- dbi_copy_to(my_db, 'd2',
#'                  data.frame(AUC = 0.6, D = 0.3))
#' eqn <- natural_join(d1, d2)
#' cat(format(eqn))
#' print(tables_used(eqn))
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
tables_used <- function(node, ...) {
  UseMethod("tables_used", node)
}

#' @export
tables_used.relop <- function(node, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  r <- list()
  for(si in node$source) {
    ui <- tables_used(si, ...)
    for(ki in names(ui)) {
      r[[ki]] <- ui[[ki]]
    }
  }
  r
}


#' Return SQL implementation of operation tree.
#'
#' @param x rquery operation tree.
#' @param db DBI database handle or rquery_db_info object.
#' @param ... generic additional arguments (not used).
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param indent_level level to indent.
#' @param tnum temp sub-query name generator.
#' @param append_cr logical if TRUE end with CR.
#' @param using character, if not NULL set of columns used from above.
#' @return SQL command
#'
#' @export
#'
to_sql <- function (x,
                    db,
                    ...,
                    source_limit = NULL,
                    indent_level = 0,
                    tnum = mkTempNameGenerator('tsql'),
                    append_cr = TRUE,
                    using = NULL) {
  UseMethod("to_sql", x)
}


#' Return SQL implementation of operation tree.
#'
#' @param x rquery operation tree.
#' @param ... generic additional arguments (not used).
#' @return pre_sql_op node
#'
#' @export
#'
to_pre_sql <- function (x,
                        ...) {
  UseMethod("to_pre_sql", x)
}


#' @export
#'
dim.relop <- function(x) {
  c(NA_real_, length(column_names(x)))
}


