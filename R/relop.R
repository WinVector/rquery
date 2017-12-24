
# define relational op, basis for this package.
# in addition to methods below, our nodes implement: format() and print().
#
# Each node should be a list with named entries:
#   source: list of nodes supplying values to this node.
#   table_name: character name of table if a concrete table or view.
#   parsed: list of parsed expressions.



#' Quote an idnetifier.
#'
#' @param x rquery operation tree.
#' @param id character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted identifier
#'
#' @export
#'
quote_identifier <- function (x, id, ...) {
  UseMethod("quote_identifier", x)
}

#' @export
quote_identifier.relop <- function (x, id, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_identifier(x$source[[1]], id)
}



#' Quote a string
#'
#' @param x rquery operation tree.
#' @param s character to quote
#' @param ... generic additional arguments (not used)
#' @return quoted string
#'
#' @export
#'
quote_string <- function (x, s, ...) {
  UseMethod("quote_string", x)
}

#' @export
quote_string.relop <- function (x, s, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_string(x$source[[1]], s)
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
columns_used <- function (x, ...,
                          using = NULL,
                          contract = FALSE) {
  UseMethod("columns_used", x)
}

#' @export
columns_used.relop <- function (x, ...,
                                       using = NULL,
                                       contract = FALSE) {
  if(length(list(...))>0) {
    stop("rquery:columns_used: unexpected arguemnts")
  }
  subs <- lapply(x$source,
                 columns_used)
  return(sort(unique(unlist(subs))))
}


#' Return vector of table names used.
#'
#' @param node rquery tree to examine
#' @return character vector of tables referenced in calculation.
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
tables_used <- function(node) {
  UseMethod("tables_used", node)
}

#' @export
tables_used.relop <- function(node) {
  subs <- lapply(node$source,
                 tables_used)
  return(sort(unique(c(node$table_name, unlist(subs)))))
}

#' @export
print.relop <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  txt <- format(x)
  txt <- trimws(gsub("[ \t\r\n]+", " ", txt), which = "both")
  print(txt, ...)
}

#' Return SQL implementation of operation tree.
#'
#' @param x rquery operation tree.
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

