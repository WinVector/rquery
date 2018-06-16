


#' Make an set indicator node.
#'
#' Create a new column indicating the membership of another column in a given set.
#'
#'
#' @param source source to select from.
#' @param rescol name of column to land indicator in.
#' @param testcol name of column to check.
#' @param testvalues values to check for.
#' @return set_indicator node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(a = c("1", "2", "1", "3"),
#'                               b = c("1", "1", "3", "2"),
#'                               q = 1,
#'                               stringsAsFactors = FALSE),
#'                    temporary = TRUE,
#'                    overwrite = TRUE)
#'   # example
#'   set <- c("1", "2")
#'   op_tree <- d %.>%
#'     set_indicator(., "one_two", "a", set) %.>%
#'     set_indicator(., "z", "a", c())
#'   print(column_names(op_tree))
#'   print(columns_used(op_tree))
#'   cat(format(op_tree))
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   op_tree2 <- d %.>%
#'     set_indicator(., "one_two", "a", set) %.>%
#'     set_indicator(., "z", "b", c()) %.>%
#'     select_columns(., c("z", "one_two"))
#'   print(column_names(op_tree2))
#'   print(columns_used(op_tree2))
#'
#'   # cleanup
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
set_indicator <- function(source,
                          rescol,
                          testcol,
                          testvalues) {
  UseMethod("set_indicator", source)
}

#' @export
set_indicator.relop <- function(source,
                                rescol,
                                testcol,
                                testvalues) {
  testvname <- rquery_deparse(substitute(testvalues))
  cols <- column_names(source)
  if(rescol %in% cols) {
    stop("rquery::set_indicator.relop rescol must not be a column name of source data")
  }
  if(!(testcol %in% cols)) {
    stop("rquery::set_indicator.relop testcol must be a column name of source data")
  }
  display_form <- paste0("set_indicator(., ",
                         rescol,
                         " = ",
                         testcol,
                         " IN ",
                         testvname,
                         ")")
  if(length(testvalues)>0) {
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
  } else {
    terms <- rescol %:=% 0
  }
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            rescol = rescol,
            testcol = testcol,
            testvalues = testvalues,
            display_form = display_form,
            terms = terms)
  r <- relop_decorate("relop_set_indicator", r)
  r
}

#' @export
set_indicator.data.frame <- function(source,
                                     rescol,
                                     testcol,
                                     testvalues) {
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- set_indicator(source = dnode,
                         rescol = rescol,
                         testcol = testcol,
                         testvalues = testvalues)
  return(enode)
}




#' @export
format_node.relop_set_indicator <- function(node) {
  paste0(node$display_form,
         "\n")
}





calc_used_relop_set_indicator <- function(x,
                                          using = NULL) {
  cols <- column_names(x)
  if(length(using)>0) {
    cols <- using
  }
  if(!(x$testcol %in% cols)) {
    cols <- c(cols, x$testcol)
  }
  cols
}

#' @export
columns_used.relop_set_indicator <- function (x, ...,
                                              using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::columns_used.relop_set_indicator")
  cols <- calc_used_relop_set_indicator(x,
                                        using = using)
  cols <- setdiff(cols, x$rescol)
  columns_used(x$source[[1]],
               using = cols)
}



#' @export
column_names.relop_set_indicator <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::column_names.relop_set_indicator")
  cols <- column_names(x$source[[1]])
  if(!(x$testcol %in% cols)) {
    cols <- c(cols, x$testcol)
  }
  if(!(x$rescol %in% cols)) {
    cols <- c(cols, x$rescol)
  }
  cols
}


#' @export
to_sql.relop_set_indicator <- function (x,
                                        db,
                                        ...,
                                        limit = NULL,
                                        source_limit = NULL,
                                        indent_level = 0,
                                        tnum = mk_tmp_name_source('tsql'),
                                        append_cr = TRUE,
                                        using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::to_sql.relop_set_indicator")
  cols1 <- column_names(x$source[[1]])
  sqlexprs <- vapply(x$terms,
                     function(ei) {
                       prep_sql_toks(db, ei)
                     }, character(1))
  if(length(sqlexprs)!=1) {
    stop("rquery::to_sql.relop_set_indicator expected indicator calculation to be length 1")
  }
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        limit = limit,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = cols1)  # TODO: double check using calculation
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT *, ",
              sqlexprs[[1]], " AS ", quote_identifier(db, names(sqlexprs)),
              " FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}


