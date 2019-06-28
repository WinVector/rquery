
#' Make an orderby node (not a relational operation).
#'
#' Order a table by a set of columns (not general expressions) and
#' limit number of rows in that order.
#'
#' Note: this is a relational operator in that it takes a table that
#' is a relation (has unique rows) to a table that is still a relation.
#' However, most relational systems do not preserve row order in storage or between
#' operations.  So without the limit set this is not a useful operator except
#' as a last step prior to pulling data to an in-memory \code{data.frame} (
#' which does preserve row order).
#'
#'
#' @param source source to select from.
#' @param cols order by column names.
#' @param ... force later arguments to be bound by name
#' @param reverse character, which columns to reverse ordering of.
#' @param limit number limit row count.
#' @param env environment to look to.
#' @return order_by node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'   optree <- orderby(d, cols = "AUC", reverse = "AUC", limit=4)
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
orderby <- function(source,
                    cols = NULL,
                    ...,
                    reverse = NULL,
                    limit = NULL,
                    env = parent.frame()) {
  force(env)
  UseMethod("orderby", source)
}

#' @export
orderby.relop <- function(source,
                    cols = NULL,
                    ...,
                    reverse = NULL,
                    limit = NULL,
                    env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::orderby.relop")
  if(length(setdiff(reverse, cols))>0) {
    stop("rquery::orderby.relop all reverse columns must be in cols list")
  }
  have <- column_names(source)
  check_have_cols(have, cols, "rquery::orderby.relop")
  if(!is.null(limit)) {
    if(limit<0) {
      stop("rquery::orderby.relop limit must be >=0 or NULL")
    }
    if(length(cols)<=0) {
      stop("rquery::orderby.relop if limit is not NULL, then cols must not be empty")
    }
  }
  if(length(cols)<=0) {
    return(source)
  }
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            orderby = cols,
            reverse = reverse,
            limit = limit)
  r <- relop_decorate("relop_orderby", r)
  r
}

#' @export
orderby.data.frame <- function(source,
                    cols = NULL,
                    ...,
                    reverse = NULL,
                    limit = NULL,
                    env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::orderby.data.frame")
  if(length(setdiff(reverse, cols))>0) {
    stop("rquery::orderby.data.frame all reverse columns must also be orderby columns")
  }
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- orderby(dnode,
                   cols = cols,
                   reverse = reverse,
                   limit = limit,
                   env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}




#' @export
format_node.relop_orderby <- function(node) {
  ot <- node$orderby
  if(length(node$reverse)>0) {
    ot[ot %in% node$reverse] <- paste0("desc(", ot[ot %in% node$reverse], ")")
  }
  paste0("orderby(., ",
         ifelse(length(ot)>0,
                paste(ot, collapse = ", "),
                ""),
         ifelse((length(node$limit)>0) && (length(node$orderby)>0),
                paste0(", LIMIT ",
                       format(ceiling(node$limit), scientific = FALSE)),
                ""),
         ")",
         "\n")
}



calc_used_relop_orderby <- function (x, ...,
                                      using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::calc_used_relop_orderby")
  if(length(using)<=0) {
    using <- column_names(x)
  }
  consuming <- x$orderby
  using <- unique(c(using, consuming))
  missing <- setdiff(using, column_names(x$source[[1]]))
  if(length(missing)>0) {
    stop(paste("rquery::calc_used_relop_orderby unknown columns",
               paste(missing, collapse = ", ")))
  }
  using
}

#' @export
columns_used.relop_orderby <- function (x, ...,
                                        using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::columns_used.relop_orderby")
  cols <- calc_used_relop_orderby(x,
                                  using = using)
  return(columns_used(x$source[[1]],
                      using = cols))
}


#' @export
to_sql.relop_orderby <- function (x,
                                  db,
                                  ...,
                                  limit = NULL,
                                  source_limit = NULL,
                                  indent_level = 0,
                                  tnum = mk_tmp_name_source('tsql'),
                                  append_cr = TRUE,
                                  using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::to_sql.relop_orderby")
  dispatch_to_sql_method(
    method_name = "to_sql.relop_orderby",
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
}


to_sql_relop_orderby <- function(
  x,
  db,
  ...,
  limit = NULL,
  source_limit = NULL,
  indent_level = 0,
  tnum = mk_tmp_name_source('tsql'),
  append_cr = TRUE,
  using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::to_sql.relop_orderby")
  cols1 <- column_names(x$source[[1]])
  cols <- vapply(cols1,
                 function(ci) {
                   quote_identifier(db, ci)
                 }, character(1))
  ot <- vapply(x$orderby,
               function(ci) {
                 quote_identifier(db, ci)
               }, character(1))
  if(length(x$reverse)>0) {
    ot[x$orderby %in% x$reverse] <- paste(ot[x$orderby %in% x$reverse], "DESC")
  }
  subcols <- calc_used_relop_orderby(x, using=using)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        limit = NULL, # can't pass down limit from order_by
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = subcols)
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT * FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab,
              ifelse(length(ot)>0,
                     paste0(" ORDER BY ", paste(ot, collapse = ", ")),
                     ""))
  if(!is.null(x$limit)) {
    limit <- min(limit, x$limit)
  }
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}

