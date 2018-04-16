

#' Create a null_replace node.
#'
#' Replace NA/NULL is specified columns with the given replacement value.
#'
#' @param src relop or data.frame data source
#' @param cols character, columns to work on
#' @param value scalar, value to write
#' @param ... force later arguments to bind by name
#' @return null_replace node.
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d1 <- dbi_copy_to(my_db, 'd1',
#'                     data.frame(A = c(NA, 2), B = c(3, NA)))
#'   optree <- null_replace(d1, qc(A, B), 0.0)
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
null_replace <- function(src,
                         cols,
                         value,
                         ...) {
  UseMethod("null_replace", src)
}

#' @export
null_replace.relop <- function(src,
                               cols,
                               value,
                               ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::null_replace.relop")
  if(length(sort(unique(cols)))!=length(cols)) {
    stop("rquery::null_replace.relop bad cols argument")
  }
  if(length(value)!=1) {
    stop("rquery::null_replace.relop value must be a scalar")
  }
  bads <- setdiff(cols, column_names(src))
  if(length(bads)>0) {
    stop(paste("rquery::null_replace.relop unknown columns",
               paste(bads, collapse = ", ")))
  }
  r <- list(source = list(src),
            table_name = NULL,
            cols = cols,
            value = value,
            parsed = NULL)
  r <- relop_decorate("relop_null_replace", r)
  r
}

#' @export
null_replace.data.frame <- function(src,
                                    cols,
                                    value,
                                    ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::null_replace.data.frame")
  nmgen <- mk_tmp_name_source("rquery_tmp")
  tmp_namea <- nmgen()
  dnodea <- table_source(tmp_namea, colnames(a))
  dnodea$data <- a
  enode <- null_replace(dnodea,
                        cols = cols,
                        value = value)
  return(enode)
}


#' @export
format_node.relop_null_replace <- function(node) {
  paste0("null_replace(.; ",
         paste(node$cols, collapse = ", "),
         " : ",
         node$value,
         ")",
         "\n")
}

calc_used_relop_null_replace <- function (x, ...,
                                          using = NULL,
                                          contract = FALSE) {
  column_names(x$src)
}

#' @export
columns_used.relop_null_replace <- function (x, ...,
                                             using = NULL,
                                             contract = FALSE) {
  calc_used_relop_null_replace(x)
}


#' @export
to_sql.relop_null_replace <- function (x,
                                       db,
                                       ...,
                                       limit = NULL,
                                       source_limit = NULL,
                                       indent_level = 0,
                                       tnum = mk_tmp_name_source('tsql'),
                                       append_cr = TRUE,
                                       using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  qlimit = limit
  if(!getDBOption(db, "use_pass_limit", TRUE)) {
    qlimit = NULL
  }
  subsqla_list <- to_sql(x$source[[1]],
                         db = db,
                         limit = qlimit,
                         source_limit = source_limit,
                         indent_level = indent_level + 1,
                         tnum = tnum,
                         append_cr = FALSE,
                         using = using)
  subsqla <- subsqla_list[[length(subsqla_list)]]
  prefix <- paste(rep(' ', indent_level), collapse = '')
  tab <- tnum()
  cols <- column_names(x$source[[1]])
  qnames <- vapply(cols,
                   function(ci) {
                     DBI::dbQuoteIdentifier(db, ci)
                   }, character(1))
  tqnames <- paste0(DBI::dbQuoteIdentifier(db, tab),
                    ".",
                    qnames)
  qexpr <- tqnames
  alter <- which(cols %in% x$cols)
  if(length(alter)>0) {
    qexpr[alter] <- paste0("CASE WHEN ",
                           tqnames[alter],
                           " IS NULL THEN ",
                           DBI::dbQuoteLiteral(db, x$value),
                           " ELSE ",
                           tqnames[alter],
                           " END")
  }
  qexpr <- paste(qexpr, "AS", qnames)
  texpr <- paste(qnames = qexpr)
  texpr <- paste(texpr, collapse = paste0(",\n ", prefix))
  q <- paste0(prefix, "SELECT\n",
              " ", prefix, texpr, "\n",
              prefix, "FROM (\n",
              subsqla, "\n",
              prefix, ") ",
              tab)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsqla_list[-length(subsqla_list)],
    q)
}
