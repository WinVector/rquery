
#' Make a select rows node.
#'
#' @param source source to select from.
#' @param expr expression to select rows.
#' @param env environment to look for values in.
#' @return select columns node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- select_rows_se(d, "AUC >= 0.5")
#' cat(format(eqn))
#' sql <- to_sql(eqn, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
select_rows_se <- function(source, expr,
                           env = parent.frame()) {
  UseMethod("select_rows_se", source)
}

#' @export
select_rows_se.relop <- function(source, expr,
                                 env = parent.frame()) {
  have <- column_names(source)
  vnam <- setdiff(paste("rquery_select_condition", 1:(length(have)+1), sep = "_"),
                  have)[[1]]
  parsed <- parse_se(source, vnam := expr, env = env)
  assignments <- unpack_assignments(source, parsed)
  parsed[[1]]$symbols_produced <- character(0)
  r <- list(source = list(source),
            table_name = NULL,
            parsed = parsed,
            expr = assignments)
  r <- relop_decorate("relop_select_rows", r)
  r
}

#' @export
select_rows_se.data.frame <- function(source, expr,
                                      env = parent.frame()) {
  tmp_name <- mkTempNameGenerator("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- select_rows_se(dnode, expr,
                          env = env)
  return(enode)
}



#' Make a select rows node.
#'
#' @param source source to select from.
#' @param expr expression to select rows.
#' @param env environment to look for values in.
#' @return select columns node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2, z = 3))
#' eqn <- select_rows_nse(d, AUC >= 0.5) %.>%
#'    select_columns(., "R2")
#' cat(format(eqn))
#' sql <- to_sql(eqn, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
select_rows_nse <- function(source, expr,
                            env = parent.frame()) {
  UseMethod("select_rows_nse", source)
}

#' @export
select_rows_nse.relop <- function(source, expr,
                            env = parent.frame()) {
  exprq <- substitute(expr)
  have <- column_names(source)
  vnam <- setdiff(paste("rquery_select_condition", 1:(length(have)+1), sep = "_"),
                  have)[[1]]
  parsed <- parse_nse(source, list(exprq), env = env)
  parsed[[1]]$symbols_produced <- vnam
  assignments <- unpack_assignments(source, parsed)
  parsed[[1]]$symbols_produced <- character(0)
  r <- list(source = list(source),
            parsed = parsed,
            expr = assignments)
  r <- relop_decorate("relop_select_rows", r)
  r
}

#' @export
select_rows_nse.data.frame <- function(source, expr,
                            env = parent.frame()) {
  exprq <- substitute(expr)
  tmp_name <- mkTempNameGenerator("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- select_rows_se(dnode, deparse(exprq),
                          env = env)
  return(enode)
}




#' @export
format.relop_select_rows <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  paste0(trimws(format(x$source[[1]]), which="right"),
         " %.>%\n ",
         "select_rows(., ", x$parsed[[1]]$presentation, ")",
         "\n")
}


calc_used_relop_select_rows <- function (x, ...,
                                         using = NULL,
                                         contract = FALSE) {
  if(length(using)<=0) {
    using <- column_names(x)
  }
  consuming <- merge_fld(x$parsed, "symbols_used")
  using <- unique(c(using, consuming))
  missing <- setdiff(using, column_names(x$source[[1]]))
  if(length(missing)>0) {
    stop(paste("rquery::calc_used_relop_select_rows unknown columns",
               paste(missing, collapse = ", ")))
  }
  using
}

#' @export
columns_used.relop_select_rows <- function (x, ...,
                                         using = NULL,
                                         contract = FALSE) {
  cols <- calc_used_relop_select_rows(x,
                                      using = using,
                                      contract = contract)
  return(columns_used(x$source[[1]],
                      using = cols,
                      contract = contract))
}


#' @export
to_sql.relop_select_rows <- function (x,
                                      db,
                                      ...,
                                      source_limit = NULL,
                                      indent_level = 0,
                                      tnum = mkTempNameGenerator('tsql'),
                                      append_cr = TRUE,
                                      using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  # re-quote expr
  re_quoted <- redo_parse_quoting(x$parsed, db)
  re_expr <- unpack_assignments(x$source[[1]], re_quoted,
                                check_is_assignment = FALSE)
  # work on query
  cols <- calc_used_relop_select_rows(x,
                                      using = using)
  subsql <- to_sql(x$source[[1]],
                   db = db,
                   source_limit = source_limit,
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE,
                   using = cols)
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT * FROM (\n",
         subsql, "\n",
         prefix, ") ",
         tab, "\n",
         prefix, "WHERE ",
         re_expr)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
