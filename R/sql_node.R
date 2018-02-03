

#' Make a general SQL node.
#'
#' @param source source to rename from.
#' @param exprs SQL expressions
#' @param mods SQL modifiers (GROUP BY, ORDER BY, and so on)
#' @return sql node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2, z = 3))
#' eqn <- sql_node(d, "n" := "COUNT(1)")
#' cat(format(eqn))
#' sql <- to_sql(eqn, my_db)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
sql_node <- function(source, exprs, mods = NULL) {
  UseMethod("sql_node", source)
}

#' @export
sql_node.relop <- function(source, exprs, mods = NULL) {
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            exprs = exprs,
            mods = mods)
  r <- relop_decorate("relop_sql", r)
  r
}

#' @export
sql_node.data.frame <- function(source, exprs, mods = NULL) {
  tmp_name <- mkTempNameGenerator("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- sql_node(dnode, exprs = exprs, mods = mods)
  return(enode)
}




#' @export
column_names.relop_sql <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  names(x$exprs)
}



#' @export
format.relop_sql <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  assignments <- paste(names(x$exprs), ":=", as.character(x$exprs))
  modsstr <- ""
  if(!is.null(x$mods)) {
    modsstr <- paste(";\n          ", x$mods)
  }
  paste0(trimws(format(x$source[[1]]), which = "right"),
         " %.>%\n ",
         "sql_node(.,\n",
         "          ", paste(assignments, collapse = "\n             "),
         modsstr,
         ")\n")
}



#' @export
columns_used.relop_sql <- function (x, ...,
                                    using = NULL,
                                    contract = FALSE) {
  # assume using all columns
  return(columns_used(x$source[[1]],
                      using = NULL,
                      contract = contract))
}


#' @export
to_sql.relop_sql <- function (x,
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
  colsA <- vapply(names(x$exprs),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  cols <- paste(as.character(x$exprs), "AS", colsA)
  subsql <- to_sql(x$source[[1]],
                   db = db,
                   source_limit = source_limit,
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE,
                   using = NULL)
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", paste(cols, collapse = paste0(",\n", prefix, " ")), "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(!is.null(x$mods)) {
    q <- paste(q, x$mods)
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}

#' @export
#'
dim.relop_sql <- function(x) {
  c(NA_real_, NA_real_)
}

