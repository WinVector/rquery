


#' project data by grouping, and adding aggregate columns.
#'
#' @param source source to select from.
#' @param groupby grouping columns.
#' @param parsed new column assignment expressions.
#' @return project node.
#'
#' @noRd
#'
project_impl <- function(source, groupby, parsed) {
  have <- column_names(source)
  check_have_cols(have, groupby, "rquery::project groupby")
  assignments <- unpack_assignments(source, parsed)
  producing <- names(assignments)
  overlap <- intersect(have, producing)
  if(length(overlap)>0) {
    stop(paste("rquery:::project_impl produced columns must be disjoint from incoming table: ",
               paste(overlap, collapse = ", ")))
  }
  r <- list(source = list(source),
            table_name = NULL,
            groupby = groupby,
            columns = c(groupby, names(assignments)),
            parsed = parsed,
            assignments = assignments)
  class(r) <- "relop_project"
  r
}

#' project data by grouping, and adding aggregate columns.
#'
#' @param source source to select from.
#' @param groupby grouping columns.
#' @param assignments new column assignment expressions.
#' @param env environment to look for values in.
#' @return project node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- project_se(d, "AUC", "v" := "max(R2)")
#' cat(format(eqn))
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
project_se <- function(source, groupby, assignments,
                    env = parent.frame()) {
  parsed <- parse_se(source, assignments, env = env)
  project_impl(source, groupby, parsed)
}

#' project data by grouping, and adding aggregate columns.
#'
#' @param source source to select from.
#' @param groupby grouping columns.
#' @param ... new column assignment expressions.
#' @param env environment to look for values in.
#' @return project node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(my_db, 'd',
#'                 data.frame(AUC = 0.6, R2 = 0.2))
#' eqn <- project_nse(d, "AUC", v := max(R2))
#' cat(format(eqn))
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
project_nse <- function(source, groupby, ...,
                    env = parent.frame()) {
  exprs <-  eval(substitute(alist(...)))
  parsed <- parse_nse(source, exprs, env = env)
  project_impl(source, groupby, parsed)
}

#' @export
quote_identifier.relop_project <- function (x, id, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_identifier(x$source[[1]], id)
}

#' @export
quote_string.relop_project <- function (x, s, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_string(x$source[[1]], s)
}

#' @export
column_names.relop_project <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  x$columns
}


#' @export
format.relop_project <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  origTerms <- vapply(x$parsed,
                      function(pi) {
                        paste(as.character(pi$presentation), collapse = ' ')
                      }, character(1))
  aterms <- paste(origTerms, collapse = ", ")
  paste0(trimws(format(x$source[[1]]), which="right"),
         " %.>%\n ",
         "project(., ",
         aterms,
         ",\n  g= ",
         paste(x$groupby, collapse = ", "),
         ")",
         "\n")
}

#' @export
print.relop_project <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  txt <- format(x)
  txt <- trimws(gsub("[ \t\r\n]+", " ", txt), which = "both")
  print(txt, ...)
}

#' @export
columns_used.relop_project <- function (x, ...,
                                        using = NULL,
                                        contract = FALSE) {
  if(length(list(...))>0) {
    stop("rquery:columns_used: unexpected arguemnts")
  }
  if(length(using)<=0) {
    using <- column_names(x)
  }
  producing <- merge_fld(x$parsed, "symbols_produced")
  expressions <- x$parsed
  if(contract) {
    expressions <- x$parsed[producing %in% using]
  }
  using <- setdiff(using, producing)
  consuming <- merge_fld(expressions, "symbols_used")
  subusing <- unique(c(using, consuming, x$groupby))
  columns_used(x$source[[1]],
               using = subusing,
               contract = contract)
}


#' @export
to_sql.relop_project <- function(x,
                                 ...,
                                 indent_level = 0,
                                 tnum = mkTempNameGenerator('tsql'),
                                 append_cr = TRUE,
                                 column_restriction = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  cols1 <- x$groupby
  cols <- NULL
  if(length(cols1)>0) {
    cols <- vapply(cols1,
                   function(ci) {
                     quote_identifier(x, ci)
                   }, character(1))
  }
  derived <- NULL
  if(length(x$assignments)>0) {
    derived <- vapply(names(x$assignments),
                      function(ni) {
                        ei <- x$assignments[[ni]]
                        paste(ei, "AS", quote_identifier(x, ni))
                      }, character(1))
  }
  subsql <- to_sql(x$source[[1]],
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = FALSE,
                   column_restriction = column_restriction)
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT ",
              paste(c(cols, derived), collapse = ", "),
              " FROM (\n",
              subsql, "\n",
              prefix, " ) ", tab)
  if(length(cols)>0) {
    q <- paste0(q,
               "\n",
               prefix, "GROUP BY\n",
               prefix, " ", paste(cols, collapse = " AND "))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
