
#' Make a theta_join node.
#'
#' Theta join is a join on an arbitrary predicate.
#'
#' @param a source to select from.
#' @param b source to select from.
#' @param expr quoated join condition
#' @param ... force later arguments to be by name
#' @param jointype type of join ('INNER', 'LEFT', 'RIGHT', 'FULL').
#' @param env environment to look for values in.
#' @return theta_join node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d1 <- dbi_copy_to(my_db, 'd1',
#'                  data.frame(AUC = 0.6, R2 = 0.2))
#' d2 <- dbi_copy_to(my_db, 'd2',
#'                  data.frame(AUC2 = 0.4, D = 0.3))
#' eqn <- theta_join_se(d1, d2, "AUC >= AUC2")
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' @export
#'
theta_join_se <- function(a, b,
                          expr,
                          ...,
                          jointype = 'INNER',
                          env = parent.frame()) {
  usesa <- column_names(a)
  usesb <- column_names(b)
  overlap = intersect(usesa, usesb)
  if(length(overlap)>0) {
    stop("rquery::theta_join_se does not currently support duplicate column names")
  }
  have = unique(c(usesa, usesb))
  vnam <- setdiff(paste("rquery_thetajoin_condition",
                        1:(length(have)+1), sep = "_"),
                  have)[[1]]
  parsed <- parse_se(a, vnam := expr,
                     env = env,
                     have = have)
  assignments <- unpack_assignments(a, parsed,
                                    have = have)
  parsed <- parsed[[1]]
  parsed$symbols_produced <- character(0)
  parsed$presentation <- gsub("^.*:= ", "", parsed$presentation)
  r <- list(source = list(a, b),
            overlap = overlap,
            jointype = jointype,
            parsed = parsed)
  class(r) <- "relop_theta_join"
  r
}


#' Make a theta_join node.
#'
#' Theta join is a join on an arbitrary predicate.
#'
#' @param a source to select from.
#' @param b source to select from.
#' @param expr unquoated join condition
#' @param ... force later arguments to be by name
#' @param jointype type of join ('INNER', 'LEFT', 'RIGHT', 'FULL').
#' @param env environment to look for values in.
#' @return theta_join node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d1 <- dbi_copy_to(my_db, 'd1',
#'                  data.frame(AUC = 0.6, R2 = 0.2))
#' d2 <- dbi_copy_to(my_db, 'd2',
#'                  data.frame(AUC2 = 0.4, D = 0.3))
#' eqn <- theta_join_nse(d1, d2, AUC >= AUC2)
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#'
#' @export
#'
theta_join_nse <- function(a, b,
                          expr,
                          ...,
                          jointype = 'INNER',
                          env = parent.frame()) {
  usesa <- column_names(a)
  usesb <- column_names(b)
  overlap = intersect(usesa, usesb)
  if(length(overlap)>0) {
    stop("rquery::theta_join_se does not currently support duplicate column names")
  }
  have = unique(c(usesa, usesb))
  vnam <- setdiff(paste("rquery_thetajoin_condition",
                        1:(length(have)+1), sep = "_"),
                  have)[[1]]
  exprq <- substitute(expr)
  parsed <- parse_nse(a, list(exprq),
                      env = env,
                      have = have)
  parsed[[1]]$symbols_produced <- vnam
  assignments <- unpack_assignments(a, parsed,
                                    have = have)
  parsed <- parsed[[1]]
  parsed$symbols_produced <- character(0)
  parsed$presentation <- gsub("^.*:= ", "", parsed$presentation)
  r <- list(source = list(a, b),
            overlap = overlap,
            jointype = jointype,
            parsed = parsed)
  class(r) <- "relop_theta_join"
  r
}


#' @export
dbi_connection.relop_theta_join <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  dbi_connection(x$source[[1]])
}


#' @export
column_names.relop_theta_join <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  unique(c(column_names(x$source[[1]]),
           column_names(x$source[[2]])))
}


#' @export
format.relop_theta_join <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  a <- format(x$source[[1]])
  b <- format(x$source[[2]])
  paste0(a,
         " %.>% ",
         "theta_join(., ",
         b,
         "; ",
         x$jointype,
         "; on ",
         paste(x$parsed$presentation, collapse = ", "),
         ")")
}

#' @export
print.relop_theta_join <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  print(format(x),...)
}



#' @export
to_sql.relop_theta_join <- function(x,
                                      indent_level = 0,
                                      tnum = mkTempNameGenerator('tsql'),
                                      append_cr = TRUE,
                                      ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  db <- dbi_connection(x)
  subsqla <- to_sql(x$source[[1]],
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE)
  subsqlb <- to_sql(x$source[[2]],
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE)
  taba <- tnum()
  tabb <- tnum()
  bterms <- setdiff(column_names(x$source[[1]]),
                    column_names(x$source[[2]]))
  if(length(bterms)>0) {
    bcols <- vapply(bterms,
                    function(ci) {
                      DBI::dbQuoteIdentifier(db, ci)
                    }, character(1))
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", taba,".*,\n",
              prefix, " ", tabb, ".*\n",
              prefix, "FROM (\n",
              subsqla, "\n",
              prefix, ") ",
              taba, "\n",
              prefix, x$jointype,
              " JOIN (\n",
              subsqlb, "\n",
              prefix, ") ",
              tabb,
              " ON ",
              x$parsed$parsed)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
