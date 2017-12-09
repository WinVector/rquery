
#' Make a theta_join node.
#'
#' Theta join is a join on an arbitrary predicate.
#'
#' @param a source to select from.
#' @param b source to select from.
#' @param expr quoated join condition
#' @param ... force later arguments to be by name
#' @param jointype type of join ('INNER', 'LEFT', 'RIGHT', 'FULL').
#' @param suffix suffix to disambiguate columns
#' @param env environment to look for values in.
#' @return theta_join node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d1 <- dbi_copy_to(my_db, 'd1',
#'                  data.frame(AUC = 0.6, R2 = 0.2))
#' d2 <- dbi_copy_to(my_db, 'd2',
#'                  data.frame(AUC2 = 0.4, R2 = 0.3))
#' eqn <- theta_join_se(d1, d2, "AUC >= AUC2")
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
theta_join_se <- function(a, b,
                          expr,
                          ...,
                          jointype = 'INNER',
                          suffix = c("_a", "_b"),
                          env = parent.frame()) {
  usesa <- column_names(a)
  usesb <- column_names(b)
  overlap = intersect(usesa, usesb)
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
            table_name = NULL,
            overlap = overlap,
            jointype = jointype,
            parsed = parsed,
            suffix = suffix)
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
#' @param suffix suffix to disambiguate columns
#' @param env environment to look for values in.
#' @return theta_join node.
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d1 <- dbi_copy_to(my_db, 'd1',
#'                  data.frame(AUC = 0.6, R2 = 0.2))
#' d2 <- dbi_copy_to(my_db, 'd2',
#'                  data.frame(AUC2 = 0.4, R2 = 0.3))
#' eqn <- theta_join_nse(d1, d2, AUC >= AUC2)
#' print(eqn)
#' sql <- to_sql(eqn)
#' cat(sql)
#' DBI::dbGetQuery(my_db, sql)
#' DBI::dbDisconnect(my_db)
#'
#' @export
#'
theta_join_nse <- function(a, b,
                          expr,
                          ...,
                          jointype = 'INNER',
                          suffix = c("_a", "_b"),
                          env = parent.frame()) {
  usesa <- column_names(a)
  usesb <- column_names(b)
  overlap = intersect(usesa, usesb)
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
            parsed = parsed,
            suffix = suffix)
  class(r) <- "relop_theta_join"
  r
}


#' @export
quote_identifier.relop_theta_join <- function (x, id, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_identifier(x$source[[1]], id)
}

#' @export
quote_string.relop_theta_join <- function (x, s, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  quote_string(x$source[[1]], s)
}

#' @export
column_names.relop_theta_join <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  c1 <- column_names(x$source[[1]])
  c2 <- column_names(x$source[[2]])
  if(length(x$overlap)>0) {
    c1[c1 %in% x$overlap] <- paste0(c1[c1 %in% x$overlap], x$suffix[[1]])
    c2[c2 %in% x$overlap] <- paste0(c2[c2 %in% x$overlap], x$suffix[[2]])
  }
  c(c1, c2)
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


prepColumnNames <- function(x, tabName, tabColumns, ambiguous, suffix) {
  tabColumnsV <- vapply(tabColumns,
                        function(ci) {
                          quote_identifier(x, ci)
                        }, character(1))
  tabColumnsV <- paste(tabName, tabColumnsV, sep = ".")
  tabColumnsA <- tabColumns
  needsFix <- which(tabColumns %in% ambiguous)
  if(length(needsFix)>0) {
    tabColumnsA[needsFix] <- paste0(tabColumnsA[needsFix], suffix)
  }
  tabColumnsA <- vapply(tabColumnsA,
                        function(ci) {
                          quote_identifier(x, ci)
                        }, character(1))
  paste(tabColumnsV, "AS", tabColumnsA)
}

#' @export
columns_used.relop_theta_join <- function (x, ...,
                                           using = NULL,
                                           contract = FALSE) {
  if(length(using)<=0) {
    s1 <- columns_used(x$source[[1]],
                       using = NULL,
                       contract = contract)
    s2 <- columns_used(x$source[[2]],
                       using = NULL,
                       contract = contract)
    return(unique(c(s1, s2)))
  }
  condTerms <- merge_fld(x$parsed, "symbols_used")
  c1 <- unique(c(condTerms,
                 intersect(using, column_names(x$source[[1]]))))
  s1 <- columns_used(x$source[[1]],
                     using = c1,
                     contract = contract)
  c2 <- unique(c(condTerms,
                 intersect(using, column_names(x$source[[1]]))))
  s2 <- columns_used(x$source[[2]],
                     using = c2,
                     contract = contract)
  return(unique(c(s1, s2)))
}

#' @export
to_sql.relop_theta_join <- function(x,
                                    ...,
                                    indent_level = 0,
                                    tnum = mkTempNameGenerator('tsql'),
                                    append_cr = TRUE,
                                    column_restriction = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  subsqla <- to_sql(x$source[[1]],
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE,
                    column_restriction = column_restriction)
  subsqlb <- to_sql(x$source[[2]],
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE,
                    column_restriction = column_restriction)
  taba <- tnum()
  tabb <- tnum()
  bterms <- setdiff(column_names(x$source[[1]]),
                    column_names(x$source[[2]]))
  if(length(bterms)>0) {
    bcols <- vapply(bterms,
                    function(ci) {
                      quote_identifier(x, ci)
                    }, character(1))
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  cseta <- prepColumnNames(x, taba, column_names(x$source[[1]]),
                          x$overlap, x$suffix[[1]])
  ctermsa <- paste(cseta, collapse = paste0(",\n", prefix, " "))
  csetb <- prepColumnNames(x, tabb, column_names(x$source[[2]]),
                          x$overlap, x$suffix[[2]])
  ctermsb <- paste(csetb, collapse = paste0(",\n", prefix, " "))
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", ctermsa, ",\n",
              prefix, " ", ctermsb, "\n",
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
