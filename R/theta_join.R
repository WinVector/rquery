
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
#' cat(format(eqn))
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
  parsed[[1]]$symbols_produced <- character(0)
  parsed[[1]]$presentation <- gsub("^.*:= ", "", parsed[[1]]$presentation)
  r <- list(source = list(a, b),
            table_name = NULL,
            overlap = overlap,
            jointype = jointype,
            parsed = parsed,
            suffix = suffix)
  class(r) <- c("relop_theta_join", "relop")
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
#' cat(format(eqn))
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
  parsed[[1]]$symbols_produced <- character(0)
  parsed[[1]]$presentation <- gsub("^.*:= ", "", parsed[[1]]$presentation)
  r <- list(source = list(a, b),
            overlap = overlap,
            jointype = jointype,
            parsed = parsed,
            suffix = suffix)
  class(r) <- c("relop_theta_join", "relop")
  r
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
  a <- trimws(format(x$source[[1]]), which = "right")
  b <- trimws(format(x$source[[2]]), which = "right")
  b <- gsub("\n", "\n  ", b, fixed = TRUE)
  paste0(a,
         " %.>%\n ",
         "theta_join(.,\n",
         "  ", b, ",\n",
         "  j= ",
         x$jointype,
         "; on= ",
         paste(x$parsed[[1]]$presentation, collapse = ", "),
         ")",
         "\n")
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

calc_used_relop_theta_join <- function (x, ...,
                                        using = NULL,
                                        contract = FALSE) {
  cols <- unique(c(column_names(x$source[[1]]),
                   column_names(x$source[[2]])))
  if(length(using)>0) {
    mpback <- c(column_names(x$source[[1]]),
                column_names(x$source[[2]]))
    names(mpback) <- column_names(x)
    using <- unique(mpback[using])
    missing <- setdiff(using, cols)
    if(length(missing)>0) {
      stop(paste("rquery::relop_theta_join unkown columns",
                 paste(missing, collapse = ", ")))

    }
    cols <- using
  }
  condTerms <- x$parsed[[1]]$symbols_used
  cols <- unique(c(cols, condTerms))
  cols
}


#' @export
columns_used.relop_theta_join <- function (x, ...,
                                           using = NULL,
                                           contract = FALSE) {
  using <- calc_used_relop_theta_join(x,
                                      using=using,
                                      contract = contract)
  c1 <- intersect(using, column_names(x$source[[1]]))
  s1 <- columns_used(x$source[[1]],
                     using = c1,
                     contract = contract)
  c2 <- intersect(using, column_names(x$source[[2]]))
  s2 <- columns_used(x$source[[2]],
                     using = c2,
                     contract = contract)
  return(unique(c(s1, s2)))
}

#' @export
to_sql.relop_theta_join <- function (x,
                                     ...,
                                     indent_level = 0,
                                     tnum = mkTempNameGenerator('tsql'),
                                     append_cr = TRUE,
                                     using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguemnts")
  }
  using <- calc_used_relop_theta_join(x,
                                      using=using)
  c1 <- intersect(using, column_names(x$source[[1]]))
  c2 <- intersect(using, column_names(x$source[[2]]))
  subsqla <- to_sql(x$source[[1]],
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE,
                    using = c1)
  subsqlb <- to_sql(x$source[[2]],
                    indent_level = indent_level + 1,
                    tnum = tnum,
                    append_cr = FALSE,
                    using = c2)
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
              x$parsed[[1]]$parsed)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}
