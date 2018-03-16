
#' build a map of original column names to unambigous column names
#'
#' @param colsa character columns from table a
#' @param colsb character columns from table b
#' @param suffix  character length 2, suffices to disambiguate columns.
#' @return list length 2 of name column lists
#'
#' # build_col_name_map(c("a", "a_a"), c("a"), c("_a", ""))
#'
#' @noRd
#'
build_col_name_map <- function(colsa, colsb, suffix) {
  if(suffix[[1]]==suffix[[2]]) {
    stop("rquery::build_col_name_map suffix entries must differ")
  }
  mapa <- colsa
  names(mapa) <- colsa
  mapb <- colsb
  names(mapb) <- colsb
  overlap = intersect(colsa, colsb)
  for(oi in overlap) {
    oia <- paste0(oi, suffix[[1]])
    oib <- paste0(oi, suffix[[2]])
    ova <- oia
    ovb <- oib
    retry_count = 1
    while(TRUE) {
      others <- unique(c(mapa[setdiff(colsa, oi)],
                         mapb[setdiff(colsa, oi)]))
      if(length(intersect(others, c(ova, ovb)))<=0) {
        break
      }
      ova <- paste(oia, retry_count, sep = "_")
      ovb <- paste(oib, retry_count, sep = "_")
    }
    mapa[[oi]] <- ova
    mapb[[oi]] <- ovb
  }
  list("a" = mapa, "b" = mapb)
}

#' Make a theta_join node.
#'
#' Theta join is a join on an arbitrary predicate.
#'
#' @param a source to select from.
#' @param b source to select from.
#' @param expr quoated join condition
#' @param ... force later arguments to be by name
#' @param jointype type of join ('INNER', 'LEFT', 'RIGHT', 'FULL').
#' @param suffix character length 2, suffices to disambiguate columns.
#' @param env environment to look for values in.
#' @return theta_join node.
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d1 <- dbi_copy_to(my_db, 'd1',
#'                     data.frame(AUC = 0.6, R2 = 0.2))
#'   d2 <- dbi_copy_to(my_db, 'd2',
#'                     data.frame(AUC2 = 0.4, R2 = 0.3))
#'   eqn <- theta_join_se(d1, d2, "AUC >= AUC2")
#'   cat(format(eqn))
#'   sql <- to_sql(eqn, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
theta_join_se <- function(a, b,
                          expr,
                          ...,
                          jointype = 'INNER',
                          suffix = c("_a", "_b"),
                          env = parent.frame()) {
  UseMethod("theta_join_se", a)
}

#' @export
theta_join_se.relop <- function(a, b,
                                expr,
                                ...,
                                jointype = 'INNER',
                                suffix = c("_a", "_b"),
                                env = parent.frame()) {
  if(length(list(...))>0) {
    stop("rquery::theta_join_se unexpected arguments")
  }
  if(!("relop" %in% class(b))) {
    stop("rquery::theta_join_se.relop b must also be of class relop")
  }
  usesa <- column_names(a)
  usesb <- column_names(b)
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
            cmap = build_col_name_map(usesa, usesb, suffix),
            jointype = jointype,
            parsed = parsed,
            suffix = suffix)
  r <- relop_decorate("relop_theta_join", r)
  r
}


#' @export
theta_join_se.data.frame <- function(a, b,
                                     expr,
                                     ...,
                                     jointype = 'INNER',
                                     suffix = c("_a", "_b"),
                                     env = parent.frame()) {
  if(length(list(...))>0) {
    stop("rquery::theta_join_se unexpected arguments")
  }
  if(!is.data.frame(b)) {
    stop("rquery::theta_join_se.data.frame b must also be a data.frame")
  }
  nmgen <- mk_tmp_name_source("rquery_tmp")
  tmp_namea <- nmgen()
  dnodea <- table_source(tmp_namea, colnames(a))
  dnodea$data <- a
  tmp_nameb <- nmgen()
  dnodeb <- table_source(tmp_namea, colnames(b))
  dnodeb$data <- b
  enode <- theta_join_se(dnodea, dnodeb,
                         expr,
                         jointype = jointype,
                         suffix = suffix,
                         env = env)
  return(enode)
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
#' @param suffix character length 2, suffices to disambiguate columns.
#' @param env environment to look for values in.
#' @return theta_join node.
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d1 <- dbi_copy_to(my_db, 'd1',
#'                     data.frame(AUC = 0.6, R2 = 0.2))
#'   d2 <- dbi_copy_to(my_db, 'd2',
#'                     data.frame(AUC2 = 0.4, R2 = 0.3))
#'   eqn <- theta_join_nse(d1, d2, AUC >= AUC2)
#'   cat(format(eqn))
#'   sql <- to_sql(eqn, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
theta_join_nse <- function(a, b,
                           expr,
                           ...,
                           jointype = 'INNER',
                           suffix = c("_a", "_b"),
                           env = parent.frame()) {
  UseMethod("theta_join_nse", a)
}


#' @export
theta_join_nse.relop <- function(a, b,
                                 expr,
                                 ...,
                                 jointype = 'INNER',
                                 suffix = c("_a", "_b"),
                                 env = parent.frame()) {
  exprq <- substitute(expr)
  if(!("relop" %in% class(b))) {
    stop("rquery::theta_join_nse.relop b must also be of class relop")
  }
  usesa <- column_names(a)
  usesb <- column_names(b)
  have = unique(c(usesa, usesb))
  vnam <- setdiff(paste("rquery_thetajoin_condition",
                        1:(length(have)+1), sep = "_"),
                  have)[[1]]
  parsed <- parse_nse(a, list(exprq),
                      env = env,
                      have = have)
  parsed[[1]]$symbols_produced <- vnam
  assignments <- unpack_assignments(a, parsed,
                                    have = have)
  parsed[[1]]$symbols_produced <- character(0)
  parsed[[1]]$presentation <- gsub("^.*:= ", "", parsed[[1]]$presentation)
  r <- list(source = list(a, b),
            cmap = build_col_name_map(usesa, usesb, suffix),
            jointype = jointype,
            parsed = parsed,
            suffix = suffix)
  r <- relop_decorate("relop_theta_join", r)
  r
}

#' @export
theta_join_nse.data.frame <- function(a, b,
                                      expr,
                                      ...,
                                      jointype = 'INNER',
                                      suffix = c("_a", "_b"),
                                      env = parent.frame()) {
  exprq <- substitute(expr)
  if(!is.data.frame(b)) {
    stop("rquery::theta_join_nse.data.frame b must also be a data.frame")
  }
  nmgen <- mk_tmp_name_source("rquery_tmp")
  tmp_namea <- nmgen()
  dnodea <- table_source(tmp_namea, colnames(a))
  dnodea$data <- a
  tmp_nameb <- nmgen()
  dnodeb <- table_source(tmp_namea, colnames(b))
  dnodeb$data <- b
  enode <- theta_join_nse(dnodea, dnodeb,
                          deparse(exprq),
                          jointype = jointype,
                          suffix = suffix,
                          env = env)
  return(enode)
}





#' @export
column_names.relop_theta_join <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  c(as.character(x$cmap[['a']]), as.character(x$cmap[['b']]))
}


#' @export
format.relop_theta_join <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
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


prepColumnNames <- function(db, tabName, tabColumns, cmap) {
  tabColumnsV <- vapply(tabColumns,
                        function(ci) {
                          quote_identifier(db, ci)
                        }, character(1))
  tabColumnsV <- paste(quote_identifier(db, tabName), tabColumnsV, sep = ".")
  tabColumnsA <- vapply(tabColumns,
                        function(ci) {
                          quote_identifier(db, cmap[[ci]])
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
  merge_columns_used(s1, s2)
}

#' @export
to_sql.relop_theta_join <- function (x,
                                     db,
                                     ...,
                                     source_limit = NULL,
                                     indent_level = 0,
                                     tnum = mk_tmp_name_source('tsql'),
                                     append_cr = TRUE,
                                     using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  # re-quote expr
  re_quoted <- redo_parse_quoting(x$parsed, db)
  # work on query
  using <- calc_used_relop_theta_join(x,
                                      using=using)
  c1 <- intersect(using, column_names(x$source[[1]]))
  c2 <- intersect(using, column_names(x$source[[2]]))
  subsqla_list <- to_sql(x$source[[1]],
                         db = db,
                         source_limit = source_limit,
                         indent_level = indent_level + 1,
                         tnum = tnum,
                         append_cr = FALSE,
                         using = c1)
  subsqla <- subsqla_list[[length(subsqla_list)]]
  subsqlb_list <- to_sql(x$source[[2]],
                         db = db,
                         source_limit = source_limit,
                         indent_level = indent_level + 1,
                         tnum = tnum,
                         append_cr = FALSE,
                         using = c2)
  subsqlb <- subsqlb_list[[length(subsqlb_list)]]
  taba <- tnum()
  tabb <- tnum()
  bterms <- setdiff(column_names(x$source[[1]]),
                    column_names(x$source[[2]]))
  if(length(bterms)>0) {
    bcols <- vapply(bterms,
                    function(ci) {
                      quote_identifier(db, ci)
                    }, character(1))
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  cseta <- prepColumnNames(db, taba, column_names(x$source[[1]]),
                          x$cmap[['a']])
  ctermsa <- paste(cseta, collapse = paste0(",\n", prefix, " "))
  csetb <- prepColumnNames(db, tabb, column_names(x$source[[2]]),
                          x$cmap[['b']])
  ctermsb <- paste(csetb, collapse = paste0(",\n", prefix, " "))
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", ctermsa, ",\n",
              prefix, " ", ctermsb, "\n",
              prefix, "FROM (\n",
              subsqla, "\n",
              prefix, ") ",
              quote_identifier(db, taba), "\n",
              prefix, x$jointype,
              " JOIN (\n",
              subsqlb, "\n",
              prefix, ") ",
              quote_identifier(db, tabb),
              " ON ",
              x$parsed[[1]]$parsed)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsqla_list[-length(subsqla_list)],
    subsqlb_list[-length(subsqlb_list)],
    q)
}
