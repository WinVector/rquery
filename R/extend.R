
try_merge_parsed_exprs <- function(p1, p2) {
  # see if we can remove any results in p1
  produced_1 <- unlist(lapply(p1, function(pi) { pi$symbols_produced }), recursive = TRUE, use.names = FALSE)
  produced_2 <- unlist(lapply(p2, function(pi) { pi$symbols_produced }), recursive = TRUE, use.names = FALSE)
  common_results <- intersect(produced_1, produced_2)
  if(length(common_results)>0) {
    # make our condition that neigher set of expressions use any produced columns
    p1_common <- p1[vapply(p1, function(pi) { pi$symbols_produced %in% common_results }, logical(1))]
    p2_common <- p2[vapply(p2, function(pi) { pi$symbols_produced %in% common_results }, logical(1))]
    used_common_1 <- unlist(lapply(p1_common, function(pi) { pi$symbols_used }), recursive = TRUE, use.names = FALSE)
    produced_common_1 <- unlist(lapply(p1_common, function(pi) { pi$symbols_produced }), recursive = TRUE, use.names = FALSE)
    used_common_2 <- unlist(lapply(p2_common, function(pi) { pi$symbols_used }), recursive = TRUE, use.names = FALSE)
    produced_common_2 <- unlist(lapply(p2_common, function(pi) { pi$symbols_produced }), recursive = TRUE, use.names = FALSE)
    if(length(intersect(used_common_1, produced_2)) > 0) {
      return(NULL)
    }
    if(length(intersect(used_common_1, produced_1)) > 0) {
      return(NULL)
    }
    if(length(intersect(produced_1, used_common_2)) > 0) {
      return(NULL)
    }
    if(length(intersect(produced_2, used_common_2)) > 0) {
      return(NULL)
    }
    # can remove p1 common portions
    p1 <- p1[vapply(p1, function(pi) { !(pi$symbols_produced %in% common_results) }, logical(1))]
  }
  # check disjointness conditions, that will allows us to merge
  used_1 <- unlist(lapply(p1, function(pi) { pi$symbols_used }), recursive = TRUE, use.names = FALSE)
  produced_1 <- unlist(lapply(p1, function(pi) { pi$symbols_produced }), recursive = TRUE, use.names = FALSE)
  used_2 <- unlist(lapply(p2, function(pi) { pi$symbols_used }), recursive = TRUE, use.names = FALSE)
  produced_2 <- unlist(lapply(p2, function(pi) { pi$symbols_produced }), recursive = TRUE, use.names = FALSE)
  if(length(intersect(produced_1, produced_2)) > 0) {
    return(NULL)
  }
  if(length(intersect(used_1, produced_2)) > 0) {
    return(NULL)
  }
  if(length(intersect(produced_1, used_2)) > 0) {
    return(NULL)
  }
  c(p1, p2)
}


#' Extend data by adding more columns.
#'
#' partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL, Spark, and so on).
#'
#' @param source source to select from.
#' @param parsed parsed assignment expressions.
#' @param ... force later arguments to bind by name
#' @param partitionby partitioning (window function) terms.
#' @param orderby ascending ordering (in window function) terms.
#' @param reverse which columns to reverse order to descending (in window function)
#' @param display_form chacter presentation form
#' @param windowed logial, if TRUE we are in a window function situation
#' @return extend node.
#'
#'
#' @noRd
#'
extend_impl <- function(source, parsed,
                        ...,
                        partitionby = NULL,
                        orderby = NULL,
                        reverse = NULL,
                        display_form = NULL,
                        windowed = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::extend_impl")
  if(length(parsed) <=0 ) {
    return(source)
  }
  if(length(partitionby)>0) {
    if(!windowed) {
      stop("rquery::extend_impl partionby non-trivial, but windowed==FALSE")
    }
  }
  if(length(orderby)>0) {
    if(!windowed) {
      stop("rquery::extend_impl orderby non-trivial, but windowed==FALSE")
    }
  }
  if(length(partitionby)!=length(unique(partitionby))) {
    stop("rquery:::extend_impl duplicate partitionby columns")
  }
  if(length(reverse)!=length(unique(reverse))) {
    stop("rquery:::extend_impl duplicate reverse columns")
  }
  if(length(orderby)!=length(unique(orderby))) {
    stop("rquery:::extend_impl duplicate orderby columns")
  }
  if(length(setdiff(reverse, orderby))>0) {
    stop("rquery::extend_imp all reverse columns must also be orderby columns")
  }
  src_columns <- column_names(source)
  required_cols <- sort(unique(c(
    merge_fld(parsed, "symbols_used"),
    merge_fld(parsed, "free_symbols"),
    partitionby,
    orderby
  )))
  check_have_cols(src_columns, required_cols, "rquery::extend")
  if(("relop_extend" %in% class(source)) && is.null(display_form)) {
    # see if we can merge nodes
    if( (windowed == source$windowed) &&
        isTRUE(all.equal(partitionby, source$partitionby)) &&
        isTRUE(all.equal(orderby, source$orderby)) &&
        isTRUE(all.equal(reverse, source$reverse)) ) {
      parsed_merged <- try_merge_parsed_exprs(source$parsed, parsed)
      if(length(parsed_merged) > 0) {
        return(extend_impl(source = source$source[[1]], parsed = parsed_merged,
                           partitionby = partitionby,
                           orderby = orderby,
                           reverse = reverse,
                           display_form = NULL,
                           windowed = windowed))
      }
    }
  }
  assignments <- unpack_assignments(source, parsed)
  r <- list(source = list(source),
            table_name = NULL,
            parsed = parsed,
            partitionby = partitionby,
            orderby = orderby,
            reverse = reverse,
            assignments = assignments,
            required_cols = required_cols,
            columns_produced = names(assignments),
            src_columns = src_columns,
            display_form = display_form,
            windowed = windowed)
  r <- relop_decorate("relop_extend", r)
  r
}

#' Extend data by adding more columns list mode (can create multiple nodes)
#'
#' partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL, Spark and so on).
#'
#' @param source source to select from.
#' @param parsed parsed assignment expressions.
#' @param ... force later arguments to bind by name
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (in window function) terms.
#' @param reverse reverse ordering (in window function) terms.
#' @param display_form chacter presentation form
#' @return extend node.
#'
#'
#' @noRd
#'
extend_impl_list <- function(source, parsed,
                             ...,
                             partitionby = NULL,
                             orderby = NULL,
                             reverse = NULL,
                             display_form = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::extend_impl_list")
  windowed = (length(orderby)>0) || (length(partitionby)>0)
  if(is.numeric(partitionby)) {
    partitionby = NULL
    windowed = TRUE
  }
  if(length(setdiff(reverse, orderby))>0) {
    stop("rquery::extend_impl_list all reverse columns must also be orderby columns")
  }
  produced <- vapply(parsed, function(pi) pi$symbols_produced, character(1))
  # if(length(produced)!=length(unique(produced))) {
  #   warning("rquery:::extend_impl_list assigned same column more than once in an extend")
  # }
  parts <- partition_assignments(parsed)
  ndchain <- source
  for(parti in parts) {
    parsedi <- parsed[parti$origOrder]
    ndchain <- extend_impl(ndchain, parsedi,
                           partitionby = partitionby,
                           orderby = orderby,
                           reverse = reverse,
                           display_form = display_form,
                           windowed = windowed)
  }
  ndchain
}



#' Extend data by adding more columns.
#'
#' Create a node similar to a Codd extend relational operator (add derived columns).
#'
#' Partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL, Spark and so on).
#'
#' Note: if any window/aggregation functions are present then at least one of partitionby or orderby
#' must be non empty.  For this purpose partitionby=1 is allowed and means "single partition on the constant 1."
#'
#' @param source source to select from.
#' @param assignments new column assignment expressions.
#' @param ... force later arguments to bind by name
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (in window function) terms.
#' @param reverse reverse ordering (in window function) terms.
#' @param display_form chacter presentation form
#' @param env environment to look for values in.
#' @return extend node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'   optree <- extend_se(d, c("v" %:=% "AUC + R2", "x" %:=% "pmax(AUC,v)"))
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
extend_se <- function(source, assignments,
                      ...,
                      partitionby = NULL,
                      orderby = NULL,
                      reverse = NULL,
                      display_form = NULL,
                      env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), 'extend_se')
  UseMethod("extend_se", source)
}

#' @export
extend_se.relop <- function(source, assignments,
                            ...,
                            partitionby = NULL,
                            orderby = NULL,
                            reverse = NULL,
                            display_form = NULL,
                            env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::extend_se.relop")
  if(length(setdiff(reverse, c(orderby, partitionby)))>0) {
    stop("rquery::extend_se.relop all reverse columns must also be orderby or partitionby columns")
  }
  parsed <- parse_se(source, assignments, env = env, allow_empty = TRUE)
  if(length(parsed)<=0) {
    return(source)
  }
  extend_impl_list(source = source,
                   parsed = parsed,
                   partitionby = partitionby,
                   orderby = orderby,
                   reverse = reverse,
                   display_form = display_form)
}

#' @export
extend_se.data.frame <- function(source, assignments,
                                 ...,
                                 partitionby = NULL,
                                 orderby = NULL,
                                 reverse = NULL,
                                 display_form = NULL,
                                 env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::extend_se.data.frame")
  if(length(setdiff(reverse, c(orderby, partitionby)))>0) {
    stop("rquery::extend_se.data.frame all reverse columns must also be orderby or partitionby columns")
  }
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- extend_se(dnode,
                     assignments = assignments,
                     partitionby = partitionby,
                     orderby = orderby,
                     reverse = reverse,
                     display_form = display_form,
                     env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}



#' Extend data by adding more columns.
#'
#' Create a node similar to a Codd extend relational operator (add derived columns).
#'
#' Partitionby and orderby can only be used with a database that supports window-functions
#' (such as PostgreSQL, Spark, and so on).
#'
#' Supports \code{bquote()} \code{.()}-style name abstraction with the extenson that \code{-} promotes strings to names
#' (please see here: \url{https://github.com/WinVector/rquery/blob/master/Examples/Substitution/Substitution.md}).
#'
#' Note: if any window/aggregation functions are present then at least one of partitionby or orderby
#' must be non empty.  For this purpose partitionby=1 is allowed and means "single partition on the constant 1."
#'
#' @param source source to select from.
#' @param ... new column assignment expressions.
#' @param partitionby partitioning (window function) terms.
#' @param orderby ordering (in window function) terms.
#' @param reverse reverse ordering (in window function) terms.
#' @param display_form chacter presentation form
#' @param env environment to look for values in.
#' @return extend node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'   NEWCOL <- as.name("v")
#'   NEWVALUE = "zz"
#'   optree <- extend(d, .(NEWCOL) %:=% ifelse(AUC>0.5, R2, 1.0), .(NEWVALUE) %:=% 6)
#'   cat(format(optree))
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
extend <- function(source,
                   ...,
                   partitionby = NULL,
                   orderby = NULL,
                   reverse = NULL,
                   display_form = NULL,
                   env = parent.frame()) {
  force(env)
  UseMethod("extend", source)
}

#' @rdname extend
#' @export
#'
extend_nse <- extend

#' @export
#'
extend.relop <- function(source,
                             ...,
                             partitionby = NULL,
                             orderby = NULL,
                             reverse = NULL,
                             display_form = NULL,
                             env = parent.frame()) {
  force(env)
  # Recommend way to capture ... unevalauted from
  # http://adv-r.had.co.nz/Computing-on-the-language.html#substitute "Capturing unevaluated ..."
  exprs <-  eval(substitute(alist(...)))
  exprs <- lapply_bquote_to_langauge_list(exprs, env)
  if(length(setdiff(reverse, c(orderby, partitionby)))>0) {
    stop("rquery::extend.relop all reverse columns must also be orderby or partitionby columns")
  }
  parsed <- parse_nse(source, exprs, env = env, allow_empty = TRUE)
  if(length(parsed)<=0) {
    return(source)
  }
  extend_impl_list(source = source,
                   parsed = parsed,
                   partitionby = partitionby,
                   orderby = orderby,
                   reverse = reverse,
                   display_form = display_form)
}

#' @export
#'
extend.data.frame <- function(source,
                                  ...,
                                  partitionby = NULL,
                                  orderby = NULL,
                                  reverse = NULL,
                                  display_form = NULL,
                                  env = parent.frame()) {
  force(env)
  if(length(setdiff(reverse, c(partitionby, orderby)))>0) {
    stop("rquery::extend.data.frame all reverse columns must also be orderby or partitionby columns")
  }
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- extend(dnode,
                      ...,
                      partitionby = partitionby,
                      orderby = orderby,
                      reverse = reverse,
                      display_form = display_form,
                      env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}


#' @export
column_names.relop_extend <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::column_names.relop_extend")
  # cols <- sort(unique(c(column_names(x$source[[1]]), x$columns_produced)))
  cols2 <- c(x$src_columns, setdiff(x$columns_produced, x$src_columns))
  cols2
}


#' @export
format_node.relop_extend <- function(node) {
  if(!is.null(node$display_form)) {
    return(node$display_form)
  }
  origTerms <- vapply(node$parsed,
                      function(pi) {
                        paste(as.character(pi$presentation), collapse = ' ')
                      }, character(1))
  aterms <- paste(origTerms, collapse = ",\n  ")
  pterms <- ""
  oterms <- ""
  rterms <- ""
  if(node$windowed) {
    if(length(node$partitionby)>0) {
      pterms <- paste0(",\n  partitionby = ", wrapr::map_to_char(node$partitionby))
    } else {
      pterms <- paste0(",\n  partitionby = 1")
    }
  }
  if(length(node$partitionby)>0) {
    oterms <- paste0(",\n  orderby = ", wrapr::map_to_char(node$orderby))
  }
  if(length(node$partitionby)>0) {
    rterms <- paste0(",\n  reverse = ", wrapr::map_to_char(node$reverse))
  }
  paste0("extend(.,\n  ",
         aterms,
         pterms,
         oterms,
         rterms,
         ")",
         "\n")
}



calc_used_relop_extend <- function(x,
                                   using = NULL) {
  source_cols <- column_names(x$source[[1]])
  if(length(using)<=0) {
    return(source_cols)
  }
  want_expr <- vapply(x$parsed,
                      function(pi) {
                        length(intersect(pi$symbols_produced, using))>0
                      }, logical(1))
  expressions <- x$parsed[want_expr]
  producing <- merge_fld(expressions, "symbols_produced")
  consuming <- merge_fld(expressions, "symbols_used")
  direct_from_source <- setdiff(intersect(using, source_cols), producing)
  subusing <- unique(c(direct_from_source, consuming, x$partitionby, x$orderby))
  subusing
}

#' @export
columns_used.relop_extend <- function (x, ...,
                                       using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::columns_used.relop_extend")
  cols <- calc_used_relop_extend(x,
                                 using = using)
  columns_used(x$source[[1]],
               using = cols)
}


#' @export
to_sql.relop_extend <- function (x,
                                 db,
                                 ...,
                                 limit = NULL,
                                 source_limit = NULL,
                                 indent_level = 0,
                                 tnum = mk_tmp_name_source('tsql'),
                                 append_cr = TRUE,
                                 using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::to_sql.relop_extend")
  dispatch_to_sql_method(
    method_name = "to_sql.relop_extend",
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
}

to_sql_relop_extend <- function(
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
                          "rquery::to_sql.relop_extend")
  # re-quote expr
  parsed <- x$parsed
  if(length(using)>0) {
    want_expr <- vapply(x$parsed,
                        function(pi) {
                          length(intersect(pi$symbols_produced, using))>0
                        }, logical(1))
    parsed <- x$parsed[want_expr]
  }
  re_quoted <- redo_parse_quoting(parsed, db)
  re_assignments <- unpack_assignments(x$source[[1]], re_quoted)
  # work on query
  using_incoming <- calc_used_relop_extend(x,
                                           using = using)
  qlimit = limit
  if(!getDBOption(db, "use_pass_limit", TRUE)) {
    qlimit = NULL
  }
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        limit = qlimit,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = using_incoming)
  subsql <- subsql_list[[length(subsql_list)]]
  cols1 <- column_names(x$source[[1]])
  if(length(using)>0) {
    cols1 <- intersect(cols1, using)
  }
  cols1 <- setdiff(cols1, names(re_assignments)) # allow simple name re-use
  cols <- NULL
  if(length(cols1)>0) {
    cols <- vapply(cols1,
                   function(ci) {
                     quote_identifier(db, ci)
                   }, character(1))
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  derived <- NULL
  if(length(re_assignments)>0) {
    windowTerm <- ""
    if(x$windowed) {
      windowTerm <- "OVER ( "
      if(length(x$partitionby)>0) {
        pcols <- vapply(x$partitionby,
                        function(ci) {
                          quote_identifier(db, ci)
                        }, character(1))
        windowTerm <- paste0(windowTerm,
                             " PARTITION BY ",
                             paste(pcols, collapse = ", "))
      }
      ocols <- NULL
      if(length(x$orderby)>0) {
        ocols <- vapply(x$orderby,
                        function(ci) {
                          quote_identifier(db, ci)
                        }, character(1))
      }
      if(length(x$reverse)>0) {
        ocols[x$orderby %in% x$reverse] <- paste(ocols[x$orderby %in% x$reverse],
                                                 "DESC")
      }
      if(length(ocols)>0) {
        windowTerm <- paste0(windowTerm,
                             " ORDER BY ",
                             paste(ocols, collapse = ", "))
      }
      windowTerm <- paste(windowTerm, ")")
    }
    derived <- vapply(names(re_assignments),
                      function(ni) {
                        ei <- re_assignments[[ni]]
                        paste(ei,
                              windowTerm,
                              "AS", quote_identifier(db, ni))
                      }, character(1))
  }
  tab <- tnum()
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", paste(c(cols, derived), collapse = paste0(",\n", prefix, " ")))
  q <- paste0(q, "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, " ) ", tab)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}
