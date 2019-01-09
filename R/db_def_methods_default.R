
# default to_sql implementations
# dispatching per handle instance (instead of per handle class)

to_sql_relop_drop_columns <- function(
  x,
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
  using <- calc_using_relop_drop_columns(x,
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
                        using = using)
  subsql <- subsql_list[[length(subsql_list)]]
  cols <- vapply(x$columns,
                 function(ci) {
                   quote_identifier(db, ci)
                 }, character(1))
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", paste(cols, collapse = paste0(",\n", prefix, " ")), "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
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
    if((length(x$partitionby)>0) || (length(x$orderby)>0)) {
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

to_sql_relop_natural_join <- function(
  x,
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
  using <- unique(c(calc_used_relop_natural_join(x,
                                                 using=using),
                    x$by))
  cs1 <- column_names(x$source[[1]])
  cs2 <- column_names(x$source[[2]])
  if(length(setdiff(using, c(cs1, cs2)))>0) {
    stop(paste("to_sql.relop_natural_join input table(s) missing columns ",
               paste(setdiff(using, c(cs1, cs2)), collapse = ", ")))
  }
  c1 <- intersect(using, cs1)
  subsqla_list <- to_sql(x$source[[1]],
                         db = db,
                         source_limit = source_limit,
                         indent_level = indent_level + 1,
                         tnum = tnum,
                         append_cr = FALSE,
                         using = c1)
  subsqla <- subsqla_list[[length(subsqla_list)]]
  c2 <- intersect(using, cs2)
  subsqlb_list <- to_sql(x$source[[2]],
                         db = db,
                         source_limit = source_limit,
                         indent_level = indent_level + 1,
                         tnum = tnum,
                         append_cr = FALSE,
                         using = c2)
  subsqlb <- subsqlb_list[[length(subsqlb_list)]]
  taba <- tnum()
  tabaq <- quote_identifier(db, taba)
  tabb <- tnum()
  tabbq <- quote_identifier(db, tabb)
  bexpr <- NULL
  aterms <- setdiff(c1, x$by)
  bterms <- setdiff(c2, x$by)
  overlap <- c(x$by, intersect(aterms, bterms))
  prefix <- paste(rep(' ', indent_level), collapse = '')
  osql <- vapply(overlap,
                 function(ci) {
                   ciq <- quote_identifier(db, ci)
                   paste0("COALESCE(",
                          tabaq, ".", ciq,
                          ", ",
                          tabbq, ".", ciq,
                          ") AS ", ciq)
                 }, character(1))
  asql <- vapply(setdiff(aterms, overlap),
                 function(ci) {
                   ciq <- quote_identifier(db, ci)
                   paste0(tabaq, ".", ciq,
                          " AS ", ciq)
                 }, character(1))
  bsql <- vapply(setdiff(bterms, overlap),
                 function(ci) {
                   ciq <- quote_identifier(db, ci)
                   paste0(tabbq, ".", ciq,
                          " AS ", ciq)
                 }, character(1))
  texpr <- paste(c(osql, asql, bsql), collapse = paste0(",\n ", prefix))
  q <- paste0(prefix, "SELECT\n",
              " ", prefix, texpr, "\n",
              prefix, "FROM (\n",
              subsqla, "\n",
              prefix, ") ",
              tabaq, "\n",
              prefix, x$jointype,
              " JOIN (\n",
              subsqlb, "\n",
              prefix, ") ",
              tabbq)
  if(length(x$by)>0) {
    bt <- vapply(x$by,
                 function(ci) {
                   quote_identifier(db, ci)
                 }, character(1))
    mt <- paste(paste(paste(tabaq, bt, sep='.'),
                      paste(tabbq, bt, sep='.'), sep = ' = '),
                collapse = ' AND ')
    q <- paste0(q, "\n",
                prefix, "ON\n",
                prefix, " ", mt)
  }
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsqla_list[-length(subsqla_list)],
    subsqlb_list[-length(subsqlb_list)],
    q)
}

to_sql_relop_non_sql <- function(
  x,
  db,
  ...,
  limit = NULL,
  source_limit = NULL,
  indent_level = 0,
  tnum = mk_tmp_name_source('tsql'),
  append_cr = TRUE,
  using = NULL) {
  subsql <- to_sql(x$source[[1]],
                   db = db,
                   source_limit = source_limit,
                   indent_level = indent_level + 1,
                   tnum = tnum,
                   append_cr = append_cr,
                   using = NULL)
  nsubsql <- length(subsql)
  # non-SQL nodes must always be surrounded by SQL on both sides
  step1 <- materialize_sql_statement(db,
                                     subsql[[nsubsql]],
                                     x$incoming_table_name,
                                     temporary = x$temporary)
  nsql_step <- list(display_form = x$display_form,
                    incoming_table_name = x$incoming_table_name,
                    outgoing_table_name = x$outgoing_table_name,
                    temporary = x$temporary,
                    node = x,
                    f = x$f_db)
  class(nsql_step) <- "rquery_non_sql_step"
  step2 <- list(nsql_step)
  qlimit = limit
  if(!getDBOption(db, "use_pass_limit", TRUE)) {
    qlimit = NULL
  }
  step3 <- list(to_sql(mk_td(x$outgoing_table_name, column_names(x)),
                       db = db,
                       limit = qlimit,
                       source_limit = source_limit,
                       indent_level = indent_level + 1,
                       tnum = tnum,
                       append_cr = append_cr,
                       using = NULL))
  c(subsql[-length(subsql)], step1, step2, step3)
}

to_sql_relop_null_replace <- function(
  x,
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
                     quote_identifier(db, ci)
                   }, character(1))
  tqnames <- paste0(quote_identifier(db, tab),
                    ".",
                    qnames)
  qexpr <- tqnames
  alter <- which(cols %in% x$cols)
  if(length(alter)>0) {
    qexpr[alter] <- paste0("CASE WHEN ",
                           tqnames[alter],
                           " IS NULL THEN ",
                           quote_literal(db, x$value),
                           " ELSE ",
                           tqnames[alter],
                           " END")
  }
  qexpr <- paste(qexpr, "AS", qnames)
  texpr <- paste(qnames = qexpr)
  if(length(x$note_col)==1) {
    sumexprs <- c("0",
                  paste0("( CASE WHEN ",
                         tqnames[alter],
                         " IS NULL THEN 1 ELSE 0 END )"))
    sexpr <- paste0(
      paste(sumexprs, collapse = paste0(" + \n ", prefix)),
      " AS ", quote_identifier(db, x$note_col),
      "\n")
    texpr <- c(texpr, sexpr)
  }
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

to_sql_relop_order_expr <- function(
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
                          "rquery::to_sql.relop_order_expr")
  # re-quote expr
  re_quoted <- redo_parse_quoting(x$parsed, db)
  re_expr <- unpack_assignments(x$source[[1]], re_quoted,
                                check_is_assignment = FALSE)
  # work on query
  cols <- calc_used_relop_order_expr(x,
                                     using = using)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = cols)
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT * FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab, "\n",
              prefix, "ORDER BY ",
              re_expr)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}

to_sql_relop_project <- function(
  x,
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
  if(!("rquery_db_info" %in% class(db))) {
    connection <- db
    db <- rquery_default_db_info
    db$connection <- connection
  }
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
  using_incoming <- calc_used_relop_project(x,
                                            using = using)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = using_incoming)
  subsql <- subsql_list[[length(subsql_list)]]
  grouping_cols <- x$groupby
  if(length(x$groupby)>0) {
    grouping_cols <- vapply(x$groupby,
                            function(ci) {
                              quote_identifier(db, ci)
                            }, character(1))
  }
  extra_cols <- x$groupby
  if(length(using)>0) {
    extra_cols <- intersect(x$groupby, using)
  }
  if(length(extra_cols)>0) {
    extra_cols <- vapply(extra_cols,
                         function(ci) {
                           quote_identifier(db, ci)
                         }, character(1))
  }
  derived <- NULL
  if(length(re_assignments)>0) {
    derived <- vapply(names(re_assignments),
                      function(ni) {
                        ei <- re_assignments[[ni]]
                        paste(ei, "AS", quote_identifier(db, ni))
                      }, character(1))
  }
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT ",
              paste(c(extra_cols, derived), collapse = ", "),
              " FROM (\n",
              subsql, "\n",
              prefix, " ) ", tab)
  if(length(grouping_cols)>0) {
    q <- paste0(q,
                "\n",
                prefix, "GROUP BY\n",
                prefix, " ", paste(grouping_cols, collapse = ", "))
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

to_sql_relop_rename_columns <- function(
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
                          "rquery::to_sql.relop_rename_columns")
  qmap <- calc_used_relop_rename_columns(x, using=using)
  colsV <- vapply(as.character(qmap),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  colsA <- vapply(names(qmap),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  cols <- paste(colsV, "AS", colsA)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        limit = limit,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = as.character(qmap))
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", paste(cols, collapse = paste0(",\n", prefix, " ")), "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}

to_sql_relop_select_columns <- function(
  x,
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
  using <- calc_using_relop_select_columns(x,
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
                        using = using)
  subsql <- subsql_list[[length(subsql_list)]]
  cols <- vapply(x$columns,
                 function(ci) {
                   quote_identifier(db, ci)
                 }, character(1))
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", paste(cols, collapse = paste0(",\n", prefix, " ")), "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}

to_sql_relop_select_rows <- function(
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
                          "rquery::to_sql.relop_select_rows")
  # re-quote expr
  re_quoted <- redo_parse_quoting(x$parsed, db)
  re_expr <- unpack_assignments(x$source[[1]], re_quoted,
                                check_is_assignment = FALSE)
  # work on query
  cols <- calc_used_relop_select_rows(x,
                                      using = using)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = cols)
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT * FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab, "\n",
              prefix, "WHERE ",
              re_expr)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}

to_sql_relop_set_indicator <- function(
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
                          "rquery::to_sql.relop_set_indicator")
  cols1 <- column_names(x$source[[1]])
  qexample = quote_string(db, "a")
  qlen = as.numeric(regexec("a", qexample, fixed = TRUE)) - 1
  qsym = substr(qexample, 1, qlen)
  sqlexprs <- vapply(x$terms,
                     function(ei) {
                       prep_sql_toks(db, ei,
                                     translate_quotes = x$translate_quotes,
                                     qsym = qsym)
                     }, character(1))
  if(length(sqlexprs)!=1) {
    stop("rquery::to_sql.relop_set_indicator expected indicator calculation to be length 1")
  }
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        limit = limit,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = cols1)  # TODO: double check using calculation
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT *, ",
              sqlexprs[[1]], " AS ", quote_identifier(db, names(sqlexprs)),
              " FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}

to_sql_relop_sql <- function(
  x,
  db,
  ...,
  limit = NULL,
  source_limit = NULL,
  indent_level = 0,
  tnum = mk_tmp_name_source('tsql'),
  append_cr = TRUE,
  using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "to_sql.relop_sql")
  colsA <- vapply(names(x$exprs),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  qexample = quote_string(db, "a")
  qlen = as.numeric(regexec("a", qexample, fixed = TRUE)) - 1
  qsym = substr(qexample, 1, qlen)
  sqlexprs <- vapply(x$exprs,
                     function(ei) {
                       prep_sql_toks(db, ei,
                                     translate_quotes = x$translate_quotes,
                                     qsym = qsym)
                     }, character(1))
  cols <- paste(sqlexprs, "AS", colsA)
  names(cols) <- names(x$exprs)
  if(x$orig_columns) {
    extras <- setdiff(column_names(x$source[[1]]),
                      names(x$exprs))
    if(length(extras)>0) {
      qcols <- vapply(extras,
                      function(ci) {
                        quote_identifier(db, ci)
                      }, character(1))
      qcols <- paste(qcols, "AS", qcols)
      names(qcols) <- extras
      cols <- c(qcols, cols)
    }
  }
  cols <- cols[x$cols]
  names(cols) <- NULL
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
                        using = NULL)
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  star_str <- ""
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", paste(cols, collapse = paste0(",\n", prefix, " ")), "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(length(x$mods)>0) {
    modsql <- prep_sql_toks(db, x$mods,
                            translate_quotes = x$translate_quotes,
                            qsym = qsym)
    q <- paste(q, modsql)
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

to_sql_relop_table_source <- function(
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
                          "rquery::to_sql.relop_table_source")
  prefix <- paste(rep(' ', indent_level), collapse = '')
  tabnam <- quote_table_name(db,  x$table_name, qualifiers = x$qualifiers)
  cols <- columns_used_relop_table_source(x, using = using)
  qcols <- vapply(cols,
                  function(ui) {
                    quote_identifier(db, ui)
                  }, character(1))
  qt <- paste(qcols, collapse = paste0(",\n", prefix, " "))
  q <- paste0(prefix,
              "SELECT\n",
              prefix, " ", qt, "\n",
              prefix, "FROM\n",
              prefix, " ", tabnam)
  if((!is.null(limit))||(!is.null(source_limit))) {
    limit <- min(limit, source_limit)
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  q
}

to_sql_relop_theta_join <- function(
  x,
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
  bterms <- setdiff(c1,
                    c2)
  if(length(bterms)>0) {
    bcols <- vapply(bterms,
                    function(ci) {
                      quote_identifier(db, ci)
                    }, character(1))
  }
  prefix <- paste(rep(' ', indent_level), collapse = '')
  cseta <- prepColumnNames(db, taba, c1,
                           x$cmap[['a']])
  ctermsa <- paste(cseta, collapse = paste0(",\n", prefix, " "))
  csetb <- prepColumnNames(db, tabb, c2,
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
  if(!is.null(limit)) {
    q <- paste(q, "LIMIT",
               format(ceiling(limit), scientific = FALSE))
  }
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsqla_list[-length(subsqla_list)],
    subsqlb_list[-length(subsqlb_list)],
    q)
}

to_sql_relop_unionall <- function(
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
                          "rquery::to_sql.relop_unionall")
  qlimit = limit
  if(!getDBOption(db, "use_pass_limit", TRUE)) {
    qlimit = NULL
  }
  subsql_list <- lapply(
    x$source,
    function(si) {
      to_sql(si,
             db = db,
             limit = qlimit,
             source_limit = source_limit,
             indent_level = indent_level + 1,
             tnum = tnum,
             append_cr = FALSE,
             using = using)
    })
  sql_list <- NULL
  inputs <- character(0)
  for(sil in subsql_list) {
    sql_list <- c(sql_list, sil[-length(sil)])
    inputs <- c(inputs, sil[length(sil)])
  }
  tmps <- vapply(seq_len(length(inputs)),
                 function(i) {
                   tnum()
                 }, character(1))
  # allows us to ensure column order
  cols <- x$cols
  if(length(using)>0) {
    cols <- intersect(cols, using)
  }
  cols <- vapply(cols,
                 function(ci) {
                   quote_identifier(db, ci)
                 }, character(1))
  cols <- paste(cols, collapse = ", ")
  inputs <- paste("SELECT ", cols, " FROM ( ", inputs, ")", tmps)
  q <- paste(inputs, collapse = " UNION ALL ")
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
  c(sql_list, q)
}

#' Default to_sql method implementations.
#'
#' @return default implementation methods
#'
#' @keywords internal
#'
#' @export
#'
rquery_default_methods <- function() {
  list(
    "to_sql.relop_drop_columns" = to_sql_relop_drop_columns,
    "to_sql.relop_extend" = to_sql_relop_extend,
    "to_sql.relop_natural_join" = to_sql_relop_natural_join,
    "to_sql.relop_non_sql" = to_sql_relop_non_sql,
    "to_sql.relop_null_replace" = to_sql_relop_null_replace,
    "to_sql.relop_orderby" = to_sql_relop_orderby,
    "to_sql.relop_order_expr" = to_sql_relop_order_expr,
    "to_sql.relop_project" = to_sql_relop_project,
    "to_sql.relop_rename_columns" = to_sql_relop_rename_columns,
    "to_sql.relop_select_columns" = to_sql_relop_select_columns,
    "to_sql.relop_select_rows" = to_sql_relop_select_rows,
    "to_sql.relop_set_indicator" = to_sql_relop_set_indicator,
    "to_sql.relop_sql" = to_sql_relop_sql,
    "to_sql.relop_table_source" = to_sql_relop_table_source,
    "to_sql.relop_theta_join" = to_sql_relop_theta_join,
    "to_sql.relop_unionall" = to_sql_relop_unionall
  )
}




