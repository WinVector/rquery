
# order new_names to try and capture as much
# of old_names order as possible (old_names can have duplicates
# and overalp with new_names)
#
# ex: order(names(c("a", "b", "c"), c("d", "b", "a")))
# # c("a", "b", "d")
order_names <- function(old_names, new_names) {
  old_names <- c(old_names, new_names)
  idxs <- match(new_names, old_names)
  idxs <- rank(idxs)
  new_names[idxs]
}


#' Make a general SQL node.
#'
#' @param source source to work from.
#' @param exprs SQL expressions
#' @param ... force later arguments to bind by name
#' @param mods SQL modifiers (GROUP BY, ORDER BY, and so on)
#' @param orig_columns logical if TRUE select all original columns.
#' @param expand_braces logical if TRUE use {col} notation to ensure {col} is a column name.
#' @param translate_quotes logical if TRUE translate quotes to SQL choice (simple replacement, no escaping).
#' @param env environment to look to.
#' @return sql node.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   # example database connection
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(),
#'                           ":memory:")
#'   # load up example data
#'   d <- rq_copy_to(
#'     my_db, 'd',
#'     data.frame(v1 = c(1, 2, NA, 3),
#'                v2 = c(NA, "b", NA, "c"),
#'                v3 = c(NA, NA, 7, 8),
#'                stringsAsFactors = FALSE))
#'
#'   # look at table
#'   execute(my_db, d)
#'
#'   # get list of columns
#'   vars <- column_names(d)
#'   print(vars)
#'
#'   # build a NA/NULLs per-row counting expression.
#'   # names are "quoted" by wrapping them with as.name().
#'   # constants can be quoted by an additional list wrapping.
#'   expr <- lapply(vars,
#'                  function(vi) {
#'                    list("+ (CASE WHEN (",
#'                         as.name(vi),
#'                         "IS NULL ) THEN 1.0 ELSE 0.0 END)")
#'                  })
#'   expr <- unlist(expr, recursive = FALSE)
#'   expr <- c(list(0.0), expr)
#'   cat(paste(unlist(expr), collapse = " "))
#'
#'   # instantiate the operator node
#'   op_tree <- d %.>%
#'     sql_node(., "num_missing" %:=% list(expr))
#'   cat(format(op_tree))
#'
#'   # examine produced SQL
#'   sql <- to_sql(op_tree, my_db)
#'   cat(sql)
#'
#'   # execute
#'   execute(my_db, op_tree) %.>%
#'      print(.)
#'
#'   # whole process wrapped in convenience node
#'   op_tree2 <- d %.>%
#'     count_null_cols(., vars, "nnull")
#'   execute(my_db, op_tree2) %.>%
#'     print(.)
#'
#'   # sql_node also allows marking variable in quoted expressions
#'   ops <- d %.>%
#'      sql_node(., qae(sqrt_v1 = sqrt(.[v1])))
#'   execute(my_db, ops) %.>%
#'      print(.)
#'   # marking variables allows for error-checking of column names
#'   tryCatch({
#'     ops <- d %.>%
#'       sql_node(., qae(sqrt_v1 = sqrt(.[v1_misspellled])))
#'     },
#'     error = function(e) {print(e)})
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
sql_node <- function(source, exprs,
                     ...,
                     mods = NULL,
                     orig_columns = TRUE,
                     expand_braces = TRUE,
                     translate_quotes = TRUE,
                     env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "sql_node")
  UseMethod("sql_node", source)
}

promote_brace_to_var <- function(s, open_symbol = "{", close_symbol = "}") {
  brace_positions <- vapply(
    s,
    function(si) {
      is.character(si) && (nchar(si)>0) && (substr(si, 1, nchar(open_symbol))==open_symbol)
    }, logical(1))
  if(!isTRUE(any(brace_positions))) {
    return(s)
  }
  s <- as.list(s)
  s[brace_positions] <-
    lapply(s[brace_positions],
           function(sij) {
             sij <- gsub(open_symbol, "", sij, fixed = TRUE)
             sij <- gsub(close_symbol, "", sij, fixed = TRUE)
             sij <- trimws(sij, which = "both")
             as.name(sij)
           })
  s
}


#' @export
sql_node.relop <- function(source, exprs,
                           ...,
                           mods = NULL,
                           orig_columns = TRUE,
                           expand_braces = TRUE,
                           translate_quotes = TRUE,
                           env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "sql_node.relop")
  # translate {Q} into as.name("Q")
  exprs <- as.list(exprs)
  if(expand_braces) {
    exprs <- wrapr::split_at_brace_pairs(exprs, open_symbol = ".[", close_symbol = "]")
    exprs <- lapply(exprs, promote_brace_to_var, open_symbol = ".[", close_symbol = "]")
    mods <- wrapr::split_at_brace_pairs(mods, open_symbol = ".[", close_symbol = "]")
    mods <- promote_brace_to_var(mods, open_symbol = ".[", close_symbol = "]")
  }
  # look for names used
  names_used <- Filter(
    is.name,
    c(unlist(exprs,
             recursive = TRUE,
             use.names = FALSE),
      unlist(mods,
             recursive = TRUE,
             use.names = FALSE)))

  names_used <- sort(unique(as.character(names_used)))
  undef <- setdiff(names_used, column_names(source))
  if(length(undef)>0) {
    stop(paste("rquery::sql_node.relop undefined columns:",
               paste(undef, collapse = ", ")))
  }
  if(length(unique(names(exprs)))!=length(exprs)) {
    stop("rquery::sql_node bad assignment name")
  }
  srcnames <- column_names(source)
  newnames <- names(exprs)
  if(orig_columns) {
    newnames <- c(newnames, setdiff(srcnames, newnames))
  }
  cols <- order_names(srcnames, newnames)
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            exprs = exprs,
            mods = mods,
            cols = cols,
            orig_columns = orig_columns,
            expand_braces = expand_braces,
            translate_quotes = translate_quotes)
  r <- relop_decorate("relop_sql", r)
  r
}

#' @export
sql_node.data.frame <- function(source, exprs,
                                ...,
                                mods = NULL,
                                orig_columns = TRUE,
                                expand_braces = TRUE,
                                translate_quotes = TRUE,
                                env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "sql_node.data.frame")
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- sql_node(dnode, exprs = exprs,
                    mods = mods,
                    orig_columns = orig_columns,
                    expand_braces = expand_braces,
                    translate_quotes = translate_quotes,
                    env = env)
  rquery_apply_to_data_frame(source, enode, env = env)
}




#' @export
column_names.relop_sql <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), "column_names.relop_sql")
  x$cols
}



#' @export
format_node.relop_sql <- function(node) {
  if(!is.null(node$display_form)) {
    str <- paste0("sql_node(.,\n",
           "          ", node$display_form,
           ")\n")
    return(str)
  }
  exprtxt <- vapply(node$exprs,
                    function(ei) {
                      paste(as.character(ei), collapse = " ")
                    }, character(1))
  assignments <- paste(names(node$exprs), "%:=%", exprtxt)
  modsstr <- ""
  indent_sep <- "\n             "
  if(length(node$mods)>0) {
    modsql <- prep_sql_toks(rquery_default_db_info(), node$mods,
                            translate_quotes = FALSE, qsym = '"')
    modsstr <- paste(";\n          ", modsql)
  }
  paste0("sql_node(.,\n",
         "          ", paste(assignments, collapse = indent_sep),
         modsstr,
         ",", indent_sep, "*=", node$orig_columns,
         ")\n")
}



#' @export
columns_used.relop_sql <- function (x, ...,
                                    using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "columns_used.relop_sql")
  # assume using all columns
  return(columns_used(x$source[[1]],
                      using = NULL))
}


# assemble SQL from list of strings (treated as is),
# names (treated as SQL column names), and
# lists (first value used, if character treated as SQL constant).
prep_sql_toks <- function(db, ei, translate_quotes, qsym) {
  eiq <- vapply(ei,
                function(eij) {
                  if(is.list(eij)) {
                    eij <- eij[[1]]
                    if(is.character(eij) || is.factor(eij) ) {
                      return(quote_string(db, as.character(eij)))
                    }
                    return(quote_literal(db, eij))
                  }
                  if(is.name(eij)) {
                    return(quote_identifier(db, as.character(eij)))
                  }
                  v <- as.character(eij)
                  if(translate_quotes) {
                    v <- gsub('"', qsym, v, fixed = TRUE)
                  }
                  return(v)
                }, character(1))
  paste(eiq, collapse = " ")
}

#' @export
to_sql.relop_sql <- function (x,
                              db,
                              ...,
                              limit = NULL,
                              source_limit = NULL,
                              indent_level = 0,
                              tnum = mk_tmp_name_source('tsql'),
                              append_cr = TRUE,
                              using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::to_sql.relop_sql")
  dispatch_to_sql_method(
    method_name = "to_sql.relop_sql",
    x = x,
    db = db,
    limit = limit,
    source_limit = source_limit,
    indent_level = indent_level,
    tnum = tnum,
    append_cr = append_cr,
    using = using)
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

