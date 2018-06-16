
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
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
sql_node <- function(source, exprs,
                     ...,
                     mods = NULL,
                     orig_columns = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sql_node")
  UseMethod("sql_node", source)
}

#' @export
sql_node.relop <- function(source, exprs,
                           ...,
                           mods = NULL,
                           orig_columns = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sql_node.relop")
  names_used <- Filter(is.name, unlist(exprs,
                                       recursive = TRUE,
                                       use.names = FALSE))
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
            orig_columns = orig_columns)
  r <- relop_decorate("relop_sql", r)
  r
}

#' @export
sql_node.data.frame <- function(source, exprs,
                                ...,
                                mods = NULL,
                                orig_columns = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "sql_node.data.frame")
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- mk_td(tmp_name, colnames(source))
  enode <- sql_node(dnode, exprs = exprs,
                    mods = mods,
                    orig_columns = orig_columns)
  return(enode)
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
  if(!is.null(node$mods)) {
    modsstr <- paste(";\n          ", node$mods)
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
prep_sql_toks <- function(db, ei) {
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
                  return(as.character(eij))
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
  wrapr::stop_if_dot_args(substitute(list(...)), "to_sql.relop_sql")
  colsA <- vapply(names(x$exprs),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  sqlexprs <- vapply(x$exprs,
                     function(ei) {
                       prep_sql_toks(db, ei)
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
  if(!is.null(x$mods)) {
    q <- paste(q, x$mods)
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


