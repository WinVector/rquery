

#' Make a table description directly.
#'
#' Build minimal structures (table name and column names) needed to represent data from a remote table.
#'
#' Generate a query that returns contents of a table, we
#' could try to eliminate this (replace the query with the table name),
#' but there are features one can work with with the query in place and
#' SQL optimizers likely make this zero-cost anyway.
#'
#' @param table_name character, name of table
#' @param columns character, column names of table
#' @return a relop representation of the data
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   rq_copy_to(my_db,
#'              'd',
#'              data.frame(AUC = 0.6, R2 = 0.2),
#'              overwrite = TRUE,
#'              temporary = TRUE)
#'   d <- mk_td('d',
#'                     columns = c("AUC", "R2"))
#'   print(d)
#'   sql <- to_sql(d, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @seealso \code{\link{db_td}}, \code{\link{rq_copy_to}}, \code{\link{materialize}}, \code{\link{execute}}, \code{\link{to_sql}}
#'
#' @export
#'
mk_td <- function(table_name, columns) {
  r <- list(source = list(),
            table_name = table_name,
            parsed = NULL,
            columns = columns)
  r <- relop_decorate("relop_table_source", r)
  r
}

#' @describeIn mk_td old name for mk_td
#' @export
table_source <- mk_td




#' Make a table description from a database source.
#'
#' Build structures (table name, column names, and quoting
#' strategy) needed to represent data from a remote table.
#'
#' Note: in examples we use \code{rq_copy_to()} to create data.  This is only for the purpose of having
#' easy portable examples.  With big data the data is usually already in the remote database or
#' Spark system. The task is almost always to connect and work with this pre-existing remote data
#' and the method to do this is \code{db_td}
#' which builds a reference to a remote table given the table name.
#'
#'
#' @param db database connection
#' @param table_name name of table
#' @return a relop representation of the data
#'
#' @seealso \code{\link{mk_td}}, \code{\link{rq_copy_to}}, \code{\link{materialize}}, \code{\link{execute}}, \code{\link{to_sql}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   rq_copy_to(my_db,
#'               'd',
#'               data.frame(AUC = 0.6, R2 = 0.2),
#'               overwrite = TRUE,
#'               temporary = TRUE)
#'   d <- db_td(my_db, 'd')
#'   print(d)
#'   sql <- to_sql(d, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   cols <- columns_used(d)
#'   print(cols)
#'
#'   sql2 <- to_sql(d, my_db, using = "AUC")
#'   cat(sql2)
#'   print(DBI::dbGetQuery(my_db, sql2))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
db_td <- function(db, table_name) {
  mk_td(table_name = table_name,
        columns = rq_colnames(db, table_name))
}

#' @describeIn db_td old name for db_td
#' @export
dbi_table <- db_td



#' Make a table description of a local data.frame.
#'
#' @param d data.frame or name of data.frame to use as a data source.
#' @param ... not used, force later arguments to be optional.
#' @param name if not null name to user for table.
#' @param name_source temporary name source.
#' @param env environment to work in.
#' @return a relop representation of the data
#'
#' @examples
#'
#' d <- data.frame(x = 1)
#' local_td(d)
#' local_td("d")
#' local_td(as.name("d"))
#' local_td(data.frame(x = 1))
#' d %.>% local_td # needs wrapr 1.5.0 or newer to capture name
#'
#' @export
#'
local_td <- function(d,
                     ...,
                     name = NULL,
                     name_source = wrapr::mk_tmp_name_source("rqltd"),
                     env = parent.frame()) {
  UseMethod("local_td")
}

#' @export
local_td.data.frame <- function(d,
                                ...,
                                name = NULL,
                                name_source = wrapr::mk_tmp_name_source("rqltd"),
                                env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::local_td.data.frame")
  table_name <- name
  if(is.null(table_name)) {
    table_name <- substitute(d)
    if(is.name(table_name) || is.character(table_name)) {
      table_name <- as.character(table_name)
    } else {
      table_name = name_source()
    }
  }
  mk_td(table_name = table_name,
        columns = colnames(d))
}

#' @export
local_td.character <- function(d,
                               ...,
                               name_source = wrapr::mk_tmp_name_source("rqltd"),
                               env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::local_td.character")
  table_name <- d
  d <- get(table_name, envir = env)
  if(!is.data.frame(d)) {
    stop("rquery::local_td.character: argument d must be a string that resolves to a data.frame")
  }
  mk_td(table_name = table_name,
        columns = colnames(d))
}


#' @export
local_td.name <- function(d,
                          ...,
                          name_source = wrapr::mk_tmp_name_source("rqltd"),
                          env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::local_td.name")
  table_name <- as.character(d)
  d <- get(table_name, envir = env)
  if(!is.data.frame(d)) {
    stop("rquery::local_td.name: argument d must be a name that resolves to a data.frame")
  }
  mk_td(table_name = table_name,
        columns = colnames(d))
}






#' @export
tables_used.relop_table_source <- function(node, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::tables_used.relop_table_source")
  node$table_name
}

#' @export
column_names.relop_table_source <- function (x, ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::column_names.relop_table_source")
  x$columns
}


columns_used_relop_table_source <- function (x,
                                             ...,
                                             using = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::columns_used_relop_table_source")
  cols <- x$columns
  if(length(using)>0) {
    missing <- setdiff(using, x$columns)
    if(length(missing)>0) {
      stop(paste("rquery:columns_used request for unknown columns",
                 paste(missing, collapse = ", ")))
    }
    cols <- intersect(cols, using)
  }
  cols
}

#' @export
columns_used.relop_table_source <- function (x, ...,
                                             using = NULL) {
  cols <- columns_used_relop_table_source(x, using = using)
  r <- list(cols)
  names(r) <- x$table_name
  r
}

#' @export
to_sql.relop_table_source <- function (x,
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
  tabnam <- quote_identifier(db,  x$table_name)
  cols <- columns_used_relop_table_source(x, using = using)
  qcols <- vapply(cols,
                  function(ui) {
                    quote_identifier(db, ui)
                  }, character(1))
  qcols <- paste(quote_identifier(db, x$table_name), qcols, sep = '.')
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



#' @export
format_node.relop_table_source <- function(node) {
  max_cols <- 20
  cols <- node$columns
  if(length(cols)>max_cols) {
    cols <- c(cols[seq_len(max_cols)], "...")
  }
  paste0("table('", node$table_name, "'; \n  ",
         paste(cols, collapse = ",\n  "),
         ")\n")
}


