

# work around common not fully DBI databases issues


#' Check if a table exists.
#'
#' @param db DBI connection
#' @param table_name character table name
#' @return logical TRUE if table exists.
#'
#' @seealso \code{\link{dbi_table}}
#'
#' @export
#'
dbi_table_exists <- function(db, table_name) {
  # Would like to just return DBI::dbExistsTable(db, table_name)
  if(getDBOption(db, "use_DBI_dbExistsTable", TRUE)) {
    return(DBI::dbExistsTable(db, table_name))
  }
  q <- paste0("SELECT * FROM ",
              DBI::dbQuoteIdentifier(db, table_name),
              " LIMIT 1")
  tryCatch(
    {
      v <- DBI::dbGetQuery(db, q)
      if(is.null(v)) {
        return(FALSE)
      }
      if(nrow(v)<1) {
        return(FALSE)
      }
      return(TRUE)
    },
    error = function(e) { return(FALSE) },
    warning = function(e) { return(FALSE) })
  return(FALSE)
}

#' List table column names.
#'
#' @param db DBI connection
#' @param table_name character table name
#' @return character list of column names
#'
#' @export
#'
dbi_colnames <- function(db, table_name) {
  # DBI::dbListFields fails intermitnently, and sometimes gives wrong results
  # filed as: https://github.com/tidyverse/dplyr/issues/3204
  if(getDBOption(db, "use_DBI_dbListFields", FALSE)) {
    return(DBI::dbListFields(db, table_name))
  }
  # below is going to have issues to to R-column name conversion!
  q <- paste0("SELECT * FROM ",
              DBI::dbQuoteIdentifier(db, table_name),
              " LIMIT 1")
  v <- DBI::dbGetQuery(db, q)
  colnames(v)
}


#' Remove table
#'
#' @param db database connection.
#' @param table_name character, name of table to create.
#' @return logical TRUE if table existed, else FALSE
#'
#' @seealso \code{\link{dbi_table}}
#'
#' @export
#'
dbi_remove_table <- function(db, table_name) {
  if(!is.null(table_name)) {
    if(dbi_table_exists(db, table_name)) {
      if(getDBOption(db, "use_DBI_dbRemoveTable", FALSE)) {
        DBI::dbRemoveTable(db, table_name)
      } else {
        dbi_execute(db,
                    paste("DROP TABLE",
                           quote_identifier(db, table_name)))
      }
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Execute a query.
#'
#' @param db DBI database connection
#' @param q character query
#' @return nothing
#'
#' @seealso \code{\link{dbi_table}}
#'
#' @export
#'
dbi_execute <- function(db, q) {
  res <- NULL
  if(getDBOption(db, "use_DBI_dbExecute", TRUE)) {
    res <- DBI::dbExecute(db, q)
  } else {
    DBI::dbGetQuery(db, q)
  }
  res
}

# try not to use this too many places, prefer the configs
connection_is_spark <- function(db) {
  length(intersect(c("spark_connection", "spark_shell_connection"),
                             class(db)))>=1
}

#' Local table to DBI data source.
#'
#' @param db database connection.
#' @param table_name name of table to create.
#' @param d data.frame to copy to database.
#' @param ... force later argument to be by name
#' @param overwrite passed to \code{\link[DBI]{dbWriteTable}}.
#' @param temporary passed to \code{\link[DBI]{dbWriteTable}}.
#' @param rowidcolumn character, name to land row-ids.
#' @return a relop representation of the data
#'
#' @seealso \code{\link{dbi_table}}, \code{\link{table_source}}, \code{\link{materialize}}, \code{\link{execute}}, \code{\link{to_sql}}
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- dbi_copy_to(db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'
#'   sql <- to_sql(d, db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(db, "SELECT * FROM d"))
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#'
dbi_copy_to <- function(db, table_name, d,
                        ...,
                        overwrite = FALSE,
                        temporary = TRUE,
                        rowidcolumn = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::dbi_copy_to")
  if(!is.null(rowidcolumn)) {
    d[[rowidcolumn]] <- seq_len(nrow(d))
  }
  # sparklyr 0.7.0 does not take overwrite or row.names arguments
  if(overwrite) {
    dbi_remove_table(db, table_name)
  }
  can_set_temp <- getDBOption(db, "control_temporary", NULL)
  can_set_rownames <- getDBOption(db, "control_rownames", NULL)
  if(connection_is_spark(db)) {
    if(is.null(can_set_temp)) {
      can_set_temp <- FALSE
    }
    if(is.null(can_set_rownames)) {
      can_set_rownames <- FALSE
    }
  }
  if(is.null(can_set_temp)) {
    can_set_temp <- TRUE
  }
  if(is.null(can_set_rownames)) {
    can_set_rownames <- TRUE
  }
  if(can_set_temp) {
     if(can_set_rownames) {
      DBI::dbWriteTable(db,
                        table_name,
                        d,
                        temporary = temporary,
                        row.names = FALSE)
    } else {
      DBI::dbWriteTable(db,
                        table_name,
                        d,
                        temporary = temporary)
    }
  } else {
    if(temporary && getOption("rquery.verbose")) {
      warning("setting rquery::dbi_copy_to setting temporary=FALSE")
    }
    if(can_set_rownames) {
      DBI::dbWriteTable(db,
                        table_name,
                        d,
                        row.names = FALSE)
    } else {
      DBI::dbWriteTable(db,
                        table_name,
                        d)
    }
  }
  dbi_table(db, table_name)
}

#' Count rows and return as numeric
#'
#' @param db database connetion
#' @param table_name character, name of table
#' @return numeric row count
#'
#' @seealso \code{\link{dbi_table}}
#'
#' @export
#'
dbi_nrow <- function(db, table_name) {
  nrowst <- DBI::dbGetQuery(
    db,
    paste0("SELECT COUNT(1) FROM ",
           DBI::dbQuoteIdentifier(db,
                                  table_name)))
  # integer64 was coming back from RPostgres
  # and that does not work as numeric in pmin()
  nrows <- as.numeric(nrowst[[1]][[1]])
  nrows
}


#' Build a cannonical name for a db connection class.
#'
#' @param db DBI database connection
#' @return character
#'
#' @examples
#'
#' if(requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   print(dbi_connection_name(my_db))
#'   print(dbi_connection_preferences(my_db))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
dbi_connection_name <- function(db) {
  cls <- sort(class(db))
  cls <- paste(cls, collapse = "_")
  cls <- gsub("[^a-zA-Z]+", "_", cls)
  cls
}



#' Get reasonable stored options for a DB connection.
#'
#' These settings are from what was known in March 2018 about
#' RSQLite, Sparklyr, RPostgreSQL, and RPostgres.  This is the
#' full set of dbi options for rquery.
#'
#' @param db DBI database connection
#' @return named list of options
#'
#' @seealso \code{\link{dbi_connection_tests}}
#'
#' @examples
#'
#' if(requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   print(dbi_connection_name(my_db))
#'   print(dbi_connection_preferences(my_db))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
dbi_connection_preferences <- function(db) {
  cname <- dbi_connection_name(db)
  opts <- list()
  opts[[paste(c("rquery", cname, "use_pass_limit"), collapse = ".")]] <- TRUE
  opts[[paste(c("rquery", cname, "use_DBI_dbExistsTable"), collapse = ".")]] <- TRUE
  opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "use_DBI_dbExecute"), collapse = ".")]] <- TRUE
  opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- TRUE
  opts[[paste(c("rquery", cname, "control_temporary"), collapse = ".")]] <- TRUE
  opts[[paste(c("rquery", cname, "control_rownames"), collapse = ".")]] <- TRUE
  if(connection_is_spark(db)) {
    opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "control_rownames"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- FALSE
  }
  if(cname == "PostgreSQLConnection") { # RPostgreSQL::PostgreSQL()
    opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- FALSE
  }
  opts
}


brute_rm_table <- function(db, table_name) {
  tryCatch(
    DBI::dbGetQuery(db, paste("DROP TABLE",
                              quote_identifier(db, table_name))),
    error = function(e) {e},
    warning = function(w) {w})
  NULL
}

#' Try and test database for some option settings.
#'
#' These settings are estimated by experiments.  This is not
#' the full set of options- but just the ones tested here.
#'
#' @param db DBI database connection
#' @return named list of options
#'
#' @seealso \code{\link{dbi_connection_preferences}}
#'
#' @examples
#'
#' if(requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   print(dbi_connection_name(my_db))
#'   print(dbi_connection_tests(my_db))
#'   # print(options(dbi_connection_tests(my_db)))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
dbi_connection_tests <- function(db) {
  cname <- dbi_connection_name(db)
  opts <- list()
  opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "use_DBI_dbExecute"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "control_temporary"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "control_rownames"), collapse = ".")]] <- FALSE
  # Run config tests in addition to dealing with known cases
  obscure_name <- wrapr::mk_tmp_name_source("dbi_test")()
  brute_rm_table(db, obscure_name)
  # see if we can turn off rownames
  tryCatch(
    {
      DBI::dbWriteTable(db,
                        obscure_name,
                        data.frame(x = 1),
                        row.names = FALSE)
      opts[[paste(c("rquery", cname, "control_rownames"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  brute_rm_table(db, obscure_name)
  # see if we can set temporary
  tryCatch(
    {
      DBI::dbWriteTable(db,
                        obscure_name,
                        data.frame(x = 1),
                        temporary = TRUE)
      opts[[paste(c("rquery", cname, "control_temporary"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  brute_rm_table(db, obscure_name)
  # see if dbExists works
  exists_prior <- DBI::dbExistsTable(db, obscure_name)
  DBI::dbWriteTable(db,
                    obscure_name,
                    data.frame(x = 1))
  exists_after <- DBI::dbExistsTable(db, obscure_name)
  brute_rm_table(db, obscure_name)
  exists_after2 <- DBI::dbExistsTable(db, obscure_name)
  opts[[paste(c("rquery", cname, "use_DBI_dbExistsTable"), collapse = ".")]] <-
    (!exists_prior) && exists_after && (!exists_after2)
  # see if DBI::dbRemoveTable works
  DBI::dbWriteTable(db,
                    obscure_name,
                    data.frame(x = 1))
  tryCatch(
    {
      DBI::dbRemoveTable(db, obscure_name)
      DBI::dbWriteTable(db,
                        obscure_name,
                        data.frame(x = 1))
      opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  # see if DBI::dbListFields works
  brute_rm_table(db, obscure_name)
  DBI::dbWriteTable(db,
                    obscure_name,
                    data.frame(x = 1))
  tryCatch(
    {
      flds <- DBI::dbListFields(db, obscure_name)
      opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <-
        paste(flds, collapse = " ") == "x"
    },
    error = function(e) { e },
    warning = function(w) { w })
  # see if DBI::dbExecute works
  tryCatch(
    {
      DBI::dbExecute(db, paste("DROP TABLE",
                               DBI::dbQuoteIdentifier(db, obscure_name)))
      opts[[paste(c("rquery", cname, "use_DBI_dbExecute"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  brute_rm_table(db, obscure_name)
  tryCatch(
    {
      DBI::dbExecute(db, paste("CREATE TEMPORARY TABLE",
                               DBI::dbQuoteIdentifier(db, obscure_name),
                               "( x INT )"))
      opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  # make sure we are clean
  brute_rm_table(db, obscure_name)
  opts
}

#' Set a database connection option.
#'
#' @param db DBI database connection handle.
#' @param optname character, single option name.
#' @param default what to return if not set.
#' @return option value
#'
#' @examples
#'
#' if(requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   print(getDBOption(my_db, "use_DBI_dbExecute"))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
getDBOption <- function(db, optname, default) {
  cname <- dbi_connection_name(db)
  key <- paste(c("rquery", cname, optname), collapse = ".")
  val <- getOption(key, default = default)
  val
}

#' Set a database connection option.
#'
#' @param db DBI database connection handle.
#' @param optname character, single option name.
#' @param val value to set
#' @return named list containing old value if any (invisible).
#'
#' @export
#'
setDBOption <- function(db, optname, val) {
  cname <- dbi_connection_name(db)
  key <- paste(c("rquery", cname, optname), collapse = ".")
  wrapr::let(
    c(KEY = key),
    options(KEY = val)
  )
}
