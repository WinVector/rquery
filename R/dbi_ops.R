

# work around common not fully DBI databases issues


#' Check if a table exists.
#'
#' @param db DBI connection
#' @param table_name character table name
#' @return logical TRUE if table exists.
#'
#' @export
#'
dbi_table_exists <- function(db, table_name) {
  # Would like to just return DBI::dbExistsTable(db, table_name)
  # But RPostgreSQL ‘0.6.2’ does not implement it.
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
#' @export
#'
dbi_remove_table <- function(db, table_name) {
  if(!is.null(table_name)) {
    if(dbi_table_exists(db, table_name)) {
      if(getDBOption(db, "use_DBI_dbRemoveTable", FALSE)) {
        DBI::dbRemoveTable(db, table_name)
      } else {
        DBI::dbExecute(db,
                       paste0("DROP TABLE ",
                              quote_identifier(db, table_name)))
      }
      return(TRUE)
    }
  }
  return(FALSE)
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

#' Get reasonable options for a DB connection.
#'
#' @param db DBI database connection
#' @return named list of options
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
  opts[[paste(c("rquery", cname, "control_temporary"), collapse = ".")]] <- TRUE
  opts[[paste(c("rquery", cname, "rownames_false"), collapse = ".")]] <- TRUE
  if(connection_is_spark(db)) {
    opts[[paste(c("rquery", cname, "control_temporary"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "control_rownames"), collapse = ".")]] <- FALSE
  }
  # RPostgres::Postgres() "PqConnection"
  if(cname == "PostgreSQLConnection") { # RPostgreSQL::PostgreSQL()
    opts[[paste(c("rquery", cname, "use_DBI_dbExistsTable"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- FALSE
  }
  # Can also run config tests in addition to dealing with known cases
  opts
}

getDBOption <- function(db, optname, default) {
  cname <- dbi_connection_name(db)
  key <- paste(c("rquery", cname, optname), collapse = ".")
  val <- getOption(key, default = default)
  val
}

