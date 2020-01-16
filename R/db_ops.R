

#' Execute a get query, typically a non-update that is supposed to return results.
#'
#' @param db database connection handle
#' @param q character query
#' @return nothing
#'
#' @seealso \code{\link{db_td}}
#'
#' @export
#'
rq_get_query <- function(db, q) {
  # first shot- see if it is a db info with function overrriden
  if(is.null(db)) {
    stop("rquery::rq_get_query db was null")
  }
  connection <- db
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_get_query
    if(!is.null(f)) {
      return(f(db, q))
    }
    connection <- db$connection
  }
  if(is.null(connection)) {
    stop("rquery::rq_get_query db$connection was null")
  }
  if(requireNamespace("DBI", quietly = TRUE)) {
    # fall back to DBI
    return(DBI::dbGetQuery(connection, q))
  }
  stop("rquery::rq_get_query no underlying implementation found (may need DBI)")
}


#' Execute a query, typically an update that is not supposed to return results.
#'
#' @param db database connection handle
#' @param q character query
#' @return nothing
#'
#' @seealso \code{\link{db_td}}
#'
#' @export
#'
rq_execute <- function(db, q) {
  if(is.null(db)) {
    stop("rquery::rq_execute db was null")
  }
  # first shot- see if it is a db info with function overrriden
  connection <- db
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_execute
    if(!is.null(f)) {
      return(f(db, q))
    }
    connection_options <- db$connection_options
    connection <- db$connection
  }
  if(is.null(connection)) {
    stop("rquery::rq_execute db$connection was null")
  }
  res <- NULL
  if(getDBOption(db, "use_DBI_dbExecute", TRUE, connection_options) && requireNamespace("DBI", quietly = TRUE)) {
    res <- DBI::dbExecute(connection, q)
  } else {
    res <- rq_get_query(db, q)
  }
  res
}



#' Check if a table exists.
#'
#' @param db Connection handle
#' @param table_name character table name
#' @param ... not used, force later argument to bind by name
#' @param qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @return logical TRUE if table exists.
#'
#' @seealso \code{\link{db_td}}
#'
#' @export
#'
rq_table_exists <- function(db, table_name,
                            ...,
                            qualifiers = NULL) {
  if(is.null(db)) {
    stop("rquery::rq_table_exists db was null")
  }
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_table_exists")
  # first shot- see if it is a db info with function overrriden
  connection <- db
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_table_exists
    if(!is.null(f)) {
      return(f(db, table_name, qualifiers = qualifiers))
    }
    connection_options <- db$connection_options
    connection <- db$connection
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  } else {
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  }
  if(is.null(connection)) {
    stop("rquery::rq_table_exists db$connection was null")
  }
  if(getDBOption(db, "use_INFORMATION_SCHEMA", FALSE, connection_options)) {
    schema <- qualifiers[["schema"]]
    if(is.null(schema)) {
      schema = "public"
    }
    info <- DBI::dbGetQuery(connection,
                            paste("SELECT * FROM INFORMATION_SCHEMA.TABLES where TABLE_NAME =",
                                  quote_string(db, table_name),
                                  "AND TABLE_SCHEMA =",
                                  quote_string(db, schema), "LIMIT 1"))
    return(nrow(info)>0)
  }
  # Would like to just return DBI::dbExistsTable(db, table_name)
  if(getDBOption(db, "use_DBI_dbExistsTable", FALSE, connection_options) && requireNamespace("DBI", quietly = TRUE)) {
    return(DBI::dbExistsTable(connection, table_name))
  }
  # brute force try to query table (can write error msgs, ugh)
  q <- paste0("SELECT * FROM ",
              q_table_name,
              " LIMIT 1")
  # others do a 0=1 thing, may be faster but let's try this.
  tryCatch(
    {
      v <- rq_get_query(db, q)
      if(is.null(v)) {
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
#' @param db Connection handle
#' @param table_name character table name
#' @param ... not used, force later argument to bind by name
#' @param qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @return character list of column names
#'
#' @export
#'
rq_colnames <- function(db, table_name,
                        ...,
                        qualifiers = NULL) {
  if(is.null(db)) {
    stop("rquery::rq_colnames db was null")
  }
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_colnames")
  # first shot- see if it is a db info with function overrriden
  connection <- db
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_colnames
    if(!is.null(f)) {
      return(f(db, table_name))
    }
    connection_options <- db$connection_options
    connection <- db$connection
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  } else {
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  }
  if(is.null(connection)) {
    stop("rquery::rq_colnames db$connection was null")
  }
  # DBI::dbListFields fails intermitnently, and sometimes gives wrong results
  # filed as: https://github.com/tidyverse/dplyr/issues/3204
  if(getDBOption(db, "use_DBI_dbListFields", FALSE, connection_options) && requireNamespace("DBI", quietly = TRUE)) {
    # this path doesn't handle schemas
    return(DBI::dbListFields(connection, table_name))
  }
  # below is going to have issues to R-column name conversion!
  q <- paste0("SELECT * FROM ",
              q_table_name,
              " LIMIT 1")
  v <- rq_get_query(db, q)
  colnames(v)
}


#' Get column types by example values as a data.frame.
#'
#' Example values not necessarily all from same row.  Taking values from different rows is
#' to try to work around NA not carrying type/class info in many cases.
#'
#' @param db Connection handle.
#' @param table_name character table name referring to a non-empty table.
#' @param ... force later arguments to bind by name.
#' @param qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @param prefer_not_NA logical, if TRUE try to find an non-NA example for all columns (FALSE just for logical columns).
#' @param force_check logical, if TRUE perform checks regardless of check_logical_column_types option setting.
#' @return single row data.frame with example values, not all values necessarily from same database row.
#'
#' @examples
#'
#' if(requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#'   # getDBOption(db, "check_logical_column_types", FALSE)
#'   # options(rq_connection_tests(db))
#'   # getDBOption(db, "check_logical_column_types", FALSE)
#'
#'   d <- data.frame(w= c(NA, 1L),
#'                   x= c(NA, 2.0),
#'                   y= factor(c(NA, "x")),
#'                   z= c(NA, "y"),
#'                   want = c(1, 0),
#'                   stringsAsFactors=FALSE)
#'   d <- rq_copy_to(db, "d", d,
#'                    overwrite = TRUE,
#'                    temporary = TRUE)
#'   res <- d %.>%
#'     extend(.,
#'                wc %:=% ifelse(w>1, "x", "y"),
#'                wn %:=% ifelse(w>1, 1, 2),
#'                xc %:=% ifelse(x>1, "x", "y"),
#'                xn %:=% ifelse(x>1, 1, 2),
#'                yc %:=% ifelse(y=="a", "x", "y"),
#'                yn %:=% ifelse(y=="a", "x", "y")) %.>%
#'     materialize(db, .)
#'   resn <- DBI::dbQuoteIdentifier(db, res$table_name)
#'   print("full table types")
#'   print(str(DBI::dbGetQuery(db, paste("SELECT * FROM", resn))))
#'   print("single row mis-reported types")
#'   print(str(DBI::dbGetQuery(db, paste("SELECT * FROM", resn, "WHERE want=1"))))
#'   print("rq_coltypes correct synthetic example row types")
#'   print(str(rq_coltypes(db, res$table_name, force_check = TRUE)))
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#'
rq_coltypes <- function(db, table_name,
                        ...,
                        qualifiers = NULL,
                        prefer_not_NA = FALSE,
                        force_check = FALSE) {
  if(is.null(db)) {
    stop("rquery::rq_coltypes db was null")
  }
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_coltypes")
  # first shot- see if it is a db info with function overrriden
  connection <- db
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_coltypes
    if(!is.null(f)) {
      return(f(db, table_name,
               qualifiers = qualifiers,
               prefer_not_NA = prefer_not_NA,
               force_check = force_check))
    }
    connection_options <- db$connection_options
    connection <- db$connection
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  } else {
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  }
  if(is.null(connection)) {
    stop("rquery::rq_coltypes db$connection was null")
  }
  # RSQLite returns logical type for any returned column
  # that is entirely NA, regardless of storage type.
  # below is going to have issues to to R-column name conversion!
  q <- paste("SELECT * FROM", q_table_name, "LIMIT 1")
  v <- rq_get_query(db, q)
  if((nrow(v)>0) &&
     (force_check || getDBOption(db, "check_logical_column_types", FALSE, connection_options))) {
    for(ci in colnames(v)) {
      cv <- v[[ci]]
      if(is.na(cv)) {
        if(prefer_not_NA || is.logical(cv)) {
          cn <- quote_identifier(db, ci)
          qi <- paste("SELECT", cn, "FROM ", q_table_name, "WHERE", cn, "IS NOT NULL LIMIT 1")
          vi <- rq_get_query(db, qi)
          if(nrow(vi)>0) {
            v[[ci]] <- vi[[ci]]
          }
        }
      }
    }
  }
  v
}


#' Remove table
#'
#' @param db database connection.
#' @param table_name character, name of table to create.
#' @param ... not used, force later argument to bind by name
#' @param qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @return logical TRUE if table existed, else FALSE
#'
#' @seealso \code{\link{db_td}}
#'
#' @export
#'
rq_remove_table <- function(db, table_name,
                            ...,
                            qualifiers = NULL) {
  if(is.null(db)) {
    stop("rquery::rq_remove_table db was null")
  }
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_remove_table")
  # first shot- see if it is a db info with function overrriden
  connection_options <- NULL
  connection <- db
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_remove_table
    if(!is.null(f)) {
      return(f(db, table_name))
    }
    connection_options <- db$connection_options
    connection <- db$connection
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  } else {
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  }
  if(is.null(connection)) {
    stop("rquery::rq_remove_table db$connection was null")
  }
  if(!is.null(table_name)) {
    if(rq_table_exists(db, table_name, qualifiers = qualifiers)) {
      if(getDBOption(db, "use_DBI_dbRemoveTable", FALSE, connection_options) && requireNamespace("DBI", quietly = TRUE)) {
        DBI::dbRemoveTable(connection, table_name)
      } else {
        if(getDBOption(db, "use_DROP_TABLE_IF_EXISTS", FALSE, connection_options)) {
          rq_execute(db,
                     paste("DROP TABLE IF EXISTS",
                           q_table_name))
        } else {
          rq_execute(db,
                     paste("DROP TABLE",
                           q_table_name))
        }
      }
      return(TRUE)
    }
  }
  return(FALSE)
}


# try not to use this too many places, prefer the configs
connection_is_sparklyr <- function(db) {
  if("rquery_db_info" %in% class(db)) {
    if(!db$is_dbi) {
      return(FALSE)
    }
    db <- db$connection
  }
  length(intersect(c("spark_connection", "spark_shell_connection"),
                   class(db)))>=1
}

maybe_dbi_table_id <- function(table_name, qualifiers) {
  # https://github.com/r-dbi/odbc/issues/91
  if("schema" %in% names(qualifiers)) {
    return(DBI::Id(table = table_name, schema = qualifiers[["schema"]]))
  }
  # DBI::Id(table = table_name) # RPostgreSQL can't use this form in some cases
  table_name
}


#' Copy local R table to remote data handle.
#'
#' @param db database connection handle.
#' @param table_name name of table to create.
#' @param d data.frame to copy to database.
#' @param ... force later argument to be by name
#' @param qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @param overwrite logical, if TRUE try to overwrite existing table.
#' @param temporary logical, if TRUE try to mark table as temporary.
#' @param rowidcolumn character, name to land row-ids.
#' @return a relop representation of the data
#'
#' @seealso \code{\link{db_td}}, \code{\link{mk_td}}, \code{\link{materialize}}, \code{\link{execute}}, \code{\link{to_sql}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(db, 'd',
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
rq_copy_to <- function(db, table_name, d,
                       ...,
                       qualifiers = NULL,
                       overwrite = FALSE,
                       temporary = TRUE,
                       rowidcolumn = NULL) {
  if(is.null(db)) {
    stop("rquery::rq_copy_to db was null")
  }
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_copy_to")
  # first shot- see if it is a db info with function overrriden
  connection_options <- NULL
  connection <- db
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_copy_to
    if(!is.null(f)) {
      return(f(db, table_name, d,
               qualifiers = qualifiers,
               overwrite = overwrite,
               temporary = temporary,
               rowidcolumn = rowidcolumn))
    }
    connection_options <- db$connection_options
    connection <- db$connection
  }
  if(is.null(connection)) {
    stop("rquery::rq_copy_to db$connection was null")
  }
  if(!is.null(rowidcolumn)) {
    d[[rowidcolumn]] <- seq_len(nrow(d))
  }
  can_set_temp <- getDBOption(db, "control_temporary", NULL, connection_options)
  can_set_rownames <- getDBOption(db, "control_rownames", NULL, connection_options)
  if(connection_is_sparklyr(db)) {
    # TODO: remove all of the Sparklyr special cases
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
  if("rquery_db_info" %in% class(db) && (!db$is_dbi)) {
    stop("rquery::rq_copy_to fell back to DBI methods for connection declared not DBI")
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery::rq_copy_to without per-connection implemention need DBI package")
  }
  if(rq_table_exists(db, table_name, qualifiers = qualifiers)) {
    if(overwrite) {
      # sparklyr 0.7.0 can't take overwrite argument
      rq_remove_table(db, table_name, qualifiers = qualifiers)
    } else {
      stop(paste("rquery::rq_copy_to table", table_name, "exists and overwrite==FALSE"))
    }
  }
  if(can_set_temp) {
    if(can_set_rownames) {
      DBI::dbWriteTable(connection,
                        maybe_dbi_table_id(table_name, qualifiers = qualifiers),
                        d,
                        temporary = temporary,
                        row.names = FALSE)
    } else {
      DBI::dbWriteTable(connection,
                        maybe_dbi_table_id(table_name, qualifiers = qualifiers),
                        d,
                        temporary = temporary)
    }
  } else {
    if(temporary && getOption("rquery.verbose")) {
      warning("setting rquery::rq_copy_to setting temporary=FALSE")
    }
    if(can_set_rownames) {
      DBI::dbWriteTable(connection,
                        maybe_dbi_table_id(table_name, qualifiers = qualifiers),
                        d,
                        row.names = FALSE)
    } else {
      DBI::dbWriteTable(connection,
                        maybe_dbi_table_id(table_name, qualifiers = qualifiers),
                        d)
    }
  }
  db_td(db, table_name, qualifiers = qualifiers)
}

#' Count rows and return as numeric
#'
#' @param db database connection
#' @param table_name character, name of table
#' @param ... not used, force later argument to bind by name
#' @param qualifiers optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.
#' @return numeric row count
#'
#' @seealso \code{\link{db_td}}
#'
#' @export
#'
rq_nrow <- function(db, table_name,
                    ...,
                    qualifiers = NULL) {
  if(is.null(db)) {
    stop("rquery::rq_nrow db was null")
  }
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_nrow")
  # first shot- see if it is a db info with function overrriden
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_nrow
    if(!is.null(f)) {
      return(f(db, table_name, qualifiers = qualifiers))
    }
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  } else {
    q_table_name <- quote_table_name(db, table_name, qualifiers = qualifiers)
  }
  nrowst <- rq_get_query(
    db,
    paste0("SELECT COUNT(1) FROM ",
           q_table_name))
  # integer64 was coming back from RPostgres
  # and that does not work as numeric in pmin()
  nrows <- as.numeric(nrowst[[1]][[1]])
  nrows
}


#' Build a canonical name for a db connection class.
#'
#' @param db Database connection handle.
#' @return character, key version of handle for option lookups.
#'
#' @examples
#'
#' if(requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   print(rq_connection_name(my_db))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
rq_connection_name <- function(db) {
  if(is.null(db)) {
    return("NULL")
  }
  if("rquery_db_info" %in% class(db)) {
    cname <- db$cname
    if(!is.null(cname)) {
      return(cname)
    }
    db <- db$connection
  }
  if(is.null(db)) {
    return("NULL")
  }
  cls <- sort(class(db))
  cls <- paste(cls, collapse = "_")
  cls <- gsub("[^[:alnum:]]+", "_", cls)
  cls
}



#' Get advice for a DB connection (beyond tests).
#'
#' These settings are set by the package maintainers based on experience with
#' specific databases.
#'
#' @param db database connection handle
#' @return named list of options
#'
#' @seealso \code{\link{rq_connection_tests}}
#'
#' @examples
#'
#' if(requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   print(rq_connection_name(my_db))
#'   print(rq_connection_advice(my_db))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
rq_connection_advice <- function(db) {
  cname <- rq_connection_name(db)
  opts <- list()
  expr_map <- list()
  big_int <- 2^28
  big_int_m_1 <- "268435455.0"
  rand_expr <- list( # ingore args
    pre_sql_fn("ABS"),
    pre_sql_token("("),
    pre_sql_fn("MOD"),
    pre_sql_token("("),
    pre_sql_fn("RANDOM"), pre_sql_token("("), pre_sql_token(")"),
    pre_sql_token(","),
    pre_sql_token(big_int),
    pre_sql_token(")"),
    pre_sql_token("/"),
    pre_sql_token(big_int_m_1),
    pre_sql_token(")"))
  if(cname=="SQLiteConnection") { # RSQLite
    expr_map[["MOD"]] <- list(pre_sql_token("("),
                              3,
                              pre_sql_token("%"),
                              5,
                              pre_sql_token(")"))
    rand_expr <- list( # ingore args
      pre_sql_fn("ABS"),
      pre_sql_token("("),
      pre_sql_token("("),
      pre_sql_fn("RANDOM"), pre_sql_token("("), pre_sql_token(")"),
      pre_sql_token("%"),
      pre_sql_token(big_int),
      pre_sql_token(")"),
      pre_sql_token("/"),
      pre_sql_token(big_int_m_1),
      pre_sql_token(")"))
  }
  opts[[paste(c("rquery", cname, "use_DROP_TABLE_IF_EXISTS"), collapse = ".")]] <- TRUE
  if(connection_is_sparklyr(db)) {
    opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "control_rownames"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
  }
  # TODO: sparkR support here instead of in https://github.com/WinVector/rquery/blob/master/db_examples/SparkR.md
  if(cname == "PostgreSQLConnection") { # RPostgreSQL::PostgreSQL()
    opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
    #opts[[paste(c("rquery", cname, "use_DBI_dbExistsTable"), collapse = ".")]] <- FALSE # fails on some CREATE AS tables
    opts[[paste(c("rquery", cname, "use_INFORMATION_SCHEMA"), collapse = ".")]] <- TRUE
  }
  if(cname =="PqConnection") { # RPostgres::Postgres()
    # schema issues
    opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
    #opts[[paste(c("rquery", cname, "use_DBI_dbExistsTable"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_INFORMATION_SCHEMA"), collapse = ".")]] <- TRUE
  }
  expr_map[["rand"]] <- rand_expr
  opts[[paste(c("rquery", cname, "expr_map"), collapse = ".")]] <- expr_map
  opts
}


brute_rm_table <- function(db, table_name,
                           ...,
                           qualifiers = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::brute_rm_table")
  if(is.null(db)) {
    stop("rquery::brute_rm_table db was null")
  }
  tryCatch(
    rq_execute(db, paste("DROP TABLE",
                         quote_table_name(db, table_name, qualifiers = qualifiers))),
    error = function(e) {e},
    warning = function(w) {w})
  NULL
}

#' Try and test database for some option settings.
#'
#' These settings are estimated by experiments.  This is not
#' the full set of options- but just the ones tested here.
#'
#' Note: tests are currently run in the default schema. Also it is normal to see some warning/error
#' messages as different database capabilities are tested.
#'
#' @param db database connection handle.
#' @param ... force later arguments to bind by name.
#' @param overrides named character vector or list, options (just name, not DB qualification) to force
#' @param use_advice logical if TRUE incorporate hard-coded advice.
#' @return named list of options
#'
#' @seealso \code{\link{rq_connection_advice}}
#'
#' @examples
#'
#' if(requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   print(rq_connection_name(my_db))
#'   print(rq_connection_tests(my_db,
#'      overrides = c("use_DBI_dbExistsTable" = FALSE)))
#'   # the following would set options
#'   # print(options(rq_connection_tests(my_db)))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
rq_connection_tests <- function(db,
                                ...,
                                overrides = NULL,
                                use_advice = TRUE) {
  if(is.null(db)) {
    stop("rquery::rq_connection_tests db was null")
  }
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_connection_tests")
  connection <- db
  if("rquery_db_info" %in% class(db)) {
    if(!db$is_dbi) {
      stop("rquery::rq_connection_tests only applies to dbi handles")
    }
    if(!requireNamespace("DBI", quietly = TRUE)) {
      stop("rquery::rq_connection_tests requires the DBI package")
    }
    connection <- db$connection
  }
  cname <- rq_connection_name(db)
  opts <- list()
  opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "use_DBI_dbExecute"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "control_temporary"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "control_temporary_view"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "control_rownames"), collapse = ".")]] <- FALSE
  # Run config tests in addition to dealing with known cases
  nm_source <- wrapr::mk_tmp_name_source("rq_test")
  obscure_name <- nm_source()
  obscure_name_q <- quote_identifier(db, obscure_name)
  obscure_name2 <- nm_source()
  obscure_name2_q <- quote_identifier(db, obscure_name2)
  brute_rm_table(db, obscure_name)
  brute_rm_table(db, obscure_name2)
  # see if we can turn off rownames
  tryCatch(
    {
      DBI::dbWriteTable(connection,
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
      DBI::dbWriteTable(connection,
                        obscure_name,
                        data.frame(x = 1),
                        temporary = TRUE)
      opts[[paste(c("rquery", cname, "control_temporary"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  brute_rm_table(db, obscure_name)
  # see if dbExists works
  exists_prior <- DBI::dbExistsTable(connection, obscure_name)
  DBI::dbWriteTable(connection,
                    obscure_name,
                    data.frame(x = 1))
  exists_after <- DBI::dbExistsTable(connection, obscure_name)
  brute_rm_table(db, obscure_name)
  exists_after2 <- DBI::dbExistsTable(connection, obscure_name)
  opts[[paste(c("rquery", cname, "use_DBI_dbExistsTable"), collapse = ".")]] <-
    (!exists_prior) && exists_after && (!exists_after2)
  # see if DBI::dbRemoveTable works
  DBI::dbWriteTable(connection,
                    obscure_name,
                    data.frame(x = 1))
  tryCatch(
    {
      DBI::dbRemoveTable(connection, obscure_name)
      DBI::dbWriteTable(connection,
                        obscure_name,
                        data.frame(x = 1))
      opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  # see if DBI::dbListFields works
  brute_rm_table(db, obscure_name)
  DBI::dbWriteTable(connection,
                    obscure_name,
                    data.frame(x = 1))
  tryCatch(
    {
      flds <- DBI::dbListFields(connection, obscure_name)
      opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <-
        paste(flds, collapse = " ") == "x"
    },
    error = function(e) { e },
    warning = function(w) { w })
  # see if DBI::dbExecute works
  tryCatch(
    {
      DBI::dbExecute(connection, paste("DROP TABLE",
                                       obscure_name_q))
      opts[[paste(c("rquery", cname, "use_DBI_dbExecute"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  brute_rm_table(db, obscure_name)
  # check on temporary table
  tryCatch(
    {
      DBI::dbGetQuery(connection, paste("CREATE TEMPORARY TABLE",
                                        obscure_name_q,
                                        "( x INT )"))
      opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  # # check on temporary view, note need to DROP VIEW to reinstate this test
  # tryCatch(
  #   {
  #     DBI::dbGetQuery(connection, paste("CREATE TEMPORARY VIEW",
  #                                       obscure_name2_q,
  #                                       "AS SELECT * FROM ",
  #                                       obscure_name))
  #     opts[[paste(c("rquery", cname, "control_temporary_view"), collapse = ".")]] <- TRUE
  #   },
  #   error = function(e) { e },
  #   warning = function(w) { w })
  # brute_rm_table(db, obscure_name)
  # see if NA columns masquerade as logical
  # (RSQLite has this property for some derived columns)
  d <- data.frame(w= c(NA, 1L),
                  x= c(NA, 2.0),
                  y= factor(c(NA, "x")),
                  z= c(NA, "y"),
                  want = c(1, 0),
                  stringsAsFactors=FALSE)
  d <- rq_copy_to(db, obscure_name, d,
                  overwrite = TRUE,
                  temporary = TRUE)
  # make column refs not look like unbound references
  w <- NULL # don't appear unbound
  want <- NULL # don't appear unbound
  wc  <- NULL # don't appear unbound
  wn  <- NULL # don't appear unbound
  x  <- NULL # don't appear unbound
  xc  <- NULL # don't appear unbound
  xn  <- NULL # don't appear unbound
  y  <- NULL # don't appear unbound
  yc  <- NULL # don't appear unbound
  yn <- NULL # don't appear unbound
  local_sample <- d %.>%
    extend(.,
           wc %:=% ifelse(w>1, "x", "y"),
           wn %:=% ifelse(w>1, 1, 2),
           xc %:=% ifelse(x>1, "x", "y"),
           xn %:=% ifelse(x>1, 1, 2),
           yc %:=% ifelse(y=="a", "x", "y"),
           yn %:=% ifelse(y=="a", "x", "y")) %.>%
    select_rows(.,
                want == 1) %.>%
    execute(db, .)
  logical_col <- vapply(colnames(local_sample),
                        function(ci) is.logical(local_sample[[ci]]), logical(1))
  bad_types <- any(logical_col)
  opts[[paste(c("rquery", cname, "check_logical_column_types"), collapse = ".")]] <- bad_types
  brute_rm_table(db, obscure_name)
  if(use_advice) {
    advice <- rq_connection_advice(db)
    for(ki in names(advice)) {
      vi <- advice[[ki]]
      opts[[ki]] <- vi
    }
  }
  for(ni in names(overrides)) {
    vi <- overrides[[ni]]
    keyi <- paste(c("rquery", cname, ni), collapse = ".")
    if(!(keyi %in% names(opts))) {
      stop(paste("rquery::rq_connection_tests unknown option", ni))
    }
    opts[[keyi]] <- vi
  }
  opts
}

#' Get a database connection option.
#'
#' Note: we are moving away from global options to options in the DB handle.
#'
#' @param db database connection handle.
#' @param optname character, single option name.
#' @param default what to return if not set.
#' @param connection_options name list of per connection options.
#' @return option value
#'
#' @examples
#'
#' if(requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   print(getDBOption(my_db, "use_DBI_dbExecute"))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
getDBOption <- function(db, optname, default,
                        connection_options = list()) {
  cname <- rq_connection_name(db)
  key <- paste(c("rquery", cname, optname), collapse = ".")
  val <- connection_options[[key]]
  if(!is.null(val)) {
    return(val)
  }
  if("rquery_db_info" %in% class(db)) {
    co <- db$connection_options
    if(!is.null(co)) {
      val <- co[[key]]
    }
  }
  if(!is.null(val)) {
    return(val)
  }
  val <- getOption(key, default = default)
  val
}

#' Set a database connection option.
#'
#' If db is of class rquery_db_info it sets the appropriate connection option, not the global state.
#'
#' @param db rquery_db_info instance
#' @param optname character, single option name.
#' @param val value to set
#' @return db
#'
#' @export
#'
setDBOpt <- function(db, optname, val) {
  if(!("rquery_db_info" %in% class(db))) {
    stop("rquery::setDBOpt db must be of class rquery_db_info")
  }
  cname <- rq_connection_name(db)
  key <- paste(c("rquery", cname, optname), collapse = ".")
  db$connection_options[[key]] <- val
  db
}

#' Set a database connection option.
#'
#'
#' Note: we are moving away from global options to options in the DB handle.
#' Prefer \code{\link{setDBOpt}}.
#'
#' @param db database connection handle.
#' @param optname character, single option name.
#' @param val value to set
#' @return original options value
#'
#' @export
#'
setDBOption <- function(db, optname, val) {
  cname <- rq_connection_name(db)
  key <- paste(c("rquery", cname, optname), collapse = ".")
  wrapr::let(
    c(KEY = key),
    options(KEY = val)
  )
}
