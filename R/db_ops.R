

# wrap database or data system (Spark) adapters


#' Execute a get query, typcially a non-update that is supposed to return results.
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
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_get_query
    if(!is.null(f)) {
      return(f(db, q))
    }
    db <- db$connection
  }
  # fall back to DBI
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  DBI::dbGetQuery(db, q)
}


#' Execute a query, typcially an update that is not supposed to return results.
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
  # first shot- see if it is a db info with function overrriden
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_execute
    if(!is.null(f)) {
      return(f(db, q))
    }
    connection_options <- db$connection_options
    db <- db$connection
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  res <- NULL
  if(getDBOption(db, "use_DBI_dbExecute", TRUE, connection_options)) {
    res <- DBI::dbExecute(db, q)
  } else {
    rq_get_query(db, q)
  }
  res
}



#' Check if a table exists.
#'
#' @param db Connection handle
#' @param table_name character table name
#' @return logical TRUE if table exists.
#'
#' @seealso \code{\link{db_td}}
#'
#' @export
#'
rq_table_exists <- function(db, table_name) {
  # first shot- see if it is a db info with function overrriden\
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_table_exists
    if(!is.null(f)) {
      return(f(db, table_name))
    }
    connection_options <- db$connection_options
    db <- db$connection
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  # Would like to just return DBI::dbExistsTable(db, table_name)
  if(getDBOption(db, "use_DBI_dbExistsTable", TRUE, connection_options)) {
    return(DBI::dbExistsTable(db, table_name))
  }
  q <- paste0("SELECT * FROM ",
              quote_identifier(db, table_name),
              " LIMIT 1")
  # others do a 0=1 thing, may be faster but let's try this.
  tryCatch(
    {
      v <- rq_get_query(db, q)
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
#' @param db Connection handle
#' @param table_name character table name
#' @return character list of column names
#'
#' @export
#'
rq_colnames <- function(db, table_name) {
  # first shot- see if it is a db info with function overrriden
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_colnames
    if(!is.null(f)) {
      return(f(db, table_name))
    }
    connection_options <- db$connection_options
    db <- db$connection
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  # DBI::dbListFields fails intermitnently, and sometimes gives wrong results
  # filed as: https://github.com/tidyverse/dplyr/issues/3204
  if(getDBOption(db, "use_DBI_dbListFields", FALSE, connection_options)) {
    return(DBI::dbListFields(db, table_name))
  }
  # below is going to have issues to to R-column name conversion!
  q <- paste0("SELECT * FROM ",
              quote_identifier(db, table_name),
              " LIMIT 1")
  v <- rq_get_query(db, q)
  colnames(v)
}


#' Get column types by example values as a data.frame.
#'
#' Example values not necissarily all from same row.  Taking values from different rows is
#' to try to work around NA not carrying type/class info in many cases.
#'
#' @param db Connection handle.
#' @param table_name character table name refering to a non-empty table.
#' @param ... force later arguments to bind by name.
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
#'     extend_nse(.,
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
                         prefer_not_NA = FALSE,
                         force_check = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_coltypes")
  # first shot- see if it is a db info with function overrriden
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_coltypes
    if(!is.null(f)) {
      return(f(db, table_name,
               prefer_not_NA = prefer_not_NA,
               force_check = force_check))
    }
    connection_options <- db$connection_options
    db <- db$connection
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  # RSQLite returns logical type for any returned column
  # that is entirely NA, regardless of storage type.
  # below is going to have issues to to R-column name conversion!
  tn <- quote_identifier(db, table_name)
  q <- paste("SELECT * FROM", tn, "LIMIT 1")
  v <- rq_get_query(db, q)
  if((nrow(v)>0) &&
     (force_check || getDBOption(db, "check_logical_column_types", FALSE, connection_options))) {
    for(ci in colnames(v)) {
      cv <- v[[ci]]
      if(is.na(cv)) {
        if(prefer_not_NA || is.logical(cv)) {
          cn <- quote_identifier(db, ci)
          qi <- paste("SELECT", cn, "FROM ", tn, "WHERE", cn, "IS NOT NULL LIMIT 1")
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
#' @return logical TRUE if table existed, else FALSE
#'
#' @seealso \code{\link{db_td}}
#'
#' @export
#'
rq_remove_table <- function(db, table_name) {
  # first shot- see if it is a db info with function overrriden
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_remove_table
    if(!is.null(f)) {
      return(f(db, table_name))
    }
    connection_options <- db$connection_options
    db <- db$connection
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  if(!is.null(table_name)) {
    if(rq_table_exists(db, table_name)) {
      if(getDBOption(db, "use_DBI_dbRemoveTable", FALSE, connection_options)) {
        DBI::dbRemoveTable(db, table_name)
      } else {
        rq_execute(db,
                    paste("DROP TABLE",
                           quote_identifier(db, table_name)))
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

#' Copy local R table to remote data handle.
#'
#' @param db database connection handle.
#' @param table_name name of table to create.
#' @param d data.frame to copy to database.
#' @param ... force later argument to be by name
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
                        overwrite = FALSE,
                        temporary = TRUE,
                        rowidcolumn = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_copy_to")
  # first shot- see if it is a db info with function overrriden
  connection_options <- NULL
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_copy_to
    if(!is.null(f)) {
      return(f(db, table_name, d,
               overwrite = overwrite,
               temporary = overwrite,
               rowidcolumn = rowidcolumn))
    }
    connection_options <- db$connection_options
    db <- db$connection
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  if(!is.null(rowidcolumn)) {
    d[[rowidcolumn]] <- seq_len(nrow(d))
  }
  # sparklyr 0.7.0 does not take overwrite or row.names arguments
  if(overwrite) {
    rq_remove_table(db, table_name)
  }
  can_set_temp <- getDBOption(db, "control_temporary", NULL, connection_options)
  can_set_rownames <- getDBOption(db, "control_rownames", NULL, connection_options)
  if(connection_is_sparklyr(db)) {
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
      warning("setting rquery::rq_copy_to setting temporary=FALSE")
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
  db_td(db, table_name)
}

#' Count rows and return as numeric
#'
#' @param db database connetion
#' @param table_name character, name of table
#' @return numeric row count
#'
#' @seealso \code{\link{db_td}}
#'
#' @export
#'
rq_nrow <- function(db, table_name) {
  # first shot- see if it is a db info with function overrriden
  if("rquery_db_info" %in% class(db)) {
    f <- db$rq_nrow
    if(!is.null(f)) {
      return(f(db, table_name))
    }
    db <- db$connection
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery this function currently requires the DBI package")
  }
  nrowst <- rq_get_query(
    db,
    paste0("SELECT COUNT(1) FROM ",
           quote_identifier(db,
                                  table_name)))
  # integer64 was coming back from RPostgres
  # and that does not work as numeric in pmin()
  nrows <- as.numeric(nrowst[[1]][[1]])
  nrows
}


#' Build a cannonical name for a db connection class.
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
  if("rquery_db_info" %in% class(db)) {
    db <- db$connection
  }
  cls <- sort(class(db))
  cls <- paste(cls, collapse = "_")
  cls <- gsub("[^a-zA-Z]+", "_", cls)
  cls
}



#' Get advice for a DB connection (beyond tests).
#'
#' These settings are set by the package mainteners based on experience with
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
  if(connection_is_sparklyr(db)) {
    opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "control_rownames"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "zero_arg_fn_map"), collapse = ".")]] <-
      c("random" = "rand")
  }
  if(cname == "PostgreSQLConnection") { # RPostgreSQL::PostgreSQL()
    opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- FALSE
    opts[[paste(c("rquery", cname, "use_DBI_dbExistsTable"), collapse = ".")]] <- FALSE # fails on some CREATE AS tables
  }
  opts
}


brute_rm_table <- function(db, table_name) {
  tryCatch(
    rq_get_query(db, paste("DROP TABLE",
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
#' @param db database connection handle.
#' @param ... force later arguments to bind by name.
#' @param overrides named character vector or list, options (just name, not DB qualification) to force
#' @param use_advice logical if TRUE incorpeate hard-coded advice.
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
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::rq_connection_tests")
  if("rquery_db_info" %in% class(db)) {
    if(!db$is_dbi) {
      stop("rquery::rq_connection_tests only applies to dbi handles")
    }
    db <- db$connection
  }
  if(!requireNamespace("DBI", quietly = TRUE)) {
    stop("rquery::rq_connection_tests this function currently requires the DBI package")
  }
  cname <- rq_connection_name(db)
  opts <- list()
  opts[[paste(c("rquery", cname, "use_DBI_dbListFields"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "use_DBI_dbRemoveTable"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "use_DBI_dbExecute"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "control_temporary"), collapse = ".")]] <- FALSE
  opts[[paste(c("rquery", cname, "control_rownames"), collapse = ".")]] <- FALSE
  # Run config tests in addition to dealing with known cases
  obscure_name <- wrapr::mk_tmp_name_source("rq_test")()
  obscure_name_q <- quote_identifier(db, obscure_name)
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
                               obscure_name_q))
      opts[[paste(c("rquery", cname, "use_DBI_dbExecute"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  brute_rm_table(db, obscure_name)
  # check on temporary
  tryCatch(
    {
      DBI::dbGetQuery(db, paste("CREATE TEMPORARY TABLE",
                               obscure_name_q,
                               "( x INT )"))
      opts[[paste(c("rquery", cname, "create_temporary"), collapse = ".")]] <- TRUE
    },
    error = function(e) { e },
    warning = function(w) { w })
  brute_rm_table(db, obscure_name)
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
    extend_nse(.,
               wc %:=% ifelse(w>1, "x", "y"),
               wn %:=% ifelse(w>1, 1, 2),
               xc %:=% ifelse(x>1, "x", "y"),
               xn %:=% ifelse(x>1, 1, 2),
               yc %:=% ifelse(y=="a", "x", "y"),
               yn %:=% ifelse(y=="a", "x", "y")) %.>%
    select_rows_nse(.,
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

#' Set a database connection option.
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
  if(is.null(val)) {
    val <- getOption(key, default = default)
  }
  val
}

#' Set a database connection option.
#'
#' @param db database connection handle.
#' @param optname character, single option name.
#' @param val value to set
#' @return named list containing old value if any (invisible).
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
