
#' Create materialize statement.
#'
#' @param db database connection.
#' @param sql character single SQL statement.
#' @param table_name character, name of table to create.
#' @param ... force later arguments to bind by name.
#' @param temporary logical if TRUE try to create a temporary table.
#' @return modified SQL
#'
#' @seealso \code{\link{materialize}}, \code{\link{db_td}}, \code{\link{execute}}, \code{\link{to_sql}}, \code{\link{rq_copy_to}}, \code{\link{mk_td}}
#'
#' @examples
#'
#' if(requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   print(materialize_sql_statement(my_db,
#'                                   "SELECT x+1 FROM z",
#'                                   "restable"))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @noRd
#'
materialize_sql_statement <- function(db, sql, table_name,
                                      ...,
                                      temporary = FALSE) {
  # TODO: put in more per-connection options here (including partion controls)
  if(isTRUE(temporary)) {
    create_temp <- getDBOption(db, "create_temporary", NULL)
    if(is.null(create_temp)) {
      create_temp <- !connection_is_sparklyr(db)
    }
    if(!create_temp) {
      if(getOption("rquery.verbose")) {
        warning("setting rquery:::materialize_sql_statement setting temporary=FALSE")
      }
      temporary <- FALSE
    }
  } else {
    temporary <- FALSE
  }
  storage_control <- getDBOption(db, "create_options", "")
  sqlc <- paste("CREATE",
                ifelse(temporary, "TEMPORARY ", ""),
                "TABLE",
                quote_identifier(db, table_name),
                storage_control,
                "AS",
                sql)
  sqlc
}

#' Materialize an optree as a table.
#'
#' Run the data query as a CREATE TABLE AS . Think of as a function
#' that can be applied to relop trees, not as a component to place
#' in pipelines.
#'
#' @param db database connecton handle.
#' @param optree relop operation tree.
#' @param table_name character, name of table to create.
#' @param ... force later arguments to bind by name.
#' @param limit numeric if not NULL result limit (to use this, last statement must not have a limit).
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param overwrite logical if TRUE drop an previous table.
#' @param temporary logical if TRUE try to create a temporary table.
#' @param precheck logical if TRUE precheck existence of table and columns.
#' @param sql character, pre-rendered SQL matching optree and options- should not be set by user code.
#' @return table description
#'
#' @seealso \code{\link{db_td}}, \code{\link{execute}}, \code{\link{to_sql}}, \code{\link{rq_copy_to}}, \code{\link{mk_td}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2),
#'                    temporary = TRUE, overwrite = TRUE)
#'   optree <- extend_se(d, c("v" %:=% "AUC + R2", "x" %:=% "pmax(AUC,v)"))
#'   cat(format(optree))
#'   res <- materialize(my_db, optree, "example", precheck = TRUE)
#'   cat(format(res))
#'   sql <- to_sql(res, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   # extra example, table that doesn't match declared structure
#'   rq_copy_to(my_db, 'd',
#'               data.frame(z = 1:5),
#'               temporary = TRUE, overwrite = TRUE)
#'   tryCatch(
#'      materialize(my_db, optree, "example", precheck = TRUE),
#'      error = function(e) { as.character(e) }) %.>%
#'      print(.)
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @noRd
#'
materialize_impl <- function(db,
                             optree,
                             table_name = mk_tmp_name_source('rquery_mat')(),
                             ...,
                             limit = NULL,
                             source_limit = NULL,
                             overwrite = TRUE,
                             temporary = FALSE,
                             precheck = FALSE,
                             sql = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::materialize")
  if(!("relop" %in% class(optree))) {
    stop("rquery::materialize expect optree to be of class relop")
  }
  if("relop" %in% class(db)) {
    stop("rquery::materialize_impl db can not be a relop tree (should be a database handle)")
  }
  if(is.environment(db)) {
    stop("rquery::materialize_impl db can not be an environment (should be a database handle)")
  }

  if(!is.null(sql)) {
    sql_list <- sql
  } else {
    sql_list <- to_sql(optree, db,
                       limit = limit,
                       source_limit = source_limit)
  }
  # establish some safe invarients
  n_steps <- length(sql_list)
  if(n_steps<1) {
    return(NULL)
  }
  if(!is.character(sql_list[[1]])) {
    stop("rquery::materialize first step must be SQL")
  }
  if(!is.character(sql_list[[n_steps]])) {
    stop("rquery::materialize last step must be SQL")
  }
  if(n_steps>=2) {
    for(ii in seq_len(n_steps-1)) {
      if((!is.character(sql_list[[ii]]))&&
         (!is.character(sql_list[[ii+1]]))) {
        stop("rquery::materialize can not have two non-SQL sub-steps in a row")
      }
    }
  }
  # get list of temporary intermediates
  temp_intermediate_tables <- character(0)
  for(ii in seq_len(n_steps)) {
    sqli <- sql_list[[ii]]
    if(!is.character(sqli)) {
      temp_intermediate_tables <- c(temp_intermediate_tables,
                                    sqli$incoming_table_name,
                                    sqli$outgoing_table_name)
    }
  }
  temp_intermediate_tables <- sort(unique(temp_intermediate_tables))
  if(precheck) {
    # check we have all tables and columns we need to calculate
    needs <- columns_used(optree)
    for(ni in names(needs)) {
      if(!(ni %in% temp_intermediate_tables)) {
        col_needs <- needs[[ni]]
        if(!rq_table_exists(db, ni)) {
          stop(paste("rquery::materialize missing required table:", ni))
        }
        cols_have <- rq_colnames(db, ni)
        missed <- setdiff(col_needs, cols_have)
        if(length(missed)>0) {
          stop(paste("rquery::materialize table",
                     ni,
                     "missing required columns:",
                     paste(missed, collapse = ", ")))
        }
      }
    }
  }
  # check/clear final result
  if(rq_table_exists(db, table_name)) {
    if(overwrite) {
      rq_remove_table(db, table_name)
    } else {
      stop(paste("rquery::materialize result table",
                 table_name,
                 "exists, but do not have overwrite=TRUE"))
    }
  }
  # clear intermediates
  for(ti in temp_intermediate_tables) {
    rq_remove_table(db, ti)
  }
  # work on all but last node of chain
  notes <- data.frame(step = seq_len(n_steps),
                      node = NA_character_,
                      sql = NA_character_,
                      incoming_table_name = NA_character_,
                      outgoing_table_name = NA_character_,
                      start_time = Sys.time(),
                      end_time = Sys.time(),
                      stringsAsFactors = FALSE)
  notes$start_time[seq_len(n_steps)] <- NA
  notes$end_time[seq_len(n_steps)] <- NA
  to_clear <- NULL
  if(n_steps>=2) {
    # do the work on all but the last node
    for(ii in seq_len(n_steps-1)) {
      notes$start_time[[ii]] <- Sys.time()
      sqli <- sql_list[[ii]]
      if(is.character(sqli)) {
        notes$node[[ii]] <- "sql"
        notes$sql[[ii]] <- sqli
        rq_execute(db, sqli)
        if(!is.null(to_clear)) {
          rq_remove_table(db, to_clear)
          to_clear <- NULL
        }
      } else {
        if(!is.null(sqli$f)) {
          notes$node[[ii]] <- sqli$display_form
          notes$incoming_table_name[[ii]] <- sqli$incoming_table_name
          notes$outgoing_table_name[[ii]] <- sqli$outgoing_table_name
          sqli$f(db,
                 sqli$incoming_table_name,
                 sqli$outgoing_table_name)
          rq_remove_table(db, sqli$incoming_table_name)
          if((!is.null(to_clear)) &&
             (to_clear!=sqli$outgoing_table_name)) {
            rq_remove_table(db, to_clear)
          }
          to_clear <- sqli$outgoing_table_name
        } else {
          if((!is.null(to_clear)) &&
             (to_clear!=sqli$outgoing_table_name)) {
            rq_remove_table(db, to_clear)
            to_clean <- NULL
          }
          to_clear <- sqli$outgoing_table_name
        }
      }
      notes$end_time[[ii]] <- Sys.time()
    }
  }
  # work on the last node (must be SQL)
  notes$start_time[[n_steps]] <- Sys.time()
  sql <- sql_list[[n_steps]]
  notes$node[[n_steps]] <- "sql"
  notes$sql[[n_steps]] <- sql
  if(!is.null(limit)) {
    # look for limit
    # TODO: do not use string manipulation for this step
    haslimit <- grep("^.*[[:space:]]LIMIT[[:space:]]+[0-9]+[[:space:]]*$",
                     sql,
                     ignore.case = TRUE)
    if(length(haslimit)<1) {
      sql <- paste(sql, "LIMIT",
                   format(ceiling(limit), scientific = FALSE))
    }
  }
  sqlc <- materialize_sql_statement(db, sql, table_name,
                                    temporary = temporary)
  rq_execute(db, sqlc)
  if(!is.null(to_clear)) {
    rq_remove_table(db, to_clear)
    to_clear <- NULL
  }
  res <- db_td(db, table_name)
  notes$end_time[[n_steps]] <- Sys.time()
  res$notes <- notes
  res
}

#' Materialize an optree as a table.
#'
#' Run the data query as a CREATE TABLE AS . Think of as a function
#' that can be applied to relop trees, not as a component to place
#' in pipelines.
#'
#' @param db database connecton.
#' @param optree relop operation tree.
#' @param table_name character, name of table to create.
#' @param ... force later arguments to bind by name.
#' @param limit numeric if not NULL result limit (to use this, last statement must not have a limit).
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param overwrite logical if TRUE drop an previous table.
#' @param temporary logical if TRUE try to create a temporary table.
#' @param precheck logical if TRUE precheck existence of table and columns.
#' @return table description
#'
#' @seealso \code{\link{db_td}}, \code{\link{execute}}, \code{\link{to_sql}}, \code{\link{rq_copy_to}}, \code{\link{mk_td}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2),
#'                    temporary = TRUE, overwrite = TRUE)
#'   optree <- extend_se(d, c("v" %:=% "AUC + R2", "x" %:=% "pmax(AUC,v)"))
#'   cat(format(optree))
#'   res <- materialize(my_db, optree, "example", precheck = TRUE)
#'   cat(format(res))
#'   sql <- to_sql(res, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   # extra example, table that doesn't match declared structure
#'   rq_copy_to(my_db, 'd',
#'               data.frame(z = 1:5),
#'               temporary = TRUE, overwrite = TRUE)
#'   tryCatch(
#'      materialize(my_db, optree, "example", precheck = TRUE),
#'      error = function(e) { as.character(e) }) %.>%
#'      print(.)
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
materialize <- function(db,
                        optree,
                        table_name = mk_tmp_name_source('rquery_mat')(),
                        ...,
                        limit = NULL,
                        source_limit = NULL,
                        overwrite = TRUE,
                        temporary = FALSE,
                        precheck = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::materialize")
  materialize_impl(db = db,
                   optree = optree,
                   table_name = table_name,
                   limit = limit,
                   source_limit = source_limit,
                   overwrite = overwrite,
                   temporary = temporary,
                   precheck = precheck)
}



#' Materialize a user supplied SQL statement as a table.
#'
#' Run the data query with a CREATE TABLE AS .
#'
#' @param db DBI connecton.
#' @param sql character, user supplied SQL statement.
#' @param table_name character, name of table to create.
#' @param ... force later arguments to bind by name.
#' @param overwrite logical if TRUE drop an previous table.
#' @param temporary logical if TRUE try to create a temporary table.
#' @return table description
#'
#' @seealso \code{\link{db_td}}, \code{\link{materialize}}, \code{\link{to_sql}}, \code{\link{rq_copy_to}}, \code{\link{mk_td}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2),
#'                    temporary = TRUE, overwrite = TRUE)
#'   t <- materialize_sql(my_db, "SELECT AUC, R2, AUC - R2 AS d FROM d")
#'   print(t)
#'   print(execute(my_db, t))
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
materialize_sql <- function(db,
                        sql,
                        table_name = mk_tmp_name_source('rqms')(),
                        ...,
                        overwrite = TRUE,
                        temporary = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::materialize_sql")
  # check/clear final result
  if(rq_table_exists(db, table_name)) {
    if(overwrite) {
      rq_remove_table(db, table_name)
    } else {
      stop(paste("rquery::materialize_sql result table",
                 table_name,
                 "exists, but do not have overwrite=TRUE"))
    }
  }
  stmt <- materialize_sql_statement(db, sql,
                                    table_name = table_name,
                                    temporary = temporary)
  rq_execute(db, stmt)
  db_td(db, table_name)
}




#' Execute an operator tree, bringing back the result to memory.
#'
#' Run the data query.
#'
#' @param source data.frame or DBI connection.
#' @param optree relop operation tree.
#' @param ... force later arguments to bind by name.
#' @param limit numeric, if set limit to this many rows during data bring back (not used when landing a table).
#' @param source_limit numeric if not NULL limit sources to this many rows.
#' @param overwrite logical if TRUE drop an previous table.
#' @param temporary logical if TRUE try to create a temporary table.
#' @param precheck logical if TRUE precheck existence of table and columns.
#' @param allow_executor logical if TRUE allow any executor set as rquery.rquery_executor to be used.
#' @param temp_source temporary name generator.
#' @param env environment to work in.
#' @return data.frame
#'
#' @seealso \code{\link{materialize}}, \code{\link{db_td}}, \code{\link{to_sql}}, \code{\link{rq_copy_to}}, \code{\link{mk_td}}
#'
#' @examples
#'
#' # WARNING: example tries to change rquery.rquery_db_executor option to RSQLite and back.
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   old_o <- options(list("rquery.rquery_db_executor" = list(db = my_db)))
#'   d <- rq_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2))
#'   optree <- extend_se(d, c("v" %:=% "AUC + R2", "x" %:=% "pmax(AUC,v)"))
#'
#'   print(optree)
#'
#'   cat(format(optree))
#'
#'   v <- execute(my_db, optree)
#'   print(v)
#'
#'   v2 <- execute(data.frame(AUC = 1, R2 = 2), optree)
#'   print(v2)
#'
#'   options(old_o)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
execute <- function(source,
                    optree,
                    ...,
                    limit = NULL,
                    source_limit = NULL,
                    overwrite = TRUE,
                    temporary = TRUE,
                    precheck = FALSE,
                    allow_executor = TRUE,
                    temp_source = mk_tmp_name_source('rquery_ex'),
                    env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::execute")
  if(!("relop" %in% class(optree))) {
    stop("rquery::execute expect optree to be of class relop")
  }
  if("relop" %in% class(source)) {
    stop("rquery::execute source can not be a relop tree (should be a database handle, data.frame, or named list of data.frames)")
  }
  if(is.environment(source)) {
    stop("rquery::execute source can not be an environment (should be a database handle, data.frame, or named list of data.frames)")
  }
  if(is.data.frame(source) || is_named_list_of_data_frames(source)) {
    res <- rquery_apply_to_data_frame(source,
                                      optree,
                                      limit = limit,
                                      allow_executor = allow_executor,
                                      source_limit = source_limit,
                                      env = env)
    return(res)
  }
  db <- source # assume it is a DBI connection (as data.frame and DBI connections should not share a base class, and do not as of 5-11-2018)
  # fast SQL only path
  sql <- to_sql(optree, db,
                limit = limit,
                source_limit = source_limit)
  if(length(sql)==1) {
    if(precheck) {
      warning("rquery::execute ignoring precheck=TRUE on direct SQL commmand")
    }
    res <- rq_get_query(db, sql)
    return(res)
  }
  table_name <-  temp_source()
  ref <- materialize_impl(db, optree,
                          table_name = table_name,
                          limit = limit,
                          source_limit = source_limit,
                          overwrite = overwrite,
                          temporary = temporary,
                          precheck = precheck,
                          sql = sql)
  res <- ref
  # if last step is order we have to re-do that
  # as order is not well define in materialized tables
  if("relop_orderby" %in% class(optree)) {
    ref <- ref  %.>%
      orderby(.,
              cols = optree$orderby,
              reverse = optree$reverse)
  }
  sql <- to_sql(ref, db, limit = limit)
  res <- rq_get_query(db, sql)
  rq_remove_table(db, ref$table_name)
  res
}


#' Hyderdrive (science fiction show) synonym for \code{\link{execute}}
#' @inherit execute
#' @seealso \code{\link{execute}}
#' @export
commencify <- execute

