
# make . not look unbound
. <- NULL

#' @importFrom stats sd
NULL

#' Apply summary function to many columns.
#'
#' @param db database handle.
#' @param tableName name of table.
#' @param lexpr left portion of expression
#' @param cols character, columns to track
#' @param rexpr left portion of expression
#' @param ... force later arguments to be by name
#' @param skipNulls if TRUE skip NULLs
#' @return counts
#'
#' @noRd
#'
summarize_columns <- function(db, tableName,
                              lexpr, cols, rexpr,
                              ...,
                              skipNulls = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery:::summarize_columns")
  nc <- length(cols)
  if(nc<1) {
    stop("rquery:::summarize_columns need at least one column name")
  }
  terms <- vapply(cols,
                  function(ci) {
                    paste0(lexpr,
                           " ",
                           quote_identifier(db, ci),
                           " ",
                           rexpr,
                           " AS ",
                           quote_identifier(db, ci))
                  }, character(1))
  q <- paste0("SELECT ",
              paste(terms, collapse = ", "),
              " FROM ",
              quote_identifier(db, tableName))
  rq_get_query(db, q)
}


#' Compute usable summary of columns of remote table.
#'
#' Compute per-column summaries and return as a \code{data.frame}.  Warning: can be an expensive operation.
#'
#' For numeric columns includes \code{NaN} in \code{nna} count (as is typical for \code{R}, e.g.,
#' \code{is.na(NaN)}).
#'
#' @param db database connection.
#' @param tableName name of table.
#' @param ... force additional arguments to be bound by name.
#' @param countUniqueNum logical, if TRUE include unique non-NA counts for numeric cols.
#' @param quartiles logical, if TRUE add Q1 (25\%), median (50\%), Q3 (75\%) quartiles.
#' @param cols if not NULL set of columns to restrict to.
#' @return data.frame summary of columns.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'   requireNamespace("RSQLite", quietly = TRUE)) {
#'   d <- data.frame(p= c(TRUE, FALSE, NA),
#'                   s= NA,
#'                   w= 1:3,
#'                   x= c(NA,2,3),
#'                   y= factor(c(3,5,NA)),
#'                   z= c('a',NA,'a'),
#'                   stringsAsFactors=FALSE)
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   RSQLite::initExtension(db)
#'   rq_copy_to(db, "dRemote", d,
#'               overwrite = TRUE, temporary = TRUE)
#'   print(rsummary(db, "dRemote"))
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#'
rsummary <- function(db,
                     tableName,
                     ...,
                     countUniqueNum = FALSE,
                     quartiles = FALSE,
                     cols = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::rsummary")
  localSample <- rq_coltypes(db, tableName)
  cnames <- colnames(localSample)
  if(!is.null(cols)) {
    cnames <- intersect(cnames, cols)
    localSample <- localSample[, cnames, drop=FALSE]
  }
  nrows <- 0
  if(nrow(localSample)>0) {
    nrows <- rq_nrow(db, tableName)
  }
  cmap <- seq_len(length(cnames))
  names(cmap) <- cnames
  numericCols <- cnames[vapply(localSample, is.numeric, logical(1))]
  logicalCols <- cnames[vapply(localSample, is.logical, logical(1))]
  charCols <- cnames[vapply(localSample,
                            function(ci) { is.character(ci) || is.factor(ci) },
                            logical(1))]
  workingCols <- c(numericCols, logicalCols, charCols)
  exoticCols <- setdiff(cnames, workingCols)
  cclass <- lapply(localSample, class)
  names(cclass) <- colnames(localSample)
  res <- data.frame(column = cnames,
                    index = NA_real_,
                    class = NA_character_,
                    nrows = nrows,
                    nna = NA_real_,
                    nunique = NA_real_,
                    min = NA_real_,
                    max = NA_real_,
                    mean = NA_real_,
                    sd = NA_real_,
                    lexmin = NA_real_,
                    lexmax = NA_real_,
                    stringsAsFactors = FALSE)
  classtr <- vapply(cclass,function(vi) {
    paste(vi,collapse=', ')
  }, character(1))
  res$class <- classtr[res$column]
  res$index <- match(res$column, cnames)
  if((nrows<1)||(length(workingCols)<1)) {
    return(res)
  }
  populate_column <- function(res, colname, z) {
    z <- z %.>%
      t(.) %.>%
      as.data.frame(.)
    idxs <- match(rownames(z), res$column)
    res[[colname]][idxs] <- z[[1]]
    res
  }
  null_stats <- summarize_columns(db, tableName,
                                  "SUM( CASE WHEN (",
                                  workingCols,
                                  "IS NULL ) THEN 1.0 ELSE 0.0 END )")
  res <- populate_column(res, "nna", null_stats)
  # limit down to populated columns
  unpop_cols <- res$column[res$nna>=res$nrows]
  res$nunique[res$column %in% unpop_cols] <- 0.0
  numericCols <- setdiff(numericCols, unpop_cols)
  logicalCols <- setdiff(logicalCols, unpop_cols)
  charCols <-  setdiff(charCols, unpop_cols)
  if(length(numericCols)>=1) {
    res <- summarize_columns(db, tableName,
                             "MAX(",
                             numericCols,
                             ")",
                             skipNulls = TRUE) %.>%
      populate_column(res, "max", .)
    res <- summarize_columns(db, tableName,
                             "MIN(",
                             numericCols,
                             ")",
                             skipNulls = TRUE) %.>%
      populate_column(res, "min", .)
    res <- summarize_columns(db, tableName,
                             "AVG(",
                             numericCols,
                             ")",
                             skipNulls = TRUE) %.>%
      populate_column(res, "mean", .)
    for(ci in numericCols) {
      idx <- which(ci==res$column)[[1]]
      ngood <- res$nrows[[idx]] - res$nna[[idx]]
      if(ngood>=2) {
        mv <- format(res$mean[[idx]])
        qdev <- paste0("SELECT SUM((",
                       quote_identifier(db, ci),
                       " - ",
                       mv,
                       ")*(",
                       quote_identifier(db, ci),
                       " - ",
                       mv,
                       ")) AS ",
                       quote_identifier(db, ci),
                       " FROM ",
                       quote_identifier(db, tableName),
                       " WHERE ",
                       quote_identifier(db, ci),
                       " IS NOT NULL")
        vdev <- as.numeric(rq_get_query(db, qdev)[[1]][[1]])
        res$sd[[idx]] <- sqrt(vdev/(ngood-1.0))
      }
    }
    if(countUniqueNum) {
      for(ci in numericCols) {
        idx <- which(ci==res$column)[[1]]
        if(res$nrows[[idx]]>res$nna[[idx]]) {
          qcount <- paste0("SELECT COUNT(1) FROM ( SELECT ",
                           quote_identifier(db, ci),
                           " FROM ",
                           quote_identifier(db, tableName),
                           " WHERE ",
                           quote_identifier(db, ci),
                           " IS NOT NULL GROUP BY ",
                           quote_identifier(db, ci),
                           " ) TMPTAB ")
          vcount <- as.numeric(rq_get_query(db, qcount)[[1]][[1]])
          res$nunique[[idx]] <- vcount
        }
      }
    }
  }
  if(length(logicalCols)>=1) {
    res <- summarize_columns(db, tableName,
                             "MIN(CASE WHEN",
                             logicalCols,
                             "THEN 1.0 ELSE 0.0 END)",
                             skipNulls = TRUE) %.>%
      populate_column(res, "min", .)
    res <- summarize_columns(db, tableName,
                             "MAX(CASE WHEN",
                             logicalCols,
                             "THEN 1.0 ELSE 0.0 END)",
                             skipNulls = TRUE) %.>%
      populate_column(res, "max", .)
    res <- summarize_columns(db, tableName,
                             "AVG(CASE WHEN",
                             logicalCols,
                             "THEN 1.0 ELSE 0.0 END)",
                             skipNulls = TRUE) %.>%
      populate_column(res, "mean", .)
    for(ci in logicalCols) {
      # sample standard deviation!
      idx <- which(ci==res$column)[[1]]
      res$nunique[[idx]] <- 0.0
      ngood <- res$nrows[[idx]] - res$nna[[idx]]
      if(ngood>=1) {
        res$nunique[[idx]] <- 1.0
        res$lexmin[[idx]] <- c("FALSE", "TRUE")[[res$min[[idx]]+1]]
        res$lexmax[[idx]] <- c("FALSE", "TRUE")[[res$max[[idx]]+1]]
      }
      if(ngood>=2) {
        res$nunique[[idx]] <- 2.0
        if(res$min[[idx]]>=res$max[[idx]]) {
          res$sd[[idx]] <- 0.0
        } else {
          nTrue <- res$mean[[idx]]*ngood
          nFalse <- (1-res$mean[[idx]])*ngood
          res$sd[[idx]] <- sqrt(sum(nTrue*(1.0-res$mean[[idx]])*(1.0-res$mean[[idx]]) +
                                      nFalse*(0.0-res$mean[[idx]])*(0.0-res$mean[[idx]]))/
                                  (ngood-1))
        }
      }
    }
  }
  if(length(charCols)>=1) {
    for(ci in charCols) {
      idx <- which(ci==res$column)[[1]]
      if(res$nrows[[idx]]>res$nna[[idx]]) {
        qmin <- paste0("SELECT ",
                       quote_identifier(db, ci),
                       " FROM ",
                       quote_identifier(db, tableName),
                       " WHERE ",
                       quote_identifier(db, ci),
                       " IS NOT NULL ORDER BY ",
                       quote_identifier(db, ci),
                       " LIMIT 1")
        vmin <- rq_get_query(db, qmin)[[1]][[1]]
        qmax <- paste0("SELECT ",
                       quote_identifier(db, ci),
                       " FROM ",
                       quote_identifier(db, tableName),
                       " WHERE ",
                       quote_identifier(db, ci),
                       " IS NOT NULL ORDER BY ",
                       quote_identifier(db, ci),
                       " DESC LIMIT 1")
        vmax <- rq_get_query(db, qmax)[[1]][[1]]
        qcount <- paste0("SELECT COUNT(1) FROM ( SELECT ",
                         quote_identifier(db, ci),
                         " FROM ",
                         quote_identifier(db, tableName),
                         " WHERE ",
                         quote_identifier(db, ci),
                         " IS NOT NULL GROUP BY ",
                         quote_identifier(db, ci),
                         " ) TMPTAB ")
        vcount <- as.numeric(rq_get_query(db, qcount)[[1]][[1]])
        res$lexmin[[idx]] <- vmin
        res$lexmax[[idx]] <- vmax
        res$nunique[[idx]] <- vcount
      }
    }
  }
  res <- res[order(res$index),]
  rownames(res) <- NULL
  if(quartiles) {
    qs <- quantile_cols(db, tableName,
                        probs = c(0.25, 0.5, 0.75),
                        probs_name = "rquery_probs_col",
                        cols = numericCols)
    res$Q1 <- NA_real_
    res$median <- NA_real_
    res$Q3 <- NA_real_
    for(ci in numericCols) {
      idx <- which(res$column == ci)[[1]]
      res$Q1[[idx]] <- qs[[ci]][[1]]
      res$median[[idx]] <- qs[[ci]][[2]]
      res$Q3[[idx]] <- qs[[ci]][[3]]
    }
  }
  res
}

# d <- data.frame(p= c(TRUE, FALSE, NA),
#                 s= NA,
#                 w= 1:3,
#                 x= c(NA,2,3),
#                 y= factor(c(3,5,NA)),
#                 z= c('a',NA,'a'),
#                 stringsAsFactors=FALSE)
# rquery:::rsummary_d(d)
#
rsummary_d <- function(d,
                       ...,
                       countUniqueNum = TRUE,
                       quartiles = TRUE,
                       cols = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::rsummary_d")
  if(!is.data.frame(d)) {
    stop("rquery::rsummary_d d is supposed to be a data.frame")
  }
  cnames <- colnames(d)
  if(!is.null(cols)) {
    cnames <- intersect(cnames, cols)
    d <- d[, cnames, drop=FALSE]
  }
  nrows <- nrow(d)
  cmap <- seq_len(length(cnames))
  names(cmap) <- cnames
  numericCols <- c(cnames[vapply(d, is.numeric, logical(1))],
                   cnames[vapply(d, is.logical, logical(1))])
  charCols <- c(cnames[vapply(d, is.character, logical(1))],
                cnames[vapply(d, is.factor, logical(1))])
  workingCols <- c(numericCols, charCols)
  exoticCols <- setdiff(cnames, workingCols)
  cclass <- lapply(d, class)
  names(cclass) <- colnames(d)
  res <- data.frame(column = cnames,
                    index = NA_real_,
                    class = NA_character_,
                    nrows = nrows,
                    nna = NA_real_,
                    nunique = NA_real_,
                    min = NA_real_,
                    max = NA_real_,
                    mean = NA_real_,
                    sd = NA_real_,
                    lexmin = NA_real_,
                    lexmax = NA_real_,
                    stringsAsFactors = FALSE)
  classtr <- vapply(cclass,function(vi) {
    paste(vi,collapse=', ')
  }, character(1))
  res$class <- classtr[res$column]
  res$index <- match(res$column, cnames)
  if((nrows<1)||(length(workingCols)<1)) {
    return(res)
  }
  populate_column <- function(res, colname, z) {
    idxs <- match(names(z), res$column)
    res[[colname]][idxs] <- z
    res
  }
  null_stats <- vapply(workingCols,
                       function(ci) {
                         sum(is.na(d[[ci]]))
                       },
                       numeric(1))
  res <- populate_column(res, "nna", null_stats)
  # limit down to populated columns
  unpop_cols <- res$column[res$nna>=res$nrows]
  res$nunique[res$column %in% unpop_cols] <- 0.0
  numericCols <- setdiff(numericCols, unpop_cols)
  charCols <-  setdiff(charCols, unpop_cols)
  if(length(numericCols)>=1) {
    res <- vapply(numericCols,
                  function(ci) {
                    max(as.numeric(d[[ci]]), na.rm = TRUE)
                  }, numeric(1)) %.>%
      populate_column(res, "max", .)
    res <- vapply(numericCols,
                  function(ci) {
                    min(as.numeric(d[[ci]]), na.rm = TRUE)
                  }, numeric(1)) %.>%
      populate_column(res, "min", .)
    res <- vapply(numericCols,
                  function(ci) {
                    mean(as.numeric(d[[ci]]), na.rm = TRUE)
                  }, numeric(1)) %.>%
      populate_column(res, "mean", .)
    res <- vapply(numericCols,
                  function(ci) {
                    stats::sd(as.numeric(d[[ci]]), na.rm = TRUE)
                  }, numeric(1)) %.>%
      populate_column(res, "sd", .)
    if(countUniqueNum) {
      res <- vapply(numericCols,
                    function(ci) {
                      length(unique(d[[ci]]))
                    }, numeric(1)) %.>%
        populate_column(res, "nunique", .)
    }
  }
  if(length(charCols)>=1) {
    res <- vapply(charCols,
                  function(ci) {
                    max(as.character(d[[ci]]), na.rm = TRUE)
                  }, character(1)) %.>%
      populate_column(res, "lexmax", .)
    res <- vapply(charCols,
                  function(ci) {
                    min(as.character(d[[ci]]), na.rm = TRUE)
                  }, character(1)) %.>%
      populate_column(res, "lexmin", .)
    res <- vapply(charCols,
                  function(ci) {
                    length(unique(as.character(d[[ci]])))
                  }, numeric(1)) %.>%
      populate_column(res, "nunique", .)
  }
  res <- res[order(res$index),]
  rownames(res) <- NULL
  if(quartiles) {
    qs <- quantile_cols_d(d,
                          probs = c(0.25, 0.5, 0.75),
                          probs_name = "rquery_probs_col",
                          cols = numericCols)
    res$Q1 <- NA_real_
    res$median <- NA_real_
    res$Q3 <- NA_real_
    for(ci in numericCols) {
      idx <- which(res$column == ci)[[1]]
      res$Q1[[idx]] <- qs[[ci]][[1]]
      res$median[[idx]] <- qs[[ci]][[2]]
      res$Q3[[idx]] <- qs[[ci]][[3]]
    }
  }
  res
}


#' Create an rsumary relop operator node.
#'
#' This is a non_sql_node, so forces the materialization of
#' the calculation prior to it losing narrowing optimizations.
#'
#' @param source incoming source (relop node or data.frame).
#' @param ... force later arguments to be by name
#' @param quartiles logical, if TRUE add Q1 (25\%), median (50\%), Q3 (75\%) quartiles.
#' @param tmp_name_source wrapr::mk_tmp_name_source(), temporary name generator.
#' @param temporary logical, if TRUE use temporary tables
#' @return rsummary node
#'
#' @seealso \code{\link{quantile_node}}, \code{\link{non_sql_node}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   d <- data.frame(p= c(TRUE, FALSE, NA),
#'                   s= NA,
#'                   w= 1:3,
#'                   x= c(NA,2,3),
#'                   y= factor(c(3,5,NA)),
#'                   z= c('a',NA,'a'),
#'                   stringsAsFactors=FALSE)
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   RSQLite::initExtension(db)
#'   rq_copy_to(db, "dRemote", d,
#'               overwrite = TRUE,
#'               temporary = TRUE)
#'
#'   ops <- db_td(db, "dRemote") %.>%
#'     extend(., v %:=% ifelse(x>2, "x", "y")) %.>%
#'     rsummary_node(.)
#'   cat(format(ops))
#'
#'   print(to_sql(ops, db))
#'
#'   reshdl <- materialize(db, ops)
#'   print(DBI::dbGetQuery(db, to_sql(reshdl, db)))
#'
#'   DBI::dbDisconnect(db)
#' }
#'
#' @export
#'
rsummary_node <- function(source,
                          ...,
                          quartiles = FALSE,
                          tmp_name_source = wrapr::mk_tmp_name_source("sn"),
                          temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::rsummary_node")
  if(is.data.frame(source)) {
    tmp_name <- mk_tmp_name_source("rquery_tmp")()
    dnode <- mk_td(tmp_name, colnames(source))
    source <- dnode
  }
  columns_produced <- c("column",
                        "index",
                        "class",
                        "nrows",
                        "nna",
                        "nunique",
                        "min",
                        "max",
                        "mean",
                        "sd",
                        "lexmin",
                        "lexmax")
  if(quartiles) {
    columns_produced <- c(columns_produced,
                          c("Q1", "median", "Q3"))
  }
  force(temporary)
  force(quartiles)
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name,
                   nd = NULL) {
    stable <- rsummary(db, incoming_table_name,
                       quartiles = quartiles)
    rq_copy_to(db,
               table_name = outgoing_table_name,
               d = stable,
               overwrite = TRUE,
               temporary = temporary)
  }
  f_df <- function(d, nd = NULL) {
    rsummary_d(d,
               quartiles = quartiles)
  }
  nd <- non_sql_node(source,
                     f_db = f_db,
                     f_df = f_df,
                     f_dt = NULL,
                     incoming_table_name = incoming_table_name,
                     outgoing_table_name = outgoing_table_name,
                     columns_produced = columns_produced,
                     display_form = paste0("rsummary_node(.)"),
                     orig_columns = FALSE,
                     temporary = temporary)
  nd
}


