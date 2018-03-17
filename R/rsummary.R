
# make . not look unbound
. <- NULL


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
                           DBI::dbQuoteIdentifier(db, ci),
                           " ",
                           rexpr,
                           " AS ",
                           DBI::dbQuoteIdentifier(db, ci))
                  }, character(1))
  q <- paste0("SELECT ",
              paste(terms, collapse = ", "),
              " FROM ",
              DBI::dbQuoteIdentifier(db, tableName))
  DBI::dbGetQuery(db, q)
}


#' Compute usable summary of columns of remote table.
#'
#' Compute per-column summaries and return as a \code{data.frame}.  Warning: can be an expensive operation.
#'
#' For numeric columns includes \code{NaN} in \code{nna} count (as is typcial for \code{R}, e.g.,
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
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   d <- data.frame(p= c(TRUE, FALSE, NA),
#'                   s= NA,
#'                   w= 1:3,
#'                   x= c(NA,2,3),
#'                   y= factor(c(3,5,NA)),
#'                   z= c('a',NA,'a'),
#'                   stringsAsFactors=FALSE)
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   RSQLite::initExtension(db)
#'   dbi_copy_to(db, "dRemote", d,
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
  localSample <- DBI::dbGetQuery(db, paste0("SELECT * FROM ",
                                        DBI::dbQuoteIdentifier(db,
                                                               tableName),
                                        " LIMIT 1"))
  cnames <- colnames(localSample)
  if(!is.null(cols)) {
    cnames <- intersect(cnames, cols)
    localSample <- localSample[, cnames, drop=FALSE]
  }
  nrows <- 0
  if(nrow(localSample)>0) {
    nrows <- dbi_nrow(db, tableName)
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
                       DBI::dbQuoteIdentifier(db, ci),
                       " - ",
                       mv,
                       ")*(",
                       DBI::dbQuoteIdentifier(db, ci),
                       " - ",
                       mv,
                       ")) AS ",
                       DBI::dbQuoteIdentifier(db, ci),
                       " FROM ",
                       DBI::dbQuoteIdentifier(db, tableName),
                       " WHERE ",
                       DBI::dbQuoteIdentifier(db, ci),
                       " IS NOT NULL")
        vdev <- as.numeric(DBI::dbGetQuery(db, qdev)[[1]][[1]])
        res$sd[[idx]] <- sqrt(vdev/(ngood-1.0))
      }
    }
    if(countUniqueNum) {
      for(ci in numericCols) {
        idx <- which(ci==res$column)[[1]]
        if(res$nrows[[idx]]>res$nna[[idx]]) {
          qcount <- paste0("SELECT COUNT(1) FROM ( SELECT ",
                           DBI::dbQuoteIdentifier(db, ci),
                           " FROM ",
                           DBI::dbQuoteIdentifier(db, tableName),
                           " WHERE ",
                           DBI::dbQuoteIdentifier(db, ci),
                           " IS NOT NULL GROUP BY ",
                           DBI::dbQuoteIdentifier(db, ci),
                           " ) TMPTAB ")
          vcount <- as.numeric(DBI::dbGetQuery(db, qcount)[[1]][[1]])
          res$nunique[[idx]] <- vcount
        }
      }
    }
  }
  if(length(logicalCols)>=1) {
    res <- summarize_columns(db, tableName,
                             "MIN(CASE",
                             logicalCols,
                             "THEN 1.0 ELSE 0.0 END)",
                             skipNulls = TRUE) %.>%
      populate_column(res, "min", .)
    res <- summarize_columns(db, tableName,
                            "MAX(CASE",
                            logicalCols,
                            "THEN 1.0 ELSE 0.0 END)",
                            skipNulls = TRUE) %.>%
      populate_column(res, "max", .)
    res <- summarize_columns(db, tableName,
                             "AVG(CASE",
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
                       DBI::dbQuoteIdentifier(db, ci),
                       " FROM ",
                       DBI::dbQuoteIdentifier(db, tableName),
                       " WHERE ",
                       DBI::dbQuoteIdentifier(db, ci),
                       " IS NOT NULL ORDER BY ",
                       DBI::dbQuoteIdentifier(db, ci),
                       " LIMIT 1")
        vmin <- DBI::dbGetQuery(db, qmin)[[1]][[1]]
        qmax <- paste0("SELECT ",
                       DBI::dbQuoteIdentifier(db, ci),
                       " FROM ",
                       DBI::dbQuoteIdentifier(db, tableName),
                       " WHERE ",
                       DBI::dbQuoteIdentifier(db, ci),
                       " IS NOT NULL ORDER BY ",
                       DBI::dbQuoteIdentifier(db, ci),
                       " DESC LIMIT 1")
        vmax <- DBI::dbGetQuery(db, qmax)[[1]][[1]]
        qcount <- paste0("SELECT COUNT(1) FROM ( SELECT ",
                         DBI::dbQuoteIdentifier(db, ci),
                         " FROM ",
                         DBI::dbQuoteIdentifier(db, tableName),
                         " WHERE ",
                         DBI::dbQuoteIdentifier(db, ci),
                         " IS NOT NULL GROUP BY ",
                         DBI::dbQuoteIdentifier(db, ci),
                         " ) TMPTAB ")
        vcount <- as.numeric(DBI::dbGetQuery(db, qcount)[[1]][[1]])
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
                        c(0.25, 0.5, 0.75), "rquery_probs_col",
                        numericCols)
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
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   d <- data.frame(p= c(TRUE, FALSE, NA),
#'                   s= NA,
#'                   w= 1:3,
#'                   x= c(NA,2,3),
#'                   y= factor(c(3,5,NA)),
#'                   z= c('a',NA,'a'),
#'                   stringsAsFactors=FALSE)
#'   db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   RSQLite::initExtension(db)
#'   dbi_copy_to(db, "dRemote", d,
#'               overwrite = TRUE,
#'               temporary = TRUE)
#'
#'   ops <- dbi_table(db, "dRemote") %.>%
#'     extend_nse(., v := ifelse(x>2, "x", "y")) %.>%
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
    dnode <- table_source(tmp_name, colnames(source))
    dnode$data <- source
    source <- dnode
  }
  columns_used <- column_names(source)
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
  f <- function(db,
                incoming_table_name,
                outgoing_table_name) {
    stable <- rsummary(db, incoming_table_name,
                       quartiles = quartiles)
    dbi_copy_to(db,
                table_name = outgoing_table_name,
                d = stable,
                overwrite = TRUE,
                temporary = temporary)
  }
  nd <- non_sql_node(source,
                     f,
                     incoming_table_name = incoming_table_name,
                     columns_used = columns_used,
                     outgoing_table_name = outgoing_table_name,
                     columns_produced = columns_produced,
                     display_form = paste0("rsummary_node(., ",
                                           incoming_table_name,
                                           ", ",
                                           outgoing_table_name,
                                           ")"),
                     orig_columns = FALSE,
                     temporary = temporary)
  nd
}


