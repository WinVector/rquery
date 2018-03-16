
#' Make a rename columns node (not a relational operation).
#'
#' @param source source to rename from.
#' @param cmap map written as new column names as keys and old column names as values.
#' @return rename columns node.
#'
#' @examples
#'
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- dbi_copy_to(my_db, 'd',
#'                    data.frame(AUC = 0.6, R2 = 0.2, z = 3))
#'   eqn <- rename_columns(d, c('AUC2' := 'AUC', 'R' := 'R2'))
#'   cat(format(eqn))
#'   sql <- to_sql(eqn, my_db)
#'   cat(sql)
#'   print(DBI::dbGetQuery(my_db, sql))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
rename_columns <- function(source, cmap) {
  UseMethod("rename_columns", source)
}

#' @export
rename_columns.relop <- function(source, cmap) {
  if(length(cmap)<=0) {
    stop("rquery::rename_columns must rename at least 1 column")
  }
  if(length(cmap)!=length(unique(as.character(cmap)))) {
    stop("rquery::rename_columns map values must be unique")
  }
  if(length(cmap)!=length(unique(names(cmap)))) {
    stop("rquery::rename_columns map keys must be unique")
  }
  have <- column_names(source)
  check_have_cols(have, as.character(cmap), "rquery::rename_columns cmap")
  collisions <- intersect(names(cmap), have)
  if(length(collisions)>0) {
    stop(paste("rquery::rename_columns rename collisions",
               paste(collisions, collapse = ", ")))
  }
  r <- list(source = list(source),
            table_name = NULL,
            parsed = NULL,
            cmap = cmap)
  r <- relop_decorate("relop_rename_columns", r)
  r
}

#' @export
rename_columns.data.frame <- function(source, cmap) {
  if(length(cmap)<=0) {
    stop("rquery::rename_columns must rename at least 1 column")
  }
  if(length(cmap)!=length(unique(as.character(cmap)))) {
    stop("rquery::rename_columns map values must be unique")
  }
  if(length(cmap)!=length(unique(names(cmap)))) {
    stop("rquery::rename_columns map keys must be unique")
  }
  tmp_name <- mk_tmp_name_source("rquery_tmp")()
  dnode <- table_source(tmp_name, colnames(source))
  dnode$data <- source
  enode <- rename_columns(dnode, cmap)
  return(enode)
}


#' @export
column_names.relop_rename_columns <- function (x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  sc <- column_names(x$source[[1]])
  rmap <- names(x$cmap)
  names(rmap) <- as.character(x$cmap)
  sc[sc %in% names(rmap)] <- rmap[sc[sc %in% names(rmap)]]
  sc
}



#' @export
format.relop_rename_columns <- function(x, ...) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  paste0(trimws(format(x$source[[1]]), which = "right"),
         " %.>%\n ",
         "rename(.,\n",
         "  ", gsub("\n", "\n  ",
                    wrapr::map_to_char(x$cmap, sep = "\n  "),
                    fixed = TRUE), ")",
         "\n")
}


calc_used_relop_rename_columns <- function (x, ...,
                                            using = NULL,
                                            contract = FALSE) {
  cols <- column_names(x)
  if(length(using)>0) {
    missing <- setdiff(using, cols)
    if(length(missing)>0) {
      stop(paste("rquery::calc_used_relop_rename_columns unknown columns",
                 paste(missing, collapse = ", ")))
    }
    cols <- intersect(cols, using)
  }
  # map back prior to rename
  rmap <- x$cmap
  sc <- cols
  sc[sc %in% names(rmap)] <- rmap[sc[sc %in% names(rmap)]]
  names(sc) <- cols
  sc
}

#' @export
columns_used.relop_rename_columns <- function (x, ...,
                                               using = NULL,
                                               contract = FALSE) {
  qmap <- calc_used_relop_rename_columns(x, using=using, contract=contract)
  return(columns_used(x$source[[1]],
                      using = as.character(qmap),
                      contract = contract))
}


#' @export
to_sql.relop_rename_columns <- function (x,
                                         db,
                                         ...,
                                         source_limit = NULL,
                                         indent_level = 0,
                                         tnum = mk_tmp_name_source('tsql'),
                                         append_cr = TRUE,
                                         using = NULL) {
  if(length(list(...))>0) {
    stop("unexpected arguments")
  }
  qmap <- calc_used_relop_rename_columns(x, using=using)
  colsV <- vapply(as.character(qmap),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  colsA <- vapply(names(qmap),
                  function(ci) {
                    quote_identifier(db, ci)
                  }, character(1))
  cols <- paste(colsV, "AS", colsA)
  subsql_list <- to_sql(x$source[[1]],
                        db = db,
                        source_limit = source_limit,
                        indent_level = indent_level + 1,
                        tnum = tnum,
                        append_cr = FALSE,
                        using = as.character(qmap))
  subsql <- subsql_list[[length(subsql_list)]]
  tab <- tnum()
  prefix <- paste(rep(' ', indent_level), collapse = '')
  q <- paste0(prefix, "SELECT\n",
              prefix, " ", paste(cols, collapse = paste0(",\n", prefix, " ")), "\n",
              prefix, "FROM (\n",
              subsql, "\n",
              prefix, ") ",
              tab)
  if(append_cr) {
    q <- paste0(q, "\n")
  }
  c(subsql_list[-length(subsql_list)], q)
}



