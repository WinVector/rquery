
#' Use one column to pick values from other columns.
#'
#' The \code{pick} column selects values from the columns it names (per-row).
#'
#' @param source source to select from (relop or data.frame).
#' @param pick character scalar, name of column to control value choices.
#' @param result character scalar, name of column to place values in.
#' @param ... force later arguments to be bound by name
#' @param tmp_name_source wrapr::mk_tmp_name_source(), temporary name generator.
#' @param temporary logical, if TRUE use temporary tables
#' @return derived column result
#'
#' @examples
#'
#' df = data.frame(x = c(1, 2, 3, 4),
#'                 y = c(5, 6, 7, 8),
#'                 choice = c("x", "y", "x", "z"),
#'                 stringsAsFactors = FALSE)
#'
#' # library("rqdatatable")
#' # df %.>%
#' #   column_pick(., "choice", "derived")
#'
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   db <- DBI::dbConnect(RSQLite::SQLite(),
#'                        ":memory:")
#'   RSQLite::initExtension(db)
#'   dr <- rq_copy_to(db, "dRemote", df,
#'                    overwrite = TRUE,
#'                    temporary = TRUE)
#'
#'   ops <- dr %.>%
#'     column_pick(., "choice", "derived")
#'   cat(format(ops))
#'
#'   execute(db, ops) %.>%
#'      print(.)
#'
#'   DBI::dbDisconnect(db)
#' }
#'
#'
#' @export
#'
column_pick <- function(source,
                        pick,
                        result,
                        ...,
                        tmp_name_source = wrapr::mk_tmp_name_source("qn"),
                        temporary = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)), "rquery::column_pick.relop")
  src_cols <- column_names(source)
  if((!is.character(pick)) || (length(pick)!=1)) {
    stop("rquery::column_pick pick must a string")
  }
  if(!(pick %in% src_cols)) {
    stop("rquery::column_pick pick must be a source column")
  }
  if((!is.character(result)) || (length(result)!=1)) {
    stop("rquery::column_pick result must a string")
  }
  if(result %in% src_cols) {
    stop("rquery::column_pick result must not be a source column")
  }
  force(temporary)
  incoming_table_name = tmp_name_source()
  outgoing_table_name = tmp_name_source()
  f_db <- function(db,
                   incoming_table_name,
                   outgoing_table_name) {
    # get list of possible values
    q <- paste0("
     SELECT
        ", quote_identifier(db, pick), "
      FROM
        ", quote_identifier(db, incoming_table_name), "
     GROUP BY
        ", quote_identifier(db, pick))
    r <- rq_get_query(db, q)[[1]]
    r <- intersect(r, src_cols)
    # build query
    wterms <- vapply(r,
                     function(ri) {
                      paste0("   WHEN ",
                             quote_identifier(db, pick),
                             " = ",
                             quote_string(db, ri),
                             " THEN ",
                             quote_identifier(db, ri))
                     }, character(1))
    wterms <- paste(wterms, collapse = "\n      ")
    oterms <- vapply(src_cols,
                     function(si) {
                              quote_identifier(db, si)
                       }, character(1))
    oterms <- paste(oterms, collapse = ",\n        ")
    q <- paste0("
     SELECT
        ", oterms, ",
        CASE
         ", wterms, "
         ELSE NULL END
         AS ", quote_identifier(db, result), "
     FROM
        ", quote_identifier(db, incoming_table_name))
    qm <- materialize_sql_statement(db, q,
                                    table_name = outgoing_table_name,
                                    temporary = temporary)
    rq_execute(db, qm)
    db_td(db, outgoing_table_name)
  }
  f_df <- function(d) {
    d <- as.data.frame(d)
    dtmp <- d[,
              intersect(colnames(d), unique(d[[pick]])),
              drop = FALSE]
    d[[result]] <-
      dtmp[cbind(
        seq_len(nrow(dtmp)),
        match(d[[pick]], colnames(dtmp))
      )]
    d
  }
  nd <- non_sql_node(source,
                     f_db = f_db,
                     f_df = f_df,
                     incoming_table_name = incoming_table_name,
                     outgoing_table_name = outgoing_table_name,
                     columns_produced = result,
                     display_form = paste0("column_pick(.; ",
                                           pick,
                                           ", ", result,
                                           ")"),
                     orig_columns = TRUE,
                     temporary = temporary)
  nd
}
