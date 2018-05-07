

#' Build a optree pipeline that selects up to the top k rows from each group in the given order.
#'
#' This is an example of building up a desired pre-prepared pipleline fragment from relop nodes.
#'
#' @param source relop tree or data.frame source.
#' @param ... force later arguments to bind by name.
#' @param partitionby partitioning (window function) column names.
#' @param orderby character, ordering (in window function) column names.
#' @param rev_orderby character, reverse ordering (in window function) column names.
#' @param k integer, number of rows to limit to in each group.
#' @param order_column character, column name to write per-group rank in (no ties).
#' @param keep_order_column logical, if TRUE retain the order column in the result.
#' @param env environment to look for values in.
#'
#' @examples
#'
#' # by hand logistic regression example
#' scale <- 0.237
#' d <- table_source("survey_table",
#'                   c("subjectID", "surveyCategory", "assessmentTotal"))
#' optree <- d %.>%
#'   extend_nse(.,
#'              probability :=
#'                exp(assessmentTotal * scale))  %.>%
#'   normalize_cols(.,
#'                  "probability",
#'                  partitionby = 'subjectID') %.>%
#'   pick_top_k(.,
#'              partitionby = 'subjectID',
#'              rev_orderby = c('probability', 'surveyCategory')) %.>%
#'   rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
#'   select_columns(., c('subjectID',
#'                       'diagnosis',
#'                       'probability')) %.>%
#'   orderby(., 'subjectID')
#' cat(format(optree))
#'
#' @export
#'
pick_top_k <- function(source,
                       ...,
                       partitionby = NULL,
                       orderby = NULL,
                       rev_orderby = NULL,
                       k = 1L,
                       order_column = "row_number",
                       keep_order_column = TRUE,
                       env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::pick_top_k")
  pipe <- source %.>%
    extend_se(.,
              assignments = order_column := "row_number()",
              partitionby = partitionby,
              orderby = orderby,
              rev_orderby = rev_orderby,
              env = env) %.>%
    select_rows_se(., paste(order_column, "<=", k),
                   env = env)
  if(!keep_order_column) {
    pipe <- pipe %.>%
      drop_columns(.,
                   drops = order_column,
                   strict = TRUE,
                   env = env)
  }
  pipe
}