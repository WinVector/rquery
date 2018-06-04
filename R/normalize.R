
#' Build a optree pipeline that normalizes a set of columns so each column sums to one in each partition.
#'
#' This is an example of building up a desired pre-prepared pipleline fragment from relop nodes.
#'
#' @param source relop tree or data.frame source.
#' @param columns character, columns to normalize.
#' @param ... force later arguments to bind by name.
#' @param partitionby partitioning (window function) column names to define partitions.
#' @param env environment to look for values in.
#'
#' @examples
#'
#' # by hand logistic regression example
#' scale <- 0.237
#' d <- mk_td("survey_table",
#'                   c("subjectID", "surveyCategory", "assessmentTotal"))
#' optree <- d %.>%
#'   extend_nse(.,
#'              probability %:=%
#'                exp(assessmentTotal * scale))  %.>%
#'   normalize_cols(.,
#'                  "probability",
#'                  partitionby = 'subjectID') %.>%
#'   pick_top_k(.,
#'              partitionby = 'subjectID',
#'              orderby = c('probability', 'surveyCategory'),
#'              reverse = c('probability')) %.>%
#'   rename_columns(., 'diagnosis' %:=% 'surveyCategory') %.>%
#'   select_columns(., c('subjectID',
#'                       'diagnosis',
#'                       'probability')) %.>%
#'   orderby(., 'subjectID')
#' cat(format(optree))
#'
#' @export
#'
normalize_cols <- function(source,
                           columns,
                           ...,
                           partitionby = NULL,
                           env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::normalize_cols")
  assignments <- lapply(columns,
                        function(ci) {
                          paste0(ci, "/sum(", ci ,")")
                        })
  names(assignments) <- columns
  source %.>%
    extend_se(.,
              assignments = assignments,
              partitionby = partitionby,
              env = env)
}
