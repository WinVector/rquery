

#' Build an optree pipeline counts rows.
#'
#' This is an example of building up a desired pre-prepared pipeline fragment from relop nodes.
#'
#' @param source relop tree or data.frame source.
#' @param ... force later arguments to bind by name.
#' @param groupby partitioning (window function) column names.
#' @param env environment to look for values in.
#'
#' @examples
#'
#' # by hand logistic regression example
#' d <- mk_td("survey_table",
#'            c("subjectID", "surveyCategory", "assessmentTotal"))
#' optree <- d %.>%
#'   row_counts(., groupby = "subjectID")
#' cat(format(optree))
#'
#' @export
#'
row_counts <- function(source,
                       ...,
                       groupby = character(0),
                       env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::row_counts")
  one <- NULL # don't look like an unbound reference
  pipe <- source %.>%
    extend(., one = 1) %.>%
    project(., groupby = groupby, n = sum(one))
  pipe
}
