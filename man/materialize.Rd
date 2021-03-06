% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/materialize.R
\name{materialize}
\alias{materialize}
\title{Materialize an optree as a table.}
\usage{
materialize(
  db,
  optree,
  table_name = mk_tmp_name_source("rquery_mat")(),
  ...,
  limit = NULL,
  source_limit = NULL,
  overwrite = TRUE,
  temporary = FALSE,
  qualifiers = NULL
)
}
\arguments{
\item{db}{database connecton (rquery_db_info class or DBI connections preferred).}

\item{optree}{relop operation tree.}

\item{table_name}{character, name of table to create.}

\item{...}{force later arguments to bind by name.}

\item{limit}{numeric if not NULL result limit (to use this, last statement must not have a limit).}

\item{source_limit}{numeric if not NULL limit sources to this many rows.}

\item{overwrite}{logical if TRUE drop an previous table.}

\item{temporary}{logical if TRUE try to create a temporary table.}

\item{qualifiers}{optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.}
}
\value{
table description
}
\description{
Run the data query as a CREATE TABLE AS . Think of as a function
that can be applied to relop trees, not as a component to place
in pipelines.
}
\examples{

if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
  my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  d <- rq_copy_to(my_db, 'd',
                   data.frame(AUC = 0.6, R2 = 0.2),
                   temporary = TRUE, overwrite = TRUE)
  optree <- extend_se(d, c("v" \%:=\% "AUC + R2", "x" \%:=\% "pmax(AUC,v)"))
  cat(format(optree))
  res <- materialize(my_db, optree, "example")
  cat(format(res))
  sql <- to_sql(res, my_db)
  cat(sql)
  print(DBI::dbGetQuery(my_db, sql))

  DBI::dbDisconnect(my_db)
}

}
\seealso{
\code{\link{db_td}}, \code{\link{execute}}, \code{\link{to_sql}}, \code{\link{rq_copy_to}}, \code{\link{mk_td}}
}
