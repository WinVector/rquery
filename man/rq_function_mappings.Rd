% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_info.R
\name{rq_function_mappings}
\alias{rq_function_mappings}
\title{Return function mappings for a connection}
\usage{
rq_function_mappings(db, ..., qualifiers = NULL)
}
\arguments{
\item{db}{a rquery_db_info}

\item{...}{not used, force later arguments to bind by name}

\item{qualifiers}{optional named ordered vector of strings carrying additional db hierarchy terms, such as schema.}
}
\value{
data.frame of function mappings
}
\description{
Return function mappings for a connection
}
\keyword{internal}
