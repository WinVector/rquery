% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_ops.R
\name{setDBOption}
\alias{setDBOption}
\title{Set a database connection option.}
\usage{
setDBOption(db, optname, val)
}
\arguments{
\item{db}{database connection handle.}

\item{optname}{character, single option name.}

\item{val}{value to set}
}
\value{
named list containing old value if any (invisible).
}
\description{
Set a database connection option.
}