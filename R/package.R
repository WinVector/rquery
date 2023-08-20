

#' \code{rquery}: Relational Query Generator for Data Manipulation
#'
#' \code{rquery} supplies a piped query generator based on Edgar F. Codd's relational
#' algebra and operator names (plus experience using \code{SQL} and \code{dplyr} at big data
#' scale).  The design represents an attempt to make \code{SQL} more teachable by
#' denoting composition a sequential pipeline notation instead of nested
#' queries or functions.  Package features include: data processing trees
#' or pipelines as observable objects (able to report both columns
#' produced and columns used), optimized \code{SQL} generation as an explicit
#' user visible modeling step, and convenience methods for applying query
#' trees to in-memory data.frames.
#'
#' Note: \code{rquery} is a "database first" design.  This means choices are made that
#' favor database implementation. These include: capturing the entire calculation prior
#' to doing any work (and using recursive methods to inspect this object, which can limit
#' the calculation depth to under 1000 steps at a time), preferring "tame column names"
#' (which isn't a bad idea in `R` anyway as columns and variables are often seen as cousins),
#'  and not preserving row or column order (or supporting numeric column indexing).  Also,
#'  \code{rquery} does have a fast in-memory implementation: \code{rqdatatable}
#'  (thanks to the \code{data.table}, so one can in fact use `rquery` without a database.
#'
"_PACKAGE"

#' @importFrom wrapr apply_right
NULL

#' @importFrom wrapr apply_left
NULL

