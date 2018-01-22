

#' \code{rquery}: Relational Query Generator for Data Manipulation
#'
#' \code{rquery} supplies a query generator based on Codd's relational
#' algebra and operator names (plus experience using SQL at big data
#' scale).  The design is an attempt to make SQL more teachable by
#' denoting composition a sequential pipeline notation instead of nested
#' queries or functions.  Package features include: data processing trees
#' or pipelines as inspectable objects (able to report both columns
#' produced and columns used), optimized SQL generation as an explicit
#' user visible modeling step, and convenience methods for applying query
#' trees to in-memory data.frames.
#'
#' @docType package
#' @name rquery
NULL



#' @importFrom wrapr %.>% :=
#' @importFrom cdata makeTempNameGenerator
NULL
