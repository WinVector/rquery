

#' \code{rquery}: Relational Query Generator for Data Manipulation
#'
#' \code{rquery} supplies a piped query generator based on Edgar F. Codd's relational
#' algebra and operator names (plus experience using \code{SQL} at big data
#' scale).  The design represents an attempt to make \code{SQL} more teachable by
#' denoting composition a sequential pipeline notation instead of nested
#' queries or functions.  Package features include: data processing trees
#' or pipelines as observable objects (able to report both columns
#' produced and columns used), optimized \code{SQL} generation as an explicit
#' user visible modeling step, and convenience methods for applying query
#' trees to in-memory data.frames.
#'
#' @docType package
#' @name rquery
NULL


#' @importFrom wrapr apply_left
#' @export
wrapr::apply_left

#' @importFrom wrapr apply_right
#' @export
wrapr::apply_right


#' @importFrom wrapr mk_tmp_name_source
#' @export
wrapr::mk_tmp_name_source

#' @importFrom wrapr map_to_char
#' @export
wrapr::map_to_char

#' @importFrom wrapr %.>%
#' @export
wrapr::`%.>%`

#' @importFrom wrapr let
#' @export
wrapr::let

#' @importFrom wrapr qc
#' @export
wrapr::qc

#' @importFrom wrapr qe
#' @export
wrapr::qe

#' @importFrom wrapr qae
#' @export
wrapr::qae

#' @importFrom wrapr build_frame
#' @export
wrapr::build_frame

#' @importFrom wrapr draw_frame
#' @export
wrapr::draw_frame

#' @importFrom wrapr qchar_frame
#' @export
wrapr::qchar_frame


# can collide with data.table := , but we
# need this for a lot of the pipe examples.


#' @importFrom wrapr %:=%
#' @export
wrapr::`%:=%`

