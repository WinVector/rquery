
#' Implement an affine transformaton
#'
#' @param source relop source (or data.frame source)
#' @param linear_transform matrix with row names taken from source column names (inputs), and column names are outputs.
#' @param offset vector of offsets with names same as column names of linear_transform.
#' @param ... force later arguments to bind by name
#' @param env environment to look for values in.
#' @return relop node
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- data.frame(AUC = 0.6, R2 = 0.2)
#'   source <- rq_copy_to(my_db, 'd',
#'                        d,
#'                        overwrite = TRUE,
#'                        temporary = TRUE)
#'   linear_transform <- matrix(c(1 ,1, 2, -1, 1, 0, 0, 0), nrow = 2)
#'   rownames(linear_transform) <- c("AUC", "R2")
#'   colnames(linear_transform) <- c("res1", "res2", "res3", "res4")
#'   offset <- c(5, 7, 1, 0)
#'   names(offset) <- colnames(linear_transform)
#'
#'   optree <- affine_transform(source, linear_transform, offset)
#'   cat(format(optree))
#'
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'
#'   print(DBI::dbGetQuery(my_db, sql))
#'   print(as.matrix(d) %*% linear_transform + offset)
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
affine_transform <- function(source, linear_transform, offset,
                             ...,
                             env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::affine_transform")
  linear_transform_name <- substitute(linear_transform)
  offset_name <- substitute(offset)
  display_form <- "affine_op(.)"
  if(is.name(linear_transform_name) && is.name(offset_name)) {
    display_form <- paste0("affine_op(. %*% ", as.character(linear_transform_name),
                           " + ", as.character(offset_name), ")")
  }
  using <- rownames(linear_transform)
  producing <- colnames(linear_transform)
  dups <- intersect(using, producing)
  if(length(dups)>0) {
    stop(paste("rquery::affine_transform overlap in incoming and outgoing column names",
               paste(dups, collapse = ", ")))
  }
  offset_names <- names(offset)
  if(!isTRUE(all.equal(sort(producing), sort(offset_names)))) {
    stop("rquery::affine_transform offset names must match linear_transform column names")
  }
  incoming <- column_names(source)
  missing <- setdiff(incoming, using)
  if(length(missing)>0) {
    stop(paste("rquery::affine_transform missing required need incoming columns",
               paste(missing, collapse = ", ")))
  }
  terms <- vapply(seq_len(length(producing)),
                  function(j) {
                    ri <- linear_transform[, j, drop = TRUE]
                    nz <- ri!=0
                    if(any(nz)) {
                      vi <- paste0("(", as.numeric(ri)[nz], ")")
                      expri <- paste(paste(names(ri)[nz], vi, sep = "*"), collapse = " + ")
                      expri <- paste(expri, offset[[colnames(linear_transform)[[j]]]], sep = " + ")
                    } else {
                      expri <- as.character(offset[[colnames(linear_transform)[[j]]]])
                    }
                    expri
                  }, character(1))
  names(terms) <- colnames(linear_transform)
  extend_se(source, terms, display_form = display_form)
}
