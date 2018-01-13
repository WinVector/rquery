
#' Build per-group order assignments.
#'
#' @param groupid vector comparable group ids in contiguous blocks
#' @return per-group order assignment vector
#'
#' @examples
#'
#' g <- c(1, 1, 4, 2, 2, 5, 5, 5)
#' per_group_order_ids(g)
#' # [1] 1 2 1 1 2 1 2 3
#'
#' @export
#'
per_group_order_ids <- function(groupid) {
  n <- length(groupid)
  isfirst <- c(TRUE, groupid[-1] != groupid[-n])
  s1 <- cumsum(numeric(n) + 1.0)
  deltas <- (s1-1) * as.numeric(isfirst)
  s1 - cummax(deltas)
}

#' Build an id by category table.
#'
#' @param groupid character vector
#' @param catid character vector of length length(groupid), (should have a small number of unique values, each pair (groupid[[i]], catid[[i]] should occur at most once).
#' @return cframe data frame with one row per groupid and one column per catid, contents from values.
#'
#' @example
#'
#' gv <- c('01', '01', '02', '02', '03', '03')
#' cv <- c('a', 'b', 'a', 'b', 'a', 'b')
#' build_cframe(gv, cv)
#'
#' @export
#'
build_cframe <- function(groupid, catid) {
  origids <- 1:length(groupid)
  ids <- unique(groupid)
  nids <- length(ids)
  mp <- 1:nids
  names(mp) <- ids
  cats <- sort(unique(catid))
  cols <- lapply(cats,
                 function(ci) {
                   hit <- catid==ci
                   coli <- numeric(nids)
                   coli[mp[groupid[hit]]] <- origids[hit]
                   coli
                 })
  names(cols) <- cats
  d <- as.data.frame(cols, stringsAsFactors = FALSE)
  rownames(d) <- ids
  d
}

#' Compute grouped argmax().
#'
#' @param cframe build_cframe() result
#' @param v comparable values
#' @return index of first max in each group
#'
#' @examples
#'
#' gv <- c('01', '01', '02', '02', '03', '03')
#' cv <- c('a', 'b', 'a', 'b', 'a', 'b')
#' cframe <- build_cframe(gv, cv)
#' vs <- c(1, -1, 2, 4, 5, 3)
#' grouped_arg_max(cframe, vs)
#'
#' @export
#'
grouped_arg_max <- function(cframe, v) {
  selidxs <- cframe[[1]]
  maxvs <- v[selidxs]
  nc <- ncol(cframe)
  if(nc>1) {
    for(j in 2:nc) {
      idxs <- cframe[[j]]
      vs <- v[idxs]
      wants <- vs > maxvs
      wants[is.na(wants)] <- FALSE
      selidxs[wants] <- idxs[wants]
      maxvs[wants] <- vs[wants]
    }
  }
  names(selidxs) <- rownames(cframe)
  selidxs
}

#' Compute grouped sum().
#'
#' @param cframe build_cframe() result
#' @param v comparable values
#' @return index of first max in each group
#'
#' @examples
#'
#' gv <- c('01', '01', '02', '02', '03', '03')
#' cv <- c('a', 'b', 'a', 'b', 'a', 'b')
#' cframe <- build_cframe(gv, cv)
#' vs <- c(1, -1, 2, 4, 5, 3)
#' grouped_sum(cframe, vs)
#'
#' @export
#'
grouped_sum <- function(cframe, v) {
  nc <- ncol(cframe)
  sums <- numeric(nrow(cframe))
  for(j in 1:nc) {
    idxs <- cframe[[j]]
    vs <- v[idxs]
    vs[is.na(vs) || is.nan(vs)] <- 0.0
    sums <- sums + vs
  }
  names(sums) <- rownames(cframe)
  sums
}


