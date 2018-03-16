
#' partition expressions
#'
#' Find longest ordered not created and used in same block chains.
#'
#' We assume the sequence of expressions is in a valid order
#' (all items available before use).  This function partitions the expressions
#' into ordered longest "no new value used blocks" by greedily scanning forward
#' remaining expressions in order taking any that: have all their values available from earlier groups,
#' do not use a value formed in the current group, and do not overwrite a value formed in the current group.
#'
#' @param parsed list of parsed expressions
#' @return ordered list of mutate_se assignment blocks
#'
#' @noRd
#'
partition_assignments <- function(parsed) {
  n <- length(parsed)
  # find step to step dependences
  mostRecent <- list()
  deps <- vector(mode = 'list', length = n)
  for(i in 1:n) {
    si <- parsed[[i]]$symbols_used
    depsi <- numeric(0)
    dkeys <- intersect(si, names(mostRecent))
    if(length(dkeys)>0) {
      depsi <- as.numeric(mostRecent[dkeys])
    }
    deps[[i]] <- depsi
    mostRecent[parsed[[i]]$symbols_produced] <- i
  }
  de <- data.frame(origOrder = 1:n,
                   group = 0L)
  de$deps <- deps
  de$parsed <- parsed
  group <- 1L
  while(any(de$group<=0)) {
    # sweep forward in order greedily taking anything
    have <- which(de$group>0)
    usedInGroup <- NULL
    formedInGroup <- NULL
    for(i in 1:n) {
      lhsi <- de$parsed[[i]]$symbols_produced
      if( (de$group[[i]]<=0) &&  # available to take
          (!any(lhsi %in% formedInGroup)) && # not assigned to in this block
          (!any(lhsi %in% usedInGroup)) &&  # not used to in this block
          (length(intersect(de$deps[[i]], formedInGroup))<=0) && # not using a new value
          (length(setdiff(de$deps[[i]], have))<=0) # all pre-conditions met
      ) {
        usedInGroup <- unique(c(usedInGroup, parsed[[i]]$symbols_used))
        formedInGroup <- c(formedInGroup, lhsi)
        de$group[[i]] <- group
      }
    }
    if(length(formedInGroup)<=0) {
      # should only get here in error
      # but if we don't stop we will spin forever
      stop("rquery::partition_assignments pass failed to accumulate steps")
    }
    group <- group + 1L
  }
  de <- de[order(de$group, de$origOrder), , drop=FALSE]
  # break out into no-dependency blocks
  split(de, de$group)
}

