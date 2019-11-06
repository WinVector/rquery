
# TODO: switch to wrapr::check_equiv_frames
rquery_check_equiv_frames <- function(d1, d2,
                                      ...,
                                      tolerance = sqrt(.Machine$double.eps)) {
  if( (!is.data.frame(d1)) != (!is.data.frame(d2)) ) {
    return(FALSE)
  }
  d1 <- data.frame(d1)
  d2 <- data.frame(d2)
  if((nrow(d1)!=nrow(d2)) || (ncol(d1)!=ncol(d2))) {
    return(FALSE)
  }
  cols <- sort(colnames(d1))
  c2 <- sort(colnames(d2))
  if(!isTRUE(all.equal(cols, c2))) {
    return(FALSE)
  }
  d1 <- d1[, cols, drop=FALSE]
  d1 <- d1[wrapr::orderv(d1), , drop=FALSE]
  rownames(d1) <- NULL
  d2 <- d2[, cols, drop=FALSE]
  d2 <- d2[wrapr::orderv(d2), , drop=FALSE]
  rownames(d2) <- NULL
  for(c in cols) {
    c1 <- d1[[c]]
    c2 <- d2[[c]]
    if(is.numeric(c1) != is.numeric(c2)) {
      return(FALSE)
    }
    if(is.numeric(c1)) {
      if(!isTRUE(all.equal(c1, c2, tolerance=tolerance))) {
        return(FALSE)
      }
    } else {
      if(!isTRUE(all.equal(c1, c2))) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

