
#' Prepare a statement for SQL.
#'
#'
#' @param lepxr item from  \code{substitute}
#' @param colnames column names of table
#' @param db database connection (for DBI quoting)
#' @param env environment to look for values
#' @return sql info
#'
#' @noRd
#'
prepForSQL <- function(lexpr, colnames, db,
                       env = parent.frame()) {
  nexpr <- lexpr
  n <- length(nexpr)
  # just in case (establishes an invarient of n>=1)
  if(n<=0) {
    return("")
  }
  # left-hand sides of lists/calls are represented as keys
  nms <- names(nexpr)
  if(length(nms)>0) {
   stop("rquery::prepForSQL saw named items")
  }
  # special cases
  if(is.call(nexpr)) {
    callName <- as.character(nexpr[[1]])
    inlineops = c(":=", "==", "!=", ">=", "<=", "=", "<", ">", "+", "-", "*", "/")
    if((n==3) && (length(nexpr[[2]]==1)) && (callName %in% inlineops)) {
      if(callName=="==") {
        callName <- "="
      }
      rhs <-  prepForSQL(nexpr[[3]],
                         colnames = colnames,
                         db = db,
                         env = env)
      if(callName==":=") {
        names(rhs) <- as.character(nexpr[[2]])
        return(rhs)
      }
      lhs <- prepForSQL(nexpr[[2]],
                        colnames = colnames,
                        db = db,
                        env = env)
      res <- paste(lhs, callName, rhs)
      return(res)
    }
    rest <- character(n-1)
    if(n>=2) {
      for(i in 2:n) {
        rest[[i-1]] <- prepForSQL(nexpr[[i]],
                                  colnames = colnames,
                                  db = db,
                                  env = env)
      }
    }
    return(paste0(callName, "(", paste(rest, collapse = ", "),")"))
  }
  # basic recurse, establish invariant n==1
  if(n>1) {
    sube <- vapply(nexpr,
                   function(ei) {
                     prepForSQL(ei,
                                colnames = colnames,
                                db = db,
                                env = env)
                   },
                   character(1))
    return(paste(sube, collapse = " "))
  }
  # now have n==1
  # re-map quoted strings (except above)
  if(is.name(nexpr)) {
    nexpr <- as.character(nexpr)
    if(nexpr %in% colnames) {
      return(as.character(DBI::dbQuoteIdentifier(db, nexpr)))
    }
    tryCatch({
      v <- base::get(nexpr, envir = env)
      if(is.character(v)) {
        return(as.character(DBI::dbQuoteString(db, v)))
      }
      if(is.numeric(v)) {
        return(as.character(v))
      }
    },
    error = function(e) { NULL })
    return(nexpr)
  }
  if(is.character(nexpr)) {
    return(as.character(DBI::dbQuoteString(db, nexpr)))
  }
  # fall-back
  return(paste(as.character(nexpr), collapse = " "))
}
