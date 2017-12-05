

merge_fld <- function(reslist, field) {
  got <- lapply(reslist,
                function(ri) {
                  ri[[field]]
                })
  unique(unlist(got))
}

#' Prepare a statement for SQL.
#'
#'
#' @param lepxr item from  \code{substitute}
#' @param colnames column names of table
#' @param db database connection (for DBI quoting)
#' @param env environment to look for values
#' @return sql info: list(orig, parsed, symbols_used, symbols_produced)
#'
#' @noRd
#'
prepForSQL <- function(lexpr, colnames, db,
                       env = parent.frame()) {
  n <- length(lexpr)
  res <- list(orig = lexpr,
              parsed = "",
              symbols_used = character(0),
              symbols_produced = character(0))
  # just in case (establishes an invarient of n>=1)
  if(n<=0) {
    return(res)
  }
  # left-hand sides of lists/calls are represented as keys
  nms <- names(lexpr)
  if(length(nms)>0) {
   stop("rquery::prepForSQL saw named items")
  }
  # special cases
  if(is.call(lexpr)) {
    callName <- as.character(lexpr[[1]])
    inlineops = c(":=", "==", "!=", ">=", "<=", "=", "<", ">", "+", "-", "*", "/")
    if((n==3) && (length(lexpr[[2]]==1)) && (callName %in% inlineops)) {
      if(callName=="==") {
        callName <- "="
      }
      rhs <- prepForSQL(lexpr[[3]],
                        colnames = colnames,
                        db = db,
                        env = env)
      if(callName==":=") {
        names(rhs$parsed) <- as.character(lexpr[[2]])
        res$parsed <- rhs$parsed
        res$symbols_used <- rhs$symbols_used
        res$symbols_produced <- unique(c(as.character(lexpr[[2]]),
                                         rhs$symbols_produced))
        return(res)
      }
      lhs <- prepForSQL(lexpr[[2]],
                        colnames = colnames,
                        db = db,
                        env = env)
      res$parsed <- paste(lhs$parsed, callName, rhs$parsed)
      res$symbols_used = merge_fld(list(lhs, rhs),
                                   "symbols_used")
      res$symbols_produced = merge_fld(list(lhs, rhs),
                                       "symbols_produced")
      return(res)
    }
    rest <- vector(n-1, mode="list")
    if(n>=2) {
      for(i in 2:n) {
        rest[[i-1]] <- prepForSQL(lexpr[[i]],
                                  colnames = colnames,
                                  db = db,
                                  env = env)
      }
    }
    subqstrs <- vapply(rest,
                       function(ri) {
                         ri$parsed
                       }, character(1))
    res$parsed <- paste0(callName,
                         "(", paste(subqstrs, collapse = ", "),")")
    res$symbols_used <- merge_fld(rest,
                                  "symbols_used")
    res$symbols_produced <- merge_fld(rest,
                                      "symbols_produced")
    return(res)
  }
  # basic recurse, establish invariant n==1
  if(n>1) {
    sube <- lapply(lexpr,
                   function(ei) {
                     prepForSQL(ei,
                                colnames = colnames,
                                db = db,
                                env = env)
                   })
    subqstrs <- vapply(sube,
                       function(ri) {
                         ri$parsed
                       }, character(1))
    res$parsed <- paste(sube, collapse = " ")
    res$symbols_used <- merge_fld(sube,
                                  "symbols_used")
    res$symbols_produced = merge_fld(sube,
                                     "symbols_produced")
    return(res)
  }
  # now have n==1
  if(is.name(lexpr)) {
    lexpr <- as.character(lexpr)
    if(lexpr %in% colnames) {
      res$symbols_used <- lexpr
      res$parsed <- as.character(DBI::dbQuoteIdentifier(db, lexpr))
      return(res)
    }
    tryCatch({
      v <- base::get(lexpr, envir = env)
      if(is.character(v)) {
        res$parsed <- as.character(DBI::dbQuoteString(db, v))
        return(res)
      }
      if(is.numeric(v)) {
        res$parsed <- as.character(v)
        return(res)
      }
    },
    error = function(e) { NULL })
  }
  if(is.character(lexpr)) {
    res$parsed <- as.character(DBI::dbQuoteString(db, lexpr))
    return(res)
  }
  # fall-back
  res$parsed <- paste(as.character(lexpr), collapse = " ")
  return(res)
}
