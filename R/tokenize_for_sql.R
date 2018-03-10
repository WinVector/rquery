
ltok <- function(v) {
  list(pre_sql_token(v))
}

#' Cross-parse a call from an R parse tree into SQL.
#'
#' @param lexpr item from  \code{substitute} with length(lexpr)>0 and is.call(lexpr)
#' @param colnames column names of table
#' @param env environment to look for values
#' @return sql info: list(presentation, parsed_toks(list of tokens), symbols_used, symbols_produced, free_symbols)
#'
#' @noRd
#'
tokenize_call_for_SQL <- function(lexpr,
                               colnames,
                               env) {
  n <- length(lexpr)
  if((n<=0) || (!is.call(lexpr))) {
    stop("rquery::tokenize_call_for_SQL called on non-call")
  }
  inlineops = c(":=", "==", "!=", ">=", "<=", "=",
                "<", ">",
                "+", "-", "*", "/",
                "&&", "||",
                "&", "|")
  res <- list(presentation = paste(as.character(deparse(lexpr)),
                                   collapse = ' '),
              parsed_toks = list(),
              symbols_used = character(0),
              symbols_produced = character(0),
              free_symbols = character(0))
  callName <- as.character(lexpr[[1]])
  args <- list()
  subseq <- list()
  subpres <- ""
  if(n>=2) {
    args <- lapply(2:n,
                   function(i) {
                     tokenize_for_SQL_r(lexpr[[i]],
                                   colnames = colnames,
                                   env = env)
                   })
    subqstrs <- lapply(args,
                       function(ri) {
                         ri$parsed_toks
                       })
    if(length(subqstrs)>1) {
      # insert commas
      subqstrso <- subqstrs
      ns <- length(subqstrs)
      subqstrs <- rep(list(ltok(",")), ns+ns-1)
      subqstrs[2*seq_len(ns)-1] <- subqstrso
    }
    subseq <- unlist(subqstrs, recursive = FALSE)
    subpresv <- vapply(args,
                       function(ri) {
                         ri$presentation
                       }, character(1))
    subpres <- paste(subpresv, collapse = " ")
    res$symbols_used <- merge_fld(args,
                                  "symbols_used")
    res$symbols_produced <- merge_fld(args,
                                      "symbols_produced")
    res$free_symbols <- merge_fld(args,
                                  "free_symbols")
  }
  if(callName=="(") {
    res$presentation <- paste("(", subpres, ")")
    res$parsed_toks <- c(ltok("("), subseq, ltok(")"))
    return(res)
  }
  if(callName=="!") {
    res$presentation <- paste("!(", subpres, ")")
    res$parsed_toks <- c(ltok("("), ltok("NOT"), ltok("("), subseq, ltok(")"), ltok(")"))
    return(res)
  }
  if(callName=='is.na') {
    if(n!=2) {
      stop("rquery::tokenize_call_for_SQL expect is.na to have 1 argument")
    }
    res$presentation <- paste0("is.na(", subpres, ")")
    res$parsed_toks <- c(ltok("("), args[[1]]$parsed_toks, ltok(")"), ltok("IS NULL"))
    return(res)
  }
  if(callName=='ifelse') {
    if(n!=4) {
      stop("rquery::tokenize_call_for_SQL expect ifelse to have 3 arguments")
    }
    res$presentation <- paste0(
      "ifelse(",
      args[[1]]$presentation,
      ", ",
      args[[2]]$presentation,
      ", ",
      args[[3]]$presentation,
      ")")
    res$parsed_toks <- c(ltok("("),
                         ltok("CASE WHEN"),
                         ltok("("),
                         args[[1]]$parsed_toks,
                         ltok(")"),
                         ltok("THEN"),
                         ltok("("),
                         args[[2]]$parsed_toks,
                         ltok(")"),
                         ltok("ELSE"),
                         ltok("("),
                         args[[3]]$parsed_toks,
                         ltok(")"),
                         ltok("END"),
                         ltok(")"))
    return(res)
  }
  # ifelse back in place.
  if((n==3) && (callName %in% inlineops)) {
    lhs <- args[[1]]
    rhs <- args[[2]]
    if(callName==":=") { # assignment special case
      res$parsed_toks <- rhs$parsed_toks
      res$symbols_used <- rhs$symbols_used
      res$symbols_produced <- unique(c(as.character(lexpr[[2]]),
                                       rhs$symbols_produced))
      res$free_symbols <- rhs$free_symbols
      res$presentation <- paste0(lhs$presentation, " := ", rhs$presentation)
      return(res)
    }
    replacements <- list("==" = "=",
                         "&&" = "AND",
                         "||" = "OR")
    replacement <- replacements[[callName]]
    if(!is.null(replacement)) {
      callName <- replacement
    }
    res$parsed_toks <- c(lhs$parsed_toks, ltok(callName), rhs$parsed_toks)
    res$presentation <- paste(lhs$presentation, callName, rhs$presentation)
    return(res)
  }
  if(callName %in% c("pmin", "pmax")) {
    if(n!=3) {
      stop("rquery::tokenize_call_for_SQL expect pmin/pmax to have 2 arguments")
    }
    if(callName=="pmax") {
      comptok <- ltok(">=")
    } else {
      comptok <- ltok("<=")
    }
    res$parsed_toks <- c(ltok("("),
                         ltok("CASE"),
                         ltok("WHEN"),
                         ltok("("),
                         args[[1]]$parsed_toks,
                         ltok(")"),
                         ltok("IS NULL"),
                         ltok("THEN"),
                         ltok("("),
                         args[[2]]$parsed_toks,
                         ltok(")"),
                         ltok("WHEN"),
                         ltok("("),
                         args[[2]]$parsed_toks,
                         ltok(")"),
                         ltok("IS NULL"),
                         ltok("THEN"),
                         ltok("("),
                         args[[1]]$parsed_toks,
                         ltok(")"),
                         ltok("WHEN"),
                         ltok("("),
                         args[[1]]$parsed_toks,
                         ltok(")"),
                         comptok,
                         ltok("("),
                         args[[2]]$parsed_toks,
                         ltok(")"),
                         ltok("THEN"),
                         ltok("("),
                         args[[1]]$parsed_toks,
                         ltok(")"),
                         ltok("ELSE"),
                         ltok("("),
                         args[[2]]$parsed_toks,
                         ltok(")"),
                         ltok("END"),
                         ltok(")"))
    return(res)
  }
  # default
  res$parsed_toks <- c(ltok(callName),
                       ltok("("), subseq, ltok(")"))
  res$presentation <- paste0(callName, "(", subpres, ")")
  return(res)
}


#' Cross-parse from an R parse tree into SQL.
#'
#'
#' @param lexpr item from  \code{substitute}
#' @param colnames column names of table
#' @param env environment to look for values
#' @return sql info: list(presentation, parsed_toks(list of tokens), symbols_used, symbols_produced, free_symbols)
#'
#' @noRd
#'
tokenize_for_SQL_r <- function(lexpr,
                          colnames,
                          env) {
  n <- length(lexpr)
  res <- list(presentation = paste(as.character(lexpr), collapse = ' '),
              parsed_toks = list(),
              symbols_used = character(0),
              symbols_produced = character(0),
              free_symbols = character(0))
  # just in case (establishes an invarient of n>=1)
  if(n<=0) {
    return(res)
  }
  # left-hand sides of lists/calls are represented as keys
  nms <- names(lexpr)
  if(length(nms)>0) {
    stop("rquery::tokenize_for_SQL_r saw named items")
  }
  # special cases
  if(is.call(lexpr)) {
    res <- tokenize_call_for_SQL(lexpr = lexpr,
                              colnames = colnames,
                              env = env)
    return(res)
  }
  # basic recurse, establish invariant n==1
  if(n>1) {
    sube <- lapply(lexpr,
                   function(ei) {
                     tokenize_for_SQL_r(ei,
                                   colnames = colnames,
                                   env = env)
                   })
    subqstrs <- lapply(sube,
                       function(ri) {
                         ri$parsed_toks
                       })
    subseq <- unlist(subqstrs, recursive = FALSE)
    res$parsed_toks <- subseq
    res$symbols_used <- merge_fld(sube,
                                  "symbols_used")
    res$symbols_produced = merge_fld(sube,
                                     "symbols_produced")
    res$free_symbols = merge_fld(sube,
                                 "free_symbols")
    return(res)
  }
  # now have n==1, handle identifiers and constants
  if(is.name(lexpr)) {
    lexpr <- as.character(lexpr)
    # look for columns
    if(lexpr %in% colnames) {
      res$symbols_used <- lexpr
      res$parsed_toks <- list(pre_sql_identifier(lexpr))
      return(res)
    }
    # now look in environment
    v <- base::mget(lexpr,
                    envir = env,
                    ifnotfound = list(NULL),
                    inherits = TRUE)[[1]]
    if(length(v)>0) {
      if(is.character(v)) {
        res$parsed_toks <- list(pre_sql_string(v))
        res$presentation <- paste0('"', v, '"')
        return(res)
      }
      if(is.numeric(v)) {
        res$parsed_toks <- list(pre_sql_token(paste(as.character(v), collapse = " ")))
        return(res)
      }
      # finding functions in the env is a problem here
    }
    res$free_symbols <- lexpr
    # fall back
    res$parsed_toks <- list(pre_sql_token(paste(as.character(lexpr), collapse = " ")))
    return(res)
  }
  if(is.character(lexpr)) {
    res$parsed_toks <- list(pre_sql_string(paste(as.character(lexpr), collapse = " ")))
    res$presentation <- paste0('"', paste(as.character(lexpr), collapse = " "), '"')
    return(res)
  }
  # fall-back
  res$parsed_toks <- list(pre_sql_token(paste(as.character(lexpr), collapse = " ")))
  return(res)
}


#' Cross-parse from an R parse tree into SQL.
#'
#'
#' @param lexpr item from  \code{substitute}
#' @param colnames column names of table
#' @param env environment to look for values
#' @return sql info: list(presentation, parsed_toks(list of tokens), sql_text, symbols_used, symbols_produced, free_symbols)
#'
#' @examples
#'
#' tokenize_for_SQL(substitute(1 + 1), colnames= NULL)
#'
#' @export
#'
tokenize_for_SQL <- function(lexpr,
                            colnames,
                            env = parent.frame()) {
  p <- tokenize_for_SQL_r(lexpr = lexpr,
                            colnames = colnames,
                            env = env)
  class(p$parsed_toks) <- c("pre_sql_expr")
  p
}
