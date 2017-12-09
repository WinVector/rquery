

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
#' @param node rquery node we are working in context of (for DB quoting)
#' @param env environment to look for values
#' @return sql info: list(presentation, parsed, symbols_used, symbols_produced)
#'
#' @noRd
#'
prepForSQL <- function(lexpr, colnames, node,
                       env = parent.frame()) {
  n <- length(lexpr)
  res <- list(presentation = paste(as.character(lexpr), collapse = ' '),
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
    if(callName=="(") {
      sres <- prepForSQL(lexpr[[2]],
                         colnames = colnames,
                         node = node,
                         env = env)
      sres$presentation <- paste("(", sres$presentation, ")")
      sres$parsed <- paste("(", sres$parsed, ")")
      return(sres)
    }
    if(callName=="!") {
      sres <- prepForSQL(lexpr[[2]],
                         colnames = colnames,
                         node = node,
                         env = env)
      sres$presentation <- paste("!(", sres$presentation, ")")
      sres$parsed <- paste("( NOT ( ", sres$parsed, "))")
      return(sres)
    }
    args <- list()
    if(n>=2) {
      args <- lapply(2:n,
                     function(i) {
                       prepForSQL(lexpr[[i]],
                                  colnames = colnames,
                                  node = node,
                                  env = env)
                     })
      res$symbols_used <- merge_fld(args,
                                    "symbols_used")
      res$symbols_produced <- merge_fld(args,
                                        "symbols_produced")
    }
    inlineops = c(":=", "==", "!=", ">=", "<=", "=",
                  "<", ">",
                  "+", "-", "*", "/",
                  "&&", "||",
                  "&", "|")
    if((n==3) && (callName %in% inlineops)) {
      res$presentation <- paste(args[[1]]$presentation,
                                callName,
                                args[[2]]$presentation)
    } else {
      argstr <- vapply(args,
                       function(ai) {
                         ai$presentation
                       }, character(1))
      res$presentation <- paste0(callName,
                                 "(",
                                 paste(argstr, collapse = ", "),
                                 ")")
    }
    # TODO: make special cases like this table driven
    if((n==4) && (callName=="ifelse")) {
      res$symbols_used = merge_fld(args,
                                   "symbols_used")
      res$symbols_produced = merge_fld(args,
                                       "symbols_produced")
      res$parsed <- paste0("( CASE WHEN ",
                           args[[1]]$parsed,
                           " THEN ",
                           args[[2]]$parsed,
                           " ELSE ",
                           args[[3]]$parsed,
                           " END )")
      return(res)
    }
    if((n==3) && (length(lexpr[[2]]==1)) && (callName %in% inlineops)) {
      lhs <- args[[1]]
      rhs <- args[[2]]
      if(callName==":=") { # assignment special case
        names(rhs$parsed) <- as.character(lexpr[[2]])
        res$parsed <- rhs$parsed
        res$symbols_used <- rhs$symbols_used
        res$symbols_produced <- unique(c(as.character(lexpr[[2]]),
                                         rhs$symbols_produced))
        return(res)
      }
      replacements <- list("==" = "=",
                           "&&" = "AND",
                           "||" = "OR")
      replacement <- replacements[[callName]]
      if(!is.null(replacement)) {
        callName <- replacement
      }
      res$parsed <- paste(lhs$parsed, callName, rhs$parsed)
      res$symbols_used = merge_fld(list(lhs, rhs),
                                   "symbols_used")
      res$symbols_produced = merge_fld(list(lhs, rhs),
                                       "symbols_produced")
      return(res)
    }
    subqstrs <- vapply(args,
                       function(ri) {
                         ri$parsed
                       }, character(1))
    res$parsed <- paste0(callName,
                         "(", paste(subqstrs, collapse = ", "),")")
    return(res)
  }
  # basic recurse, establish invariant n==1
  if(n>1) {
    sube <- lapply(lexpr,
                   function(ei) {
                     prepForSQL(ei,
                                colnames = colnames,
                                node = node,
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
      res$parsed <- as.character(quote_identifier(node, lexpr))
      return(res)
    }
    v <- base::mget(lexpr,
                    envir = env,
                    ifnotfound = list(NULL),
                    inherits = TRUE)[[1]]
    if(is.character(v)) {
      res$parsed <- as.character(quote_string(node, v))
      return(res)
    }
    if(is.numeric(v)) {
      res$parsed <- as.character(v)
      return(res)
    }
  }
  if(is.character(lexpr)) {
    res$parsed <- as.character(quote_string(node, lexpr))
    return(res)
  }
  # fall-back
  res$parsed <- paste(as.character(lexpr), collapse = " ")
  return(res)
}
