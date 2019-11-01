


is_inline_expr <- function(lexpr) {
  if(!is.call(lexpr)) {
    return(FALSE)
  }
  if(length(lexpr)!=3) {
    return(FALSE)
  }
  callName <- trimws(as.character(lexpr[[1]]), which = "both")
  inlineops <- c("%:=%", ":=", "==", "!=", ">=", "<=", "=",
                 "<", ">",
                 "+", "-", "*", "/",
                 "^",
                 ":",
                 "&&", "||",
                 "&", "|")
  if(callName %in% inlineops) {
    return(TRUE)
  }
  if(length(grep("^%.*%$", callName))==1) {
    return(TRUE)
  }
  return(FALSE)
}

check_for_forbidden_forms <- function(lexpr) {
  if(is.call(lexpr) && is_inline_expr(lexpr)) {
    call_text = as.character(lexpr[[1]])
    if(call_text %in% c("%in%", "in")) {
      stop(paste("rquery forbidden construction: ",
                 call_text,
                 "(please try a set_indicator() node instead)"))
    }
  }
}


#' Cross-parse a call from an R parse tree strings
#'
#' @param lexpr item from  \code{substitute} with length(lexpr)>0 and is.call(lexpr)
#' @param colnames column names of table
#' @param env environment to look for values
#' @return character
#'
#' @noRd
#'
tokenize_call_for_R <- function(lexpr, colnames, env) {
  if(is.null(lexpr)) {
    return("NULL")
  }
  if((!is.language(lexpr)) && (length(lexpr)==1) && is.na(lexpr)) {
    return("NA")
  }
  check_for_forbidden_forms(lexpr)
  if(is_inline_expr(lexpr)) {
    callName <- as.character(lexpr[[1]])
    lhs <- lexpr[[2]]
    if(callName %in% c("=", ":=", "%:=%")) {
      lhs <- as.character(lhs)[[1]]
    } else {
      lhs <- tokenize_call_for_R(lhs, colnames, env)
    }
    rhs <- lexpr[[3]]
    return(paste(lhs, callName, tokenize_call_for_R(rhs, colnames, env)))
  }
  if(is.character(lexpr)) {
    return(rquery_deparse(lexpr))
  }
  if(is.name(lexpr)) {
    sym <- as.character(lexpr)
    if(!(sym %in% colnames)) {
      if(exists(sym, envir = env)) {
        v <- get(sym, envir = env)
        if(!is.function(v)) {
          return(rquery_deparse(v))
        }
      }
    }
    return(rquery_deparse(lexpr))
  }
  if(as.character(lexpr[[1]])=="(") {
    sub <- ""
    if(length(lexpr)>1) {
      sub <- paste(vapply(lexpr[-1],
                          function(li) {
                            tokenize_call_for_R(li, colnames, env)
                          },
                          character(1)),
                   collapse = ", ")
    }
    return(paste0("(", sub, ")"))
  }
  if(is.call(lexpr)) {
    sub <- ""
    if(length(lexpr)>1) {
      sub <- paste(vapply(lexpr[-1],
                          function(li) {
                            tokenize_call_for_R(li, colnames, env)
                          },
                          character(1)),
                   collapse = ", ")
    }
    return(paste0(as.character(lexpr[[1]]), "(", sub, ")"))
  }
  if(length(lexpr)<=1) {
    return(rquery_deparse(lexpr))
  }
  return(paste(vapply(lexpr,
                      function(li) {
                        tokenize_call_for_R(li, colnames, env)
                      },
                      character(1)),
               collapse = " "))
}


#' Cross-parse a call from an R parse tree into SQL.
#'
#' @param lexpr item from  \code{substitute} with length(lexpr)>0 and is.call(lexpr)
#' @param colnames column names of table
#' @param env environment to look for values
#' @return sql info: list(parsed_toks(list of tokens), symbols_used, symbols_produced, free_symbols)
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
  check_for_forbidden_forms(lexpr)
  res <- list(parsed_toks = list(),
              symbols_used = character(0),
              symbols_produced = character(0),
              free_symbols = character(0))
  callName <- as.character(lexpr[[1]])
  if(n==1) {
    # zero argument call
    # wire-up for possible later name-remapping.
    ctok <- pre_sql_token(callName)
    ctok$is_zero_argument_call <- TRUE
    res$parsed_toks <- pre_sql_sub_expr(
      list(ctok, pre_sql_token("("),  pre_sql_token(")")),
      info = list(name = "zero_arg_call", call = callName))
    return(res)
  }
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
    res$symbols_used <- merge_fld(args,
                                  "symbols_used")
    res$symbols_produced <- merge_fld(args,
                                      "symbols_produced")
    res$free_symbols <- merge_fld(args,
                                  "free_symbols")
    subseq <- args[[1]]$parsed_toks
    if(length(args)>1) {
      subqstrs <- lapply(args,
                         function(ri) {
                           ri$parsed_toks
                         })
      # insert commas
      subqstrso <- subqstrs
      ns <- length(subqstrs)
      subqstrs <- rep(list(pre_sql_token(",")), ns+ns-1)
      subqstrs[2*seq_len(ns)-1] <- subqstrso
      subseq <- pre_sql_sub_expr(subqstrs,
                                 info = list(name = "arguments"))
    }
  }
  if(callName=="(") {
    res$parsed_toks <- pre_sql_sub_expr(
      list(pre_sql_token("("), subseq, pre_sql_token(")")),
      info = list(name = "paren_expr"))
    return(res)
  }
  if(callName=="!") {
    if(n!=2) {
      stop("rquery::tokenize_call_for_SQL expect ! to have 1 argument")
    }
    res$parsed_toks <- pre_sql_sub_expr(
      list(pre_sql_token("("), pre_sql_token("NOT"), pre_sql_token("("), subseq, pre_sql_token(")"), pre_sql_token(")")),
      info = list(name = "not"))
    return(res)
  }
  if(callName=='is.na') {
    if(n!=2) {
      stop("rquery::tokenize_call_for_SQL expect is.na to have 1 argument")
    }
    res$parsed_toks <- pre_sql_sub_expr(
      list(pre_sql_token("("), pre_sql_token("("), subseq, pre_sql_token(")"), pre_sql_token("IS NULL"), pre_sql_token(")")),
      info = list(name = "is.na"))
    return(res)
  }
  if(callName=='ifelse') {
    if(n!=4) {
      stop("rquery::tokenize_call_for_SQL expect ifelse to have 3 arguments")
    }
    res$parsed_toks <- pre_sql_sub_expr(
      list(pre_sql_token("("),
           pre_sql_token("CASE"),
           pre_sql_token("WHEN"),
           pre_sql_token("("),
           args[[1]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("THEN"),
           pre_sql_token("("),
           args[[2]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("WHEN"),
           pre_sql_token("NOT ("),
           args[[1]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("THEN"),
           pre_sql_token("("),
           args[[3]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("ELSE"),
           pre_sql_token("NULL"),
           pre_sql_token("END"),
           pre_sql_token(")")),
      info = list(name = "ifelse"))
    return(res)
  }
  if(is_inline_expr(lexpr)) {
    if(callName=="^") {
      res$parsed_toks <- pre_sql_sub_expr(
        list(pre_sql_fn("POWER"),
             pre_sql_token("("),
             args[[1]]$parsed_toks,
             pre_sql_token(","),
             args[[2]]$parsed_toks,
             pre_sql_token(")")),
        info = list(name = "power"))
      return(res)
    }
    if(callName=="%%") {
      res$parsed_toks <- pre_sql_sub_expr(
        list(pre_sql_fn("MOD"),
             pre_sql_token("("),
             args[[1]]$parsed_toks,
             pre_sql_token(","),
             args[[2]]$parsed_toks,
             pre_sql_token(")")),
        info = list(name = "modulo"))
      return(res)
    }
    lhs <- args[[1]]
    rhs <- args[[2]]
    if(callName %in% c("=", ":=", "%:=%")) { # assignment special case
      res$parsed_toks <- rhs$parsed_toks
      res$symbols_used <- rhs$symbols_used
      res$symbols_produced <- unique(c(as.character(lexpr[[2]]),
                                       rhs$symbols_produced))
      res$free_symbols <- rhs$free_symbols
      return(res)
    }
    replacements <- list("==" = "=",
                         "&&" = "AND",
                         "&" = "AND",
                         "||" = "OR",
                         "|" = "OR")
    replacement <- replacements[[callName]]
    if(!is.null(replacement)) {
      callName <- replacement
    }
    res$parsed_toks <- pre_sql_sub_expr(
      list(lhs$parsed_toks, pre_sql_token(callName), rhs$parsed_toks),
      info = list(name = "inline_op",
                  op = callName))
    return(res)
  }
  if(callName %in% c("pmin", "pmax")) {
    if(n!=3) {
      stop("rquery::tokenize_call_for_SQL expect pmin/pmax to have 2 arguments")
    }
    if(callName=="pmax") {
      comptok <- pre_sql_token(">=")
    } else {
      comptok <- pre_sql_token("<=")
    }
    res$parsed_toks <- pre_sql_sub_expr(
      list(pre_sql_token("("),
           pre_sql_token("CASE"),
           pre_sql_token("WHEN"),
           pre_sql_token("("),
           args[[1]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("IS NULL"),
           pre_sql_token("THEN"),
           pre_sql_token("("),
           args[[2]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("WHEN"),
           pre_sql_token("("),
           args[[2]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("IS NULL"),
           pre_sql_token("THEN"),
           pre_sql_token("("),
           args[[1]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("WHEN"),
           pre_sql_token("("),
           args[[1]]$parsed_toks,
           pre_sql_token(")"),
           comptok,
           pre_sql_token("("),
           args[[2]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("THEN"),
           pre_sql_token("("),
           args[[1]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("ELSE"),
           pre_sql_token("("),
           args[[2]]$parsed_toks,
           pre_sql_token(")"),
           pre_sql_token("END"),
           pre_sql_token(")")),
      info = list(name = callName))
    return(res)
  }
  # default
  if(length(subseq)>0) {
    res$parsed_toks <- pre_sql_sub_expr(
      list(pre_sql_fn(callName), pre_sql_token("("), subseq, pre_sql_token(")")),
      info = list("call"))
  } else {
    res$parsed_toks <- pre_sql_sub_expr(
      list(pre_sql_fn(callName), pre_sql_token("("), pre_sql_token(")")),
      info = list("zero_arg_call"))
  }
  return(res)
}


#' Cross-parse from an R parse tree into SQL.
#'
#'
#' @param lexpr item from  \code{substitute}
#' @param colnames column names of table
#' @param env environment to look for values
#' @return sql info: list(parsed_toks(list of tokens), symbols_used, symbols_produced, free_symbols)
#'
#' @noRd
#'
tokenize_for_SQL_r <- function(lexpr,
                          colnames,
                          env) {
  check_for_forbidden_forms(lexpr)
  n <- length(lexpr)
  res <- list(parsed_toks = NULL,
              symbols_used = character(0),
              symbols_produced = character(0),
              free_symbols = character(0))
  # just in case (establishes an invarient of n>=1)
  if(n<=0) {
    res <- list(parsed_toks = pre_sql_token("NULL"),
                symbols_used = character(0),
                symbols_produced = character(0),
                free_symbols = character(0))
    return(res)
  }
  if((!is.language(lexpr)) && (length(lexpr)==1) && is.na(lexpr)) {
    res <- list(parsed_toks = pre_sql_token("NA"),  # TODO special case this to NULL?
                symbols_used = character(0),
                symbols_produced = character(0),
                free_symbols = character(0))
    return(res)
  }
  # left-hand sides of lists/calls are represented as keys
  nms <- names(lexpr)
  if(length(nms)>0) {
    stop("rquery::tokenize_for_SQL_r saw named items (rquery does not allow assignment of function arguments by name)")
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
    res$parsed_toks <- pre_sql_sub_expr(subqstrs,
                                        info = "list")
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
      res$parsed_toks <- pre_sql_identifier(lexpr)
      return(res)
    }
    # now look in environment
    v <- base::mget(lexpr,
                    envir = env,
                    ifnotfound = list(NULL),
                    inherits = TRUE)[[1]]
    if((length(v)<1) || ("function" %in% class(v))) {
      res$free_symbols <- lexpr
      res$parsed_toks <- pre_sql_token(paste(as.character(lexpr), collapse = " "))
      return(res)
    }
    if(length(v)==1) {
      if(is.na(v)) {
        res$parsed_toks <- pre_sql_token("NA")
        return(res)
      }
      if(is.character(v) || is.factor(v)) {
        res$parsed_toks <- pre_sql_string(as.character(v))
        return(res)
      }
      if(is.numeric(v)) {
        # TODO: see if we can keep things numeric
        res$parsed_toks <- pre_sql_token(paste(as.character(v), collapse = " "))
        return(res)
      }
      if(is.logical(v)) {
        res$parsed_toks <- pre_sql_token(paste(as.character(v), collapse = " "))
        return(res)
      }
    }
    # # # ISSUE: would like to throw here, but knitr during pkg build seems to define
    # # # saw this in block 319 of vignettes rquery_intro.Rmd
    # # # a complex structure called "res" that interferes with this code in vignettes!
    # if(length(v)>1) {
    #   stop(paste0("error: symbol lookup on ", lexpr, " is not a scalar value"))
    # }
    # binding to functions in the env is a problem we want to avoid here
    res$free_symbols <- lexpr
    # fall back
    res$parsed_toks <- pre_sql_token(paste(as.character(lexpr), collapse = " "))
    return(res)
  }
  if(is.character(lexpr)) {
    res$parsed_toks <- pre_sql_string(paste(as.character(lexpr), collapse = " "))
    return(res)
  }
  # fall-back (NA comes to here if not caught earlier)
  res$parsed_toks <- pre_sql_token(paste(as.character(lexpr), collapse = " "))
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
#' tokenize_for_SQL(substitute(1 + 2), colnames= NULL)
#' tokenize_for_SQL(substitute(a := 3), colnames= NULL)
#' tokenize_for_SQL(substitute(a %:=% ( 3 + 4 )), colnames= NULL)
#' tokenize_for_SQL(substitute(a %:=% rank(3, 4)), colnames= NULL)
#'
#' @keywords internal
#'
#' @export
#'
tokenize_for_SQL <- function(lexpr,
                             colnames,
                             env = parent.frame()) {
  force(env)
  p <- tokenize_for_SQL_r(lexpr = lexpr,
                          colnames = colnames,
                          env = env)
  presentation <- tokenize_call_for_R(lexpr,
                                      colnames = colnames,
                                      env = env)
  p <- c(list(presentation = presentation), p)
  p
}
