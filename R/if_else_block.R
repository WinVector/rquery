

#' Simulate a per-row block-\code{if(){}else{}}.
#'
#' NOT WORKING YET (FINDING BUGS ELSEWHERE).
#'
#' This device uses expression-\code{ifelse(,,)} to simulate the
#' more powerful per-row block-\code{if(){}else{}}.  The difference is
#' expression-\code{ifelse(,,)} can choose per-row what value to express,
#' whereas block-\code{if(){}else{}} can choose per-row where to assign multiple
#' values. By simulation we mean: a sequence of quoted mutate expressions
#' are emitted that implement the transform.  These expressions can then
#' be optimized into a minimal number of no-dependency
#' blocks by \code{\link{extend_se}} for efficient execution.
#' The idea is the user can write legible code in this notation, and
#' the translation turns it into safe and efficient code suitable for
#' execution either on \code{data.frame}s or at a big data scale using
#' \code{RPostgreSQL} or \code{sparklyr}.
#'
#' Note: \code{ifebtest_*}
#' is a reserved column name for this procedure.
#'
#' @param testexpr character containing the test expression.
#' @param thenexprs named character then assignments (altering columns, not creating).
#' @param elseexprs named character else assignments (altering columns, not creating).
#'
#' @examples
#'
#' # Example: clear one of a or b in any row where both are set.
#' my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' d <- dbi_copy_to(
#'   my_db,
#'   'd',
#'   data.frame(a = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
#'              b = c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1),
#'              edited = 0))
#'
#' program <- if_else_block(
#'   testexpr = qe((a+b)>1),
#'   thenexprs = c(
#'     if_else_block(
#'       testexpr = qe(random() >= 0.5),
#'       thenexprs = qae(a := 0),
#'       elseexprs = qae(b := 0)),
#'     qae(edited := 1)))
#' print(program)
#'
#' trf <- extend_se(d, program)
#' cat(gsub("%.>%", "%.>%\n   ", format(trf), fixed = TRUE))
#'
#' sql <- to_sql(trf)
#' cat(sql)
#'
#' DBI::dbGetQuery(my_db, sql)
#'
#'
#' @export
#'
if_else_block <- function(testexpr,
                          thenexprs = NULL,
                          elseexprs = NULL) {
  if((length(thenexprs) + length(elseexprs))<=0) {
    return(NULL)
  }
  # get a fresh symbol
  knownsyms <- c(names(thenexprs), names(elseexprs))
  testsym <- setdiff(
    paste0('ifebtest_', seq_len(length(knownsyms)+1)),
    knownsyms)[[1]]
  program <- c(testsym := testexpr) # this statement is special, perculates out
  # the idea is we don't have to nest testsym generation as it is a unique
  # name, so can not be confused with other values.
  prepStmts <- function(stmts, condition) {
    ret <- NULL
    n <- length(stmts)
    if(n<=0) {
      return(ret)
    }
    isSpecial <- startsWith(names(stmts), 'ifebtest_')
    if(any(isSpecial)) {
      spc <- stmts[isSpecial]
      stmts <- stmts[!isSpecial]
      ret <- c(ret, spc)
    }
    n <- length(stmts)
    if(n<=0) {
      return(ret)
    }
    nexprs <- vapply(1:n,
                     function(i) {
                       paste0('ifelse( ', condition,
                              ', ', stmts[[i]],
                              ', ', names(stmts)[[i]],
                              ')')
                     }, character(1))
    names(nexprs) <- names(stmts)
    ret <- c(ret,nexprs)
    ret
  }
  program <- c(program,
               prepStmts(thenexprs, testsym))
  program <- c(program,
               prepStmts(elseexprs, paste('! ', testsym)))
  program
}

