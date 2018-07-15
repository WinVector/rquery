

#' Build a sequence of statements simulating an if/else block-\code{if(){}else{}}.
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
#' @param ... force later arguments to bind by name.
#' @param thenexprs named character then assignments (altering columns, not creating).
#' @param elseexprs named character else assignments (altering columns, not creating).
#' @return sequence of statements for extend_se().
#'
#' @seealso \code{\link{if_else_op}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   # Example: clear one of a or b in any row where both are set.
#'   # Land random selections early to avoid SQLite bug.
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(
#'     my_db,
#'     'd',
#'     data.frame(i = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'                a = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
#'                b = c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1),
#'                r = runif(10),
#'                edited = 0),
#'     temporary=TRUE, overwrite=TRUE)
#'
#'   program <- if_else_block(
#'     testexpr = qe((a+b)>1),
#'     thenexprs = c(
#'       if_else_block(
#'         testexpr = qe(r >= 0.5),
#'         thenexprs = qae(a %:=% 0),
#'         elseexprs = qae(b %:=% 0)),
#'       qae(edited %:=% 1)))
#'   print(program)
#'
#'   optree <- extend_se(d, program)
#'   cat(format(optree))
#'
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   # Why we need to land the random selection early
#'   # for SQLIte:
#'   q <- "SELECT r AS r1, r AS r2 FROM (
#'           SELECT random() AS r FROM (
#'              SELECT * from ( VALUES(1),(2) )
#'           ) a
#'        ) b"
#'   print(DBI::dbGetQuery(my_db, q))
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
if_else_block <- function(testexpr,
                          ...,
                          thenexprs = NULL,
                          elseexprs = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::if_else_block")
  if((length(thenexprs) + length(elseexprs))<=0) {
    return(NULL)
  }
  # get a fresh symbol
  knownsyms <- c(names(thenexprs), names(elseexprs))
  testsym <- setdiff(
    paste0('ifebtest_', seq_len(length(knownsyms)+1)),
    knownsyms)[[1]]
  program <- c(testsym %:=% testexpr) # this statement is special, perculates out
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


#' Build a \code{relop} node simulating a per-row block-\code{if(){}else{}}.
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
#' @param source optree relop node or data.frame.
#' @param testexpr character containing the test expression.
#' @param ... force later arguments to bind by name.
#' @param thenexprs named character then assignments (altering columns, not creating).
#' @param elseexprs named character else assignments (altering columns, not creating).
#' @param env environment to look to.
#' @return operator tree or data.frame.
#'
#' @seealso \code{\link{if_else_block}}
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   # Example: clear one of a or b in any row where both are set.
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- rq_copy_to(
#'     my_db,
#'     'd',
#'     data.frame(i = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'                a = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
#'                b = c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1),
#'                edited = NA),
#'     temporary=TRUE, overwrite=TRUE)
#'
#'   optree <- d %.>%
#'     if_else_op(.,
#'                testexpr = qe((a+b)>1),
#'                thenexprs = qae(a %:=% 0,
#'                                b %:=% 0,
#'                                edited %:=% 1),
#'                elseexprs = qae(edited %:=% 0))
#'   cat(format(optree))
#'
#'   sql <- to_sql(optree, my_db)
#'   cat(sql)
#'
#'   print(DBI::dbGetQuery(my_db, sql))
#'
#'   DBI::dbDisconnect(my_db)
#' }
#'
#'
#' @export
#'
if_else_op <- function(source,
                       testexpr,
                       ...,
                       thenexprs = NULL,
                       elseexprs = NULL,
                       env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::if_else_op")
  steps <- if_else_block(testexpr = testexpr,
                         thenexprs = thenexprs,
                         elseexprs = elseexprs)
  source %.>%
    extend_se(., steps, env = env) %.>%
    drop_columns(., "ifebtest_1", env = env)
}

