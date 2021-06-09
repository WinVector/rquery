
test_mod <- function() {
  my_db <- rquery_default_db_info()

  td <- mk_td("data", "x")
  ops <- extend(td, xm = x %% 2)
  sql <- to_sql(ops, my_db)
  #cat(sql)
  expect_equal(1, grep("MOD", sql, fixed = TRUE))

  if(requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE) ) {

    tree_rewriter <- function(x, db_info) {
      if(("pre_sql_sub_expr" %in% class(x)) &&
         (length(x$info$name) == 1) &&
         (x$info$name == "modulo")) {
        lhs <- x$toks[[3]]
        rhs <- x$toks[[5]]
        return(pre_sql_sub_expr(
          list(pre_sql_token("("),
               lhs,
               pre_sql_token("%"),
               rhs,
               pre_sql_token(")")),
          info=list(name = "user_replaced"))
        )
      }
      x
    }

    rsqlite_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    rsqlite_db <- rquery_db_info(
      connection = rsqlite_connection,
      is_dbi = TRUE,
      connection_options = rq_connection_tests(rsqlite_connection))

    # attach our tree-rewriter to the databse handle.
    # this handle now uses this re-writer.
    rsqlite_db$tree_rewriter <- tree_rewriter

    d <- data.frame(x=-3:3)

    ops <- local_td(d) %.>%
      extend(.,
             x_mod_2 = x %% 2) %.>%
      extend(.,
             # extra step to work around SQL mod
             # returns sign of argument.
             x_mod_2 = ifelse(x_mod_2 >=0, x_mod_2, x_mod_2 + 2))

    d_rsqlite <- rq_copy_to(rsqlite_db, "d", d,
                            temporary = TRUE, overwrite = TRUE)

    res <- ops %.>%
      rsqlite_db

    expect <- data.frame(x = -3:3,
                         x_mod_2 = c(1, rep(c(0,1),3)))
    expect_equal(expect, data.frame(res))

    DBI::dbDisconnect(rsqlite_connection)
  }

  invisible(NULL)
}

test_mod()

