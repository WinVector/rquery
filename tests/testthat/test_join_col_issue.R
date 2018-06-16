
library("rquery")
context("join_col_issue")



test_that("test_join_col_issue: Works As Expected", {
  if(requireNamespace("DBI", quietly = TRUE) &&
     requireNamespace("RSQLite", quietly = TRUE)) {
    db <- DBI::dbConnect(RSQLite::SQLite(),
                         ":memory:")

    ops <- readRDS("ops2.RDS")
    str <- to_sql(ops, db)
    match_idx <- grep("Var63", str)
    expect_equal(length(match_idx), 0)

    DBI::dbDisconnect(db)
  }
})
