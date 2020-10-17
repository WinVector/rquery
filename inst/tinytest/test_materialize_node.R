
test_materialize <- function() {
  if (requireNamespace("RSQLite", quietly = TRUE) &&
      requireNamespace("DBI", quietly = TRUE)) {
    raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    dbopts <- rq_connection_tests(raw_connection)
    db_handle <- rquery_db_info(connection = raw_connection,
                                is_dbi = TRUE,
                                connection_options = dbopts)

    d <- rq_copy_to(db_handle, "data", data.frame(x_col = 1, y_col = 2, z_col = 3),
                      temporary = TRUE, overwrite = TRUE)

    p1 <- d %.>%
      materialize_node(., "tmp1")
    p2 <- d %.>%
      materialize_node(., "tmp2") %.>%
      select_columns(., columns =c("x_col", "y_col"))
    sql1 <- to_sql(p1, db_handle)
    expect_equal(1, length(grep("z_col", sql1[[1]], fixed = TRUE)))
    expect_equal(1, length(grep("z_col", sql1[[3]], fixed = TRUE)))
    sql2 <- to_sql(p2, db_handle)
    expect_equal(0, length(grep("z_col", sql2[[1]], fixed = TRUE)))
    expect_equal(0, length(grep("z_col", sql2[[3]], fixed = TRUE)))
    l1 <- execute(db_handle, p1)
    expect_equal(3, length(column_names(l1)))
    l2 <- execute(db_handle, p2)
    expect_equal(2, length(column_names(l2)))

    DBI::dbDisconnect(raw_connection)
  }

  invisible(NULL)
}
