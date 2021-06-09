
test_grouped_running_op <- function() {

  if (requireNamespace("RSQLite", quietly = TRUE) &&
      requireNamespace("DBI", quietly = TRUE)) {
    raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    dbopts <- rq_connection_tests(raw_connection)
    db_handle <- rquery_db_info(connection = raw_connection,
                                is_dbi = TRUE,
                                connection_options = dbopts)

    data <- rq_copy_to(
      db_handle, "data",
      wrapr::build_frame(
        "x", "y" |
          1  , 1   |
          0  , 0   |
          1  , 0   |
          0  , 1   |
          0  , 0   |
          1  , 1   ))

    ops1 <- extend(data,
                      running_y_sum = cumsum(y),
                      partitionby = "x",
                      orderby = "y",
                      reverse = "y")
    sql1 <- to_sql(ops1, db_handle)
    # res1 <- execute(db_handle, ops1) # can't run on SQLite

    ops2 <- extend(data,
                       running_y_sum = cumsum(y),
                       partitionby = "x",
                       orderby = "y",
                       reverse = c("y"))
    sql2 <- to_sql(ops2, db_handle)

    DBI::dbDisconnect(raw_connection)

    str1 <- gsub("tsql_[0-9_]*", "tsq*", sql1)
    str2 <- gsub("tsql_[0-9_]*", "tsq*", sql2)
    expect_equal(str1, str2)
  }

  invisible(NULL)
}

test_grouped_running_op()

