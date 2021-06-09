
test_join_check <- function() {

  d1 <- mk_td("d", "x")
  d2 <- mk_td("d", "x")
  ops <- natural_join(d1, d2, by = "x")

  d1 <- mk_td("d", "x")
  d2 <- mk_td("d", c("x", "y"))
  expect_error({
    # expect an exception here
    ops <- natural_join(d1, d2, by = "x")
  })

  # check same for theta_join

  d1 <- mk_td("d", "x")
  d2 <- mk_td("d", "x")
  ops <- theta_join(d1, d2, x == x)

  d1 <- mk_td("d", "x")
  d2 <- mk_td("d", c("x", "y"))
  expect_error({
    # expect an exception here
    ops <- theta_join(d1, d2, x == x)
  })

  if(requireNamespace("DBI", quietly = TRUE) &&
     requireNamespace("RSQLite", quietly = TRUE)) {
    raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::initExtension(raw_connection)
    db <- rquery_db_info(
      connection = raw_connection,
      is_dbi = TRUE,
      connection_options = rq_connection_tests(raw_connection))

    d1 <- rq_copy_to(
      db, 'd1',
      build_frame(
        "key", "val", "val1" |
          "a"  , 1  ,  10    |
          "b"  , NA  ,  11    |
          "c"  , 3  ,  12    ))
    d2 <- rq_copy_to(
      db, 'd2',
      build_frame(
        "key", "val", "val2" |
          "a"  , 5  ,  13    |
          "b"  , 6  ,  14    |
          "d"  , NA  ,  15    ))

    # key matching join
    optree <- natural_join(d1, d2,
                           jointype = "LEFT", by = 'key')
    res <- execute(db, optree)
    expect <- wrapr::build_frame(
      "key"  , "val", "val1", "val2"   |
        "a"  , 1    , 10    , 13       |
        "b"  , 6    , 11    , 14       |
        "c"  , 3    , 12    , NA_real_ )
    expect_true(wrapr::check_equiv_frames(expect, res))

    # renaming join
    t1 <- rq_copy_to(db, "t1", data.frame(a = c(1, 2), b = c(3, 4)))
    t2 <- rq_copy_to(db, "t2", data.frame(c = c(2, 1), d = c(6, 5)))

    ops <- natural_join(t1, t2, by = c("a" = "c"))

    res2 <- execute(db, ops)
    expect2 <- wrapr::build_frame(
      "a"  , "b", "d" |
        1  , 3  , 5   |
        2  , 4  , 6   )
    expect_true(wrapr::check_equiv_frames(expect2, res2))

    DBI::dbDisconnect(raw_connection)
  }

  invisible(NULL)
}

test_join_check()

