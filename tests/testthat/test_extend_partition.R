library("rquery")

context("partitioning")

test_that("test_extend_partition: Works As Expected", {
  if (requireNamespace("RSQLite", quietly = TRUE) &&
      requireNamespace("DBI", quietly = TRUE)) {
    raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    dbopts <- rq_connection_tests(raw_connection)
    db_handle <- rquery_db_info(connection = raw_connection,
                                is_dbi = TRUE,
                                connection_options = dbopts)
    old_o <- options(list("rquery.rquery_db_executor" = list(db = db_handle)))

    d <- mk_td("d",
               c("rowNum",
                 "a_1", "a_2", "b_1", "b_2",
                 "c_1", "c_2", "d_1", "d_2",
                 "e_1", "e_2"  ))
    dQ <- d %.>%
      extend_se(.,
                if_else_block(
                  testexpr =
                    "rand()>=0.5",
                  thenexprs = qae(
                    a_1 %:=% 'treatment',
                    a_2 %:=% 'control'),
                  elseexprs =  qae(
                    a_1 %:=% 'control',
                    a_2 %:=% 'treatment')))
    txt <- format(dQ)
    testthat::expect_true(length(grep("!", txt, fixed = TRUE))>0)
    sql <- to_sql(dQ, db_handle)
    testthat::expect_true(length(grep("e_2", sql, fixed = TRUE))>0)


    d <- rq_copy_to(db_handle, "ds",
                     data.frame(x = 1))
    sum <- 7
    optree <- d %.>%
      extend(., sum = x + 1 , y = sum + 1)
    tab <- execute(db_handle, optree)
    tab <- tab[, c("sum", "x", "y")]
    testthat::expect_equal(data.frame(sum = 2, x = 1, y = 3), tab[ , c("sum", "x", "y"), drop = FALSE])
    tab2 <- data.frame(x = 1) %.>% optree
    testthat::expect_equal(data.frame(sum = 2, x = 1, y = 3), tab2[ , c("sum", "x", "y"), drop = FALSE])

    optree <- d %.>%
       extend(., a = 1, b := 2, c %:=% 4)
    tab3 <- execute(db_handle, optree)
    testthat::expect_equal(data.frame(x = 1, a = 1, b = 2, c = 4), tab3[ , c("x", "a", "b", "c"), drop = FALSE])

    options(old_o)
    DBI::dbDisconnect(raw_connection)
  }
})
