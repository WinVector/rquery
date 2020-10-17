
test_translation <- function() {
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    db <- DBI::dbConnect(RSQLite::SQLite(),
                         ":memory:")
    RSQLite::initExtension(db)

    # example data
    d <- rq_copy_to(
      db, 'd',
      data.frame(idx = 1:5,
                 x = c(1, NA, 3, 6, NA),
                 y = c(2, 4, NA, 5, NA)),
      temporary = FALSE,
      overwrite = TRUE)

    op1 <- d %.>%
      extend(., na_count %:=% ifelse(is.na(x), 1, 0) +
                   ifelse(is.na(y), 1, 0))
    txt1 <- format(op1)
    # cat(txt1)

    sql1 <- to_sql(op1, db)
    # cat(sql1)

    res1 <- DBI::dbGetQuery(db, sql1)
    res1 <- res1[order(res1$idx), , drop = FALSE]
    expect_equal(c(0, 1, 1, 0, 2), res1$na_count)

    op2 <- d %.>%
      extend(., mx %:=% pmax(x, y))
    txt2 <- format(op2)
    # cat(txt2)

    sql2 <- to_sql(op2, db)
    # cat(sql2)

    res2 <- DBI::dbGetQuery(db, sql2)
    res2 <- res2[order(res2$idx), , drop = FALSE]
    expect_equal(c(2, 4, 3, 6, NA), res2$mx)

    d2 <-  d <- rq_copy_to(
      db, 'd2',
      data.frame(idx = 1:3,
                 test = c(0, 5, 10)),
      temporary = FALSE,
      overwrite = TRUE)

    ifet <- d2 %.>%
      extend_se(.,
                c(qae(x = '',
                      y = ''),
                  if_else_block(
                    qe(test > 5),
                    thenexprs = qae(x = 'a',
                                    y = 'b'),
                    elseexprs = qae(x = 'b',
                                    y = 'a')
                  )))
    txt3 <- format(ifet)
    # cat(txt3)

    sql3 <- to_sql(ifet, db)
    # cat(sql3)
    res3 <- DBI::dbGetQuery(db, sql3)
    res3 <- res3[order(res3$idx), , drop = FALSE]
    expect_equal(c('b', 'b', 'a'), res3$x)
    expect_equal(c('a', 'a', 'b'), res3$y)

    DBI::dbDisconnect(db)
  }

  invisible(NULL)
}
