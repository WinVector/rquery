
library("rquery")
context("if_else")



test_that("test_extend_partition: Works As Expected", {

  d <- mk_td("d",
             c("a_1", "a_2", "a_3"  ))
  dQ <- d %.>%
    extend_se(., "a_4" %:=% "ifelse(! a_1, a_2, a_3)")
  txt <- format(dQ)
  expect_true(length(grep("!", txt, fixed = TRUE))>0)



  if (requireNamespace("RSQLite", quietly = TRUE) &&
      requireNamespace("DBI", quietly = TRUE)) {
    my_db <- DBI::dbConnect(RSQLite::SQLite(),
                            ":memory:")

    d <- data.frame(id = 1:3, x = c(0, 1, NA))
    # ifelse(d$x == 1, 'one', 'not one')

    d_remote <- rq_copy_to(my_db, "d_remote", d)

    optree <- d_remote %.>%
      extend(.,
                 res = ifelse(x == 1, 'one', 'not one')) %.>%
      orderby(., "id")

    res <- execute(my_db, optree)

    expect <- build_frame(
      "id", "x", "res"     |
        1L  , 0  , "not one" |
        2L  , 1  , "one"     |
        3L  , NA ,  NA       )

    expect_equal(expect, res)



    DBI::dbDisconnect(my_db)
  }

})
