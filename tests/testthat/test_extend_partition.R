library("rquery")
context("partitioning")



test_that("test_extend_partition: Works As Expected", {

  my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  d <- table_source("d",
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
                  a_1 := 'treatment',
                  a_2 := 'control'),
                elseexprs =  qae(
                  a_1 := 'control',
                  a_2 := 'treatment')))
  txt <- format(dQ)
  expect_true(length(grep("!", txt, fixed = TRUE))>0)
  sql <- to_sql(dQ, my_db)
  expect_true(length(grep("e_2", sql, fixed = TRUE))>0)
  DBI::dbDisconnect(my_db)
})
