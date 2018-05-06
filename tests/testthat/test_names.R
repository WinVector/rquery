library("rquery")
context("namedetection")



test_that("test_names: Works As Expected", {
  db <- DBI::dbConnect(RSQLite::SQLite(),
                       ":memory:")
  hdl <- dbi_copy_to(db, "mtcars", mtcars,
                     overwrite = TRUE,
                     temporary = TRUE)

  # check values don't look unbound
  x <- "xval"
  optree <- hdl %.>%
    extend_nse(.,
               xcol = x)

  # check a few ops
  optree <- hdl %.>%
    project_nse(.,
                mean_mpg = avg(mpg),
                groupby = "cyl")
  expect_equal(qc(cyl, mpg),
               sort(columns_used(optree)$mtcars))
  expect_equal(qc(cyl, mean_mpg),
               sort(column_names(optree)))
  d <- execute(db, optree)

  # detect unbound column reference
  expect_error(
    badtree <- hdl %.>%
      project_nse(.,
                  delay = avg(dep_time),
                  groupby = "mpg")
  )

})
