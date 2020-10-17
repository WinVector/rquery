
test_names <- function() {
  if (requireNamespace("RSQLite", quietly = TRUE) &&
      requireNamespace("DBI", quietly = TRUE)) {
    db <- DBI::dbConnect(RSQLite::SQLite(),
                         ":memory:")
    hdl <- rq_copy_to(db, "mtcars", mtcars,
                      overwrite = TRUE,
                      temporary = TRUE)

    # check values don't look unbound
    x <- "xval"
    optree <- hdl %.>%
      extend(.,
                 xcol = x)

    # check a few ops
    optree <- hdl %.>%
      project(.,
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
        project(.,
                    delay = avg(dep_time),
                    groupby = "mpg")
    )

    # detect failure to assign
    expect_error(
      badtree <- hdl %.>%
        extend(.,
                   mpg + 1)
    )

    # detect non-scalar constant
    v <- c(1, 2)
    expect_error(
      badtree <- hdl %.>%
        extend(.,
                   d2 = mpg + v)
    )

    # make sure LHS do not drift and columns come first
    d2 <- "zz"
    mpg <- "zz"
    p <- hdl %.>%
      extend(.,
                 d2 = mpg)
    expect_equal(sort(qc(am, carb, cyl, d2, disp, drat, gear, hp, mpg, qsec, vs, wt)),
                 sort(column_names(p)))
    DBI::dbDisconnect(db)
  }

  invisible(NULL)
}
