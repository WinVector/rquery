
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
    RUnit::checkEquals(qc(cyl, mpg),
                 sort(columns_used(optree)$mtcars))
    RUnit::checkEquals(qc(cyl, mean_mpg),
                 sort(column_names(optree)))
    d <- execute(db, optree)

    # detect unbound column reference
    RUnit::checkException(
      badtree <- hdl %.>%
        project(.,
                    delay = avg(dep_time),
                    groupby = "mpg")
    )

    # detect failure to assign
    RUnit::checkException(
      badtree <- hdl %.>%
        extend(.,
                   mpg + 1)
    )

    # detect non-scalar constant
    v <- c(1, 2)
    RUnit::checkException(
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
    RUnit::checkEquals(sort(qc(am, carb, cyl, d2, disp, drat, gear, hp, mpg, qsec, vs, wt)),
                 sort(column_names(p)))
    DBI::dbDisconnect(db)
  }

  invisible(NULL)
}
