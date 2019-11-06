

# TODO: switch to wrapr::check_equiv_frames
rquery_check_equiv_frames <- function(d1, d2,
                               ...,
                               tolerance = sqrt(.Machine$double.eps)) {
  if( (!is.data.frame(d1)) != (!is.data.frame(d2)) ) {
    return(FALSE)
  }
  d1 <- data.frame(d1)
  d2 <- data.frame(d2)
  if((nrow(d1)!=nrow(d2)) || (ncol(d1)!=ncol(d2))) {
    return(FALSE)
  }
  cols <- sort(colnames(d1))
  c2 <- sort(colnames(d2))
  if(!isTRUE(all.equal(cols, c2))) {
    return(FALSE)
  }
  d1 <- d1[, cols, drop=FALSE]
  d1 <- d1[wrapr::orderv(d1), , drop=FALSE]
  rownames(d1) <- NULL
  d2 <- d2[, cols, drop=FALSE]
  d2 <- d2[wrapr::orderv(d2), , drop=FALSE]
  rownames(d2) <- NULL
  for(c in cols) {
    c1 <- d1[[c]]
    c2 <- d2[[c]]
    if(is.numeric(c1) != is.numeric(c2)) {
      return(FALSE)
    }
    if(is.numeric(c1)) {
      if(!isTRUE(all.equal(c1, c2, tolerance=tolerance))) {
        return(FALSE)
      }
    } else {
      if(!isTRUE(all.equal(c1, c2))) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}


test_e_example1 <- function() {
  # confir implementation can detect window functions
  d <- data.frame(
    'x_s'= c('s_03', 's_04', 's_02', 's_01', 's_03', 's_01'),
    'x_n'= c('n_13', 'n_48', 'n_77', 'n_29', 'n_91', 'n_93'),
    'y'= c(1.031, -1.337, -1.935, 1.277, -0.124, 0.306),
    stringsAsFactors = FALSE
  )
  table_desc = local_td(d, name = 'd')

  expect1 <- wrapr::build_frame(
    "x_s"   , "x_n" , "y"    , "y_mean"    |
      "s_03", "n_13", 1.031  , -0.1303333  |
      "s_04", "n_48", -1.337 , -0.1303333  |
      "s_02", "n_77", -1.935 , -0.1303333  |
      "s_01", "n_29", 1.277  , -0.1303333  |
      "s_03", "n_91", -0.124 , -0.1303333  |
      "s_01", "n_93", 0.306  , -0.1303333  )

  ops1 = table_desc %.>%
    extend(.,
           y_mean := mean(y))

  if(requireNamespace('rqdatatable', quietly = TRUE)) {
    # res1 <- d %.>% ops1
    res1 <- rqdatatable::ex_data_table(ops1, tables = list('d' = d))
    # RUnit::checkTrue(wrapr::check_equiv_frames(expect1, res1, tolerance = 1e-3))
    RUnit::checkTrue(rquery_check_equiv_frames(expect1, res1, tolerance = 1e-3))
  }

  if(requireNamespace('DBI', quietly = TRUE) &&
     requireNamespace('RSQLite', quietly = TRUE)) {

    raw_connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    RSQLite::initExtension(raw_connection)
    db <- rquery_db_info(
      connection = raw_connection,
      is_dbi = TRUE,
      connection_options = rq_connection_tests(raw_connection))

    rq_copy_to(db, 'd',
               d,
               temporary = TRUE,
               overwrite = TRUE)
    sql1 <- to_sql(ops1, db)
    res1db <- execute(db, ops1)

    # clean up
    DBI::dbDisconnect(raw_connection)

    # RUnit::checkTrue(wrapr::check_equiv_frames(expect1, res1db, tolerance = 1e-3))
    RUnit::checkTrue(rquery_check_equiv_frames(expect1, res1db, tolerance = 1e-3))
  }


  invisible(NULL)
}
