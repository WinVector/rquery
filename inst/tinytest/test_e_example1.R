
test_e_example1 <- function() {
  # confirm implementation can detect window functions
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

  `:=` <- NULL  # don't look undefined
  y_mean <- NULL  # don't look undefined
  y <- NULL  # don't look undefined
  ops1 = table_desc %.>%
    extend(.,
           y_mean := mean(y),
           partitionby = 1)

  str1 <- format(ops1)
  str2 <- format(eval(str1))
  expect_equal(str1, str2)
  expect_equal(length(grep('partitionby\\s*=\\s*1', str1)), 1)

  if(requireNamespace('rqdatatable', quietly = TRUE)) {
    # res1 <- d %.>% ops1
    res1 <- rqdatatable::ex_data_table(ops1, tables = list('d' = d))
    # expect_true(wrapr::check_equiv_frames(expect1, res1, tolerance = 1e-3))
    expect_true(rquery:::rquery_check_equiv_frames(expect1, res1, tolerance = 1e-3))
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

    # expect_true(wrapr::check_equiv_frames(expect1, res1db, tolerance = 1e-3))
    expect_true(rquery:::rquery_check_equiv_frames(expect1, res1db, tolerance = 1e-3))
  }

  # yaml path
  xport <- to_transport_representation(ops1)
  ops_back <- convert_yaml_to_pipeline(xport)
  str3 <- format(ops_back)
  str3 <- gsub('%:=%', ':=', str3, fixed=TRUE)
  expect_equal(str1, str2)

  invisible(NULL)
}
