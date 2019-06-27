
test_rename <- function() {

  if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
    my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    d <- rq_copy_to(my_db, 'd',
                    data.frame(AUC = 0.6, R2 = 0.2, z = 3))
    op_tree <- rename_columns(d, c('R2' %:=% 'AUC', 'AUC' %:=% 'R2'))
    #cat(format(op_tree))
    sql <- to_sql(op_tree, my_db)
    #cat(sql)
    res <- execute(my_db, op_tree)
    DBI::dbDisconnect(my_db)
    expect <- wrapr::build_frame(
      "R2"  , "AUC", "z" |
        0.6 , 0.2  , 3   )
    RUnit::checkTrue(wrapr::check_equiv_frames(expect, res))
  }


  invisible(NULL)
}
