
# rquery 1.4.2 2020/01/17

 * Work on query generation and clean up db adaptor path.
 * Set minimum suggested rqdatatable version.

# rquery 1.4.1 2020/01/07

 * Add many values vignette.
 * Move to non-strict arrows.
 * Make drop columns not strict.
 * Better order documentation.

# rquery 1.4.0 2019/11/30

 * Fix name substitution in immediate mode.
 * Fix union_all.
 * Allow ad-hoc (not-bquote) early LHS eval.
 * Deal with bound variable types.
 * Deal with degenerate partition in extend.
 * Work on operator composition shortcuts.

# rquery 1.3.9 2019/10/26

 * Tune yaml path.
 * Fix project signatures.
 * Fold consecutive select_columns nodes.
 * Don't print quoted table name.
 * Add arrow example.
 * Add wrap/ex pattern.
 * Remove relop_list paths.
 * Fix drop_columns shortcut.

# rquery 1.3.8 2019/09/15

 * Add YAML path for operator trees.
 * Auto-register rqdatatable as default executor on load, if available.
 * Fix column name quoting issue in operator presentation.
 * Alternate operator names.

# rquery 1.3.7 2019/07/29

 * example_employee_date() now accepts a wrapped database handle.
 * move off legacy f_db signature.
 * adjust license.

# rquery 1.3.6 2019/07/04

 * Rename before join.
 * Allow empty specifications in more situations.

# rquery 1.3.5 2019/06/25

 * Remove str2lang() from vignette (method is not in older R)
 * Improve expression re-mapping (include n()).

# rquery 1.3.4 2019/06/14

 * Work on schema qualifications.
 * Allow empty project.
 * Better function re-mapping.
 
# rquery 1.3.3 2019/06/01

 * Clean up test_set_indicator() error message.

# rquery 1.3.2 2019/03/10

 * Don't override names() (messes up str()).
 * Test checks that are correct when rqdatatable is attached.
 * Add rquery substitution vingette.
 * Remove non-ascii chars from source file to fix CRAN warning

# rquery 1.3.1 2019/02/14

 * Fix dimnames().
 * Add more argument checks.
 * Fix temp-naming in joins.
 * More tests.

# rquery 1.3.0 2019/01/29

 * User controlled SQL-tree re-writing.
 * More per-DB SQL rendering control.
 * Move to tree-based parse representation (instead of flat).
 * relop_list collector interface.
 * Specialize materialze_node for lineage breaking.
 * %%/MOD().
 * Na row interfaces (un-publish tokenize_for_SQL()).
 * Add mutable scratch area on nodes.

# rquery 1.2.1 2018/12/16

 * versions without _nse() suffix.
 * parse ^ power.
 * reflexive arguments to non-sql nodes.
 * Add .[]-variable deceleration notation.
 * Narrow suggests.
 * update docs.
 * add order_expr.
 * declare : as an inline operator.
 * allow concatenating compatible pipelines.
 * check for table name consistency.
 * NULL/NA parsing.
 * Force order limit to be >= 0.

# rquery 1.2.0 2018/11/06

 * fix name lookup issue.
 * add bquote() abstraction to extend_nse(), project_nse(), and select_rows_nse().
 * fix column check on select_rows*
 * confirm DBI on tests.

# rquery 1.1.1 2018/10/26

 * Allow a bit more flexibility on ordering in extend.
 * Start breaking up some recursive calls.

# rquery 1.1.0 2018/09/20

 * Alternate data.table implementation path.
 * lookup_by_column().
 * Force parent.frame().

# rquery 1.0.0 2018/09/10

 * Fix key_inspector_postgresql quoting issue. 
 * More tests.
 * Export a default database description.
 * Check more on project and extend interfaces.

# rquery 0.6.2 2018/08/14

 * fix in-memory order_by!
 * aggregate aliases.
 * try to clean up immediate mode hooks a bit.

# rquery 0.6.1 2018/08/01

 * Change to immediate execution.
 * Substitute in values in presentation layer (rqdatatable depends on this).
 * Fix name disambiguation.
 * Better column dependency calculation.
 * Re-map function names.
 * Improve extend narrowing.
 * Start on schema qualification.
 * Get rid of %>>% and old dbi fn-names.
 * Try to improve use of regexps.
 * Add qlook().
 * Add affine_transform().
 * Documentation fixes.

# rquery 0.5.0 2018/06/18

 * Make DBI suggested.
 * Rename dbi_ prefixes to rq_ (for now have aliases from old to new).
 * Per-connection options.
 * Add expand_grid()/complete_design().
 * Get rid of embedded data cases and print-execution.
 * Move to new wrapr ops.
 * Rationalize names of table description methods.
 * Add rquery_executor hook.
 * Don't default to setting up a temporary RSQLite database.
 * Limit wrapr re-exports.
 * Add ability to use %:=% in parsing.
 * Move to executor options.
 * Separate execute() and materialize() roles.
 * Fix natural_join() semantics and make "by" a required argument.

# rquery 0.4.4 2018/05/14

 * Fix if_else NA treatment.
 * Fix orderby() limit bug.
 * Better detection of column types: dbi_coltypes().
 * Improve execute() performance in simple case.
 * materialize_node().
 * Minor documentation fixes.

# rquery 0.4.3 2018/05/07

 * Add assign_slice(), if_else_op(), map_column_values(), and set_indicator().
 * Bug fixes (esp on data.frame path).
 * Work on printing/formatting.
 * Improve column requirement checking.
 * Improve column ordering.
 * Make db_table() more prominent.
 * Documentation improvements.

# rquery 0.4.2 2018/04/05

 * Work around RPostgreSQL issue (dbTableExists does not work).
 * More controls on dbi_ steps through package options.
 * Push LIMIT deeper into pipelines.
 * Add connection config tools.

# rquery 0.4.1 2018/03/17

 * Fix re-run/re-create situations.
 * Pipe database connection.
 * Better quantile calc (assume window functions).
 * Improve dependency decls.
 * Further limit direct dependencies.
 * Work with more DB drivers.
 * SQL node for sets of columns.

# rquery 0.4.0 2018/03/10

 * Move to wrapr 1.2.0
 * Add wrapr_function.relop().
 * Column checks on extend.
 * Default sql_node to copying incoming columns.
 * NULL column ops.
 * rsummary().
 * "Non SQL" nodes.
 * New rquery_intro vignette.
 * Improved help examples.
 * Stricter argument checking.

# rquery 0.3.1 2018/02/10

 * Be compatible with both wrapr 1.1.1 and 1.2.0.
 
# rquery 0.3.0 2018/01/31
 
 * Fix "desc".
 * Minor format change in rename (anticipate wrapr 1.2.0).
 * Get ready for S3 wrapr_applicable and pipe_step (un-block wrapr 1.2.0 release).

# rquery 0.2.0 2017/01/22

 * Don't store DB reference.
 * Prepare for wrapr 1.1.0 features.
 * Ad-hoc query modes.
 * Fix statement partitioner.
 * More flexibility on assignment notation.
  
# rquery 0.1.0 2017/12/03

 * Initial experiment

