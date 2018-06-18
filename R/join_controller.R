
#' build some example tables
#'
#' @param con db connection
#' @return example tables
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   example_employee_date(my_db)
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#
example_employee_date <- function(con) {
  . <- NULL # Declare not an unbound varaible
  # note: employeeanddate is likely built as a cross-product
  #       join of an employee table and set of dates of interest
  #       before getting to the join controller step.  We call
  #       such a table "row control" or "experimental design."
  keymap <- list()
  rq_execute(con, "
                 CREATE TABLE employeeanddate (
                 id TEXT,
                 date INTEGER
                 );
                 ")
  keymap[['employeeanddate']] = c()
  data.frame(id= c('i4', 'i4'),
             date = c(20140501, 20140601)) %.>%
    DBI::dbWriteTable(con, 'employeeanddate', value=., append=TRUE)
  rq_execute(con, "
                 CREATE TABLE orgtable (
                 eid TEXT,
                 date INTEGER,
                 dept TEXT,
                 location TEXT,
                 PRIMARY KEY (eid, date)
                 );
                 ")
  keymap[['orgtable']] = c('eid', 'date')
  data.frame(eid= c('i4', 'i4'),
             date = c(20140501, 20140601),
             dept = c('IT', 'SL'),
             location = c('CA', 'TX')) %.>%
    DBI::dbWriteTable(con, 'orgtable', value=., append=TRUE)
  rq_execute(con, "
                 CREATE TABLE revenue (
                 date INTEGER,
                 dept TEXT,
                 rev INTEGER,
                 PRIMARY KEY (date, dept)
                 );
                 ")
  keymap[['revenue']] = c('dept', 'date')
  data.frame(date = c(20140501, 20140601),
             dept = c('SL', 'SL'),
             rev = c(1000, 2000)) %.>%
    DBI::dbWriteTable(con, 'revenue', value=., append=TRUE)
  rq_execute(con, "
                 CREATE TABLE activity (
                 eid TEXT,
                 date INTEGER,
                 hours INTEGER,
                 location TEXT,
                 PRIMARY KEY (eid, date)
                 );
                 ")
  keymap[['activity']] = c('eid', 'date')
  data.frame(eid= c('i4', 'i4'),
             date = c(20140501, 20140601),
             hours = c(50, 3),
             location = c('office', 'client')) %.>%
    DBI::dbWriteTable(con, 'activity', value=., append=TRUE)
  tableNames <- c('employeeanddate',
                  'revenue',
                  'activity',
                  'orgtable')
  key_inspector_by_name <- function(db, tablename) {
    keys <- keymap[[tablename]]
    if(length(keys)<=0) {
      return(character(0))
    }
    names(keys) <- keys
    keys
  }
  describe_tables(con,
                   tableNames,
                   keyInspector = key_inspector_by_name)
}

uniqueInOrder <- function(names) {
  names[!duplicated(names)]
}

makeTableIndMap <- function(tableNameSeq) {
  tableNameSeq <- uniqueInOrder(tableNameSeq)
  tableIndColNames <- paste('table',
                            gsub("[^a-zA-Z0-9]+", '_', tableNameSeq),
                            'present', sep= '_')
  names(tableIndColNames) <- tableNameSeq
  tableIndColNames
}


#' Return all columns as guess at preferred primary keys.
#'
#' @seealso \code{describe_tables}
#'
#' @param db database handle
#' @param tablename character, name of table
#' @return map of keys to keys
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbWriteTable(my_db,
#'                     "d",
#'                     data.frame(x=1:3, y=NA))
#'   print(key_inspector_all_cols(my_db, "d"))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
key_inspector_all_cols <- function(db, tablename) {
  sample <- rq_get_query(db,
                            paste("SELECT * FROM",
                                  quote_identifier(db, tablename),
                                  "LIMIT 1"))
  cols <- colnames(sample)
  keys <- cols
  names(keys) <- keys
  keys
}


#' Return all primary key columns as guess at preferred primary keys for a SQLite handle.
#'
#' @seealso \code{describe_tables}
#'
#' @param db database handle
#' @param tablename character, name of table
#' @return map of keys to keys
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbExecute(my_db, "
#'     CREATE TABLE orgtable (
#'       eid TEXT,
#'       date INTEGER,
#'       dept TEXT,
#'       location TEXT,
#'     PRIMARY KEY (eid, date)
#'     )
#'     ")
#'   print(key_inspector_sqlite(my_db, "orgtable"))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
key_inspector_sqlite <- function(db, tablename) {
  tabInfo <- rq_get_query(db,
                             paste0("pragma table_info(",
                                    quote_identifier(db, tablename),
                                    ")"))
  keys <- NULL
  if((!is.null(tabInfo))&&(nrow(tabInfo)>0)) {
    keys <- tabInfo$name[tabInfo$pk>0]
    names(keys) <- keys
  }
  keys
}


#' Return all primary key columns as guess at preferred primary keys for a PostgreSQL handle.
#'
#' @seealso \code{describe_tables}
#'
#' @param db database handle
#' @param tablename character, name of table
#' @return map of keys to keys
#'
#'
#' @export
#'
key_inspector_postgresql <- function(db, tablename) {
  # from https://wiki.postgresql.org/wiki/Retrieve_primary_key_columns
  q <- paste0(
    "
    SELECT a.attname, format_type(a.atttypid, a.atttypmod) AS data_type
    FROM   pg_index i
    JOIN   pg_attribute a ON a.attrelid = i.indrelid
    AND a.attnum = ANY(i.indkey)
    WHERE  i.indrelid = ", quote_identifier(db, tablename), "::regclass
    AND    i.indisprimary;
    "
  )
  tabInfo <- rq_get_query(db, q)
  keys <- NULL
  if((!is.null(tabInfo))&&(nrow(tabInfo)>0)) {
    keys <- tabInfo$attname
    names(keys) <- keys
  }
  keys
}



#' Build a nice description of a table.
#'
#' Please see \url{http://www.win-vector.com/blog/2017/05/managing-spark-data-handles-in-r/} for details.
#' Note: one usually needs to alter the keys column which is just populated with all columns.
#'
#' Please see \code{vignette('DependencySorting', package = 'rquery')} and \code{vignette('joinController', package= 'rquery')} for more details.
#'
#' @seealso \code{\link{build_join_plan}}, \code{\link{graph_join_plan}}, \code{\link{actualize_join_plan}}
#'
#' @param db database handle
#' @param tablenames character, names of tables to describe.
#' @param ... force later arguments to bind by name.
#' @param keyInspector function that determines prefered primary key set for tables.
#' @return table describing the data.
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   ex <- example_employee_date(my_db)
#'   print(describe_tables(my_db, ex$tableName,
#'                          keyInspector = key_inspector_sqlite))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#'
#' @export
#'
describe_tables <- function(db,
                             tablenames,
                             ...,
                             keyInspector = key_inspector_all_cols) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::describe_tables")
  reslist <- vector(mode = "list", length = length(tablenames))
  for(ii in seq_len(length(tablenames))) {
    tablename = tablenames[[ii]]
    sample <- rq_coltypes(db, tablename)
    cols <- colnames(sample)
    # may not get classes on empty tables
    # https://github.com/tidyverse/dplyr/issues/2913
    classes <- vapply(cols,
                      function(si) {
                        paste(class(sample[[si]]),
                              collapse=', ')
                      }, character(1))
    keys <- keyInspector(db, tablename)
    tableIndColNames <- makeTableIndMap(tablename)
    if(length(intersect(tableIndColNames, cols))>0) {
      warning("rquery::describe_tables table_CLEANEDTABNAME_present column may cause problems (please consider renaming before these steps)")
    }
    res <-
      data.frame(tableName= tablename,
                 isEmpty= nrow(sample)<=0,
                 indicatorColumn= tableIndColNames[[1]],
                 stringsAsFactors = FALSE)
    res$columns <- list(cols)
    res$keys <- list(keys)
    res$colClass <- list(classes)
    reslist[[ii]] <- res
  }
  if(length(reslist)<=1) {
    reslist[[1]]
  } else {
    do.call(rbind, reslist)
  }
}





# type unstable: return data.frame if okay, character if problem
inspect_and_limit_join_plan <- function(columnJoinPlan, checkColClasses) {
  # sanity check
  for(ci in c('tableName', 'sourceColumn', 'sourceClass', 'resultColumn')) {
    if(is.null(columnJoinPlan[[ci]])) {
      return(paste('columnJoinPlan column', ci, 'not present'))
    }
    if(!is.character(columnJoinPlan[[ci]])) {
      return(paste('columnJoinPlan column', ci, 'should be of type character'))
    }
    if(any(nchar(columnJoinPlan[[ci]])<=0) ||
       any(is.na(columnJoinPlan))) {
      return(paste("empty or NA', ci, ' column in columnJoinPlan"))
    }
  }
  for(ci in c('isKey','want')) {
    if(is.null(columnJoinPlan[[ci]])) {
      return(paste('columnJoinPlan column', ci, 'not present'))
    }
    if(!is.logical(columnJoinPlan[[ci]])) {
      return(paste('columnJoinPlan column', ci, 'should be of type logical'))
    }
    if(any(is.na(columnJoinPlan))) {
      return(paste("NA', ci, ' colum in columnJoinPlan"))
    }
  }
  if(any(columnJoinPlan$isKey & (!columnJoinPlan$want))) {
    return("any row marked isKey must also be marked want")
  }
  valCols <- columnJoinPlan$resultColumn[!columnJoinPlan$isKey]
  if(length(valCols) !=
     length(unique(valCols))) {
    return("columnJoinPlan result columns must be unique")
  }
  tabs <-  uniqueInOrder(columnJoinPlan$tableName)
  for(tabnam in tabs) {
    ci <- columnJoinPlan[columnJoinPlan$tableName==tabnam, , drop=FALSE]
    if(length(ci$sourceColumn) !=
       length(unique(ci$sourceColumn))) {
      return(paste("columnJoinPlan sourceColumns not unique for table",
                   ci))
    }
    if((sum(ci$isKey)<=0) && (tabnam!=tabs[[1]])) {
      return("no keys for table", tabnam)
    }
  }
  tableIndColNames <- makeTableIndMap(columnJoinPlan$tableName)
  tabNOverlap <- intersect(tableIndColNames,
                           c(columnJoinPlan$resultColumn, columnJoinPlan$sourceColumn))
  if(length(tabNOverlap)>0) {
    return(paste("column source or result names intersect table present columns:",
                 paste(tabNOverlap, collapse = ', ')))

  }
  # limit down to things we are using
  columnJoinPlan <- columnJoinPlan[columnJoinPlan$want, , drop=FALSE]
  # check a few desired invariants of the plan
  columnJoinPlan$joinSource <- ''
  prevResultColInfo <- list()
  for(tabnam in tabs) {
    ci <- columnJoinPlan[columnJoinPlan$tableName==tabnam, , drop=FALSE]
    cMap <- ci$sourceClass
    names(cMap) <- ci$resultColumn
    keyCols <- ci$resultColumn[ci$isKey]
    if(tabnam!=tabs[[1]]) {
      if(length(keyCols)<=0) {
        return(paste("table", tabnam, "declares no keys"))
      }
    }
    resCols <- ci$resultColumn[ci$want]
    if(length(prevResultColInfo)>0) {
      missedKeys <- setdiff(keyCols, names(prevResultColInfo))
      if(length(missedKeys)>0) {
        return(paste("key col(s) (",
                     paste(missedKeys, collapse = ', '),
                     ") not contained in result cols of previous table(s) for table:", tabnam))
      }
      for(ki in keyCols) {
        prevInfo <- prevResultColInfo[[ki]]
        #print(paste(prevInfo$tabableName, ki, '->', tabnam, ki))
        columnJoinPlan$joinSource[(columnJoinPlan$tableName==tabnam) &
                                    (columnJoinPlan$resultColumn==ki)] <- prevInfo$tabableName
      }
    }
    for(ki in resCols) {
      prevInfo <- prevResultColInfo[[ki]]
      curClass <- cMap[[ki]]
      if((checkColClasses)&&(!is.null(prevInfo))&&
         (curClass!=prevInfo$clsname)) {
        return(paste("column",ki,"changed from",
                     prevInfo$clsname,"to",curClass,"at table",
                     tabnam))

      }
      if(is.null(prevInfo)) {
        prevResultColInfo[[ki]] <- list(clsname= curClass,
                                        tabableName= tabnam)
      }
    }
  }
  columnJoinPlan
}


#' Topologically sort join plan so values are available before uses.
#'
#' Depends on \code{igraph} package.
#' Please see \code{vignette('DependencySorting', package = 'rquery')} and \code{vignette('joinController', package= 'rquery')} for more details.
#'
#' @param columnJoinPlan join plan
#' @param leftTableName which table is left
#' @param ... force later arguments to bind by name
#' @return list with dependencyGraph and sorted columnJoinPlan
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE) &&
#'     requireNamespace('igraph', quietly = TRUE)) {
#'   # note: employeeanddate is likely built as a cross-product
#'   #       join of an employee table and set of dates of interest
#'   #       before getting to the join controller step.  We call
#'   #       such a table "row control" or "experimental design."
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   RSQLite::initExtension(my_db)
#'   tDesc <- example_employee_date(my_db)
#'   columnJoinPlan <- build_join_plan(tDesc, check= FALSE)
#'   # unify keys
#'   columnJoinPlan$resultColumn[columnJoinPlan$resultColumn=='id'] <- 'eid'
#'   # look at plan defects
#'   print(paste('problems:',
#'               inspect_join_plan(tDesc, columnJoinPlan)))
#'   # fix plan
#'   sorted <- topo_sort_tables(columnJoinPlan, 'employeeanddate')
#'   print(paste('problems:',
#'               inspect_join_plan(tDesc, sorted$columnJoinPlan)))
#'   print(plot(sorted$dependencyGraph))
#'   DBI::dbDisconnect(my_db)
#'   my_db <- NULL
#' }
#'
#' @export
#'
topo_sort_tables <- function(columnJoinPlan, leftTableName,
                           ...) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::topo_sort_tables")
  if(!requireNamespace('igraph', quietly = TRUE)) {
    stop("rquery::topo_sort_tables: requres igraph to sort tables")
  }
  g <- igraph::make_empty_graph()
  vnams <- sort(unique(columnJoinPlan$tableName))
  for(vi in vnams) {
    g <- g + igraph::vertex(vi)
  }
  # left table is special, prior to all
  for(vi in setdiff(vnams, leftTableName)) {
    g <- g + igraph::edge(leftTableName, vi)
  }
  # add in any other order conditions
  n <- length(vnams)
  for(vii in seq_len(n)) {
    if(vnams[[vii]]!=leftTableName) {
      ci <- columnJoinPlan[columnJoinPlan$tableName==vnams[[vii]], ,
                           drop=FALSE]
      knownI <- ci$resultColumn[!ci$isKey]
      for(vjj in setdiff(seq_len(n), vii)) {
        if(vnams[[vjj]]!=leftTableName) {
          cj <- columnJoinPlan[columnJoinPlan$tableName==vnams[[vjj]], ,
                               drop=FALSE]
          keysJ <- cj$resultColumn[cj$isKey]
          if(length(intersect(knownI, keysJ))>0) {
            g <- g + igraph::edge(vnams[[vii]], vnams[[vjj]])
          }
        }
      }
    }
  }
  tableOrder <- vnams[as.numeric(igraph::topo_sort(g))]
  tabs <- split(columnJoinPlan, columnJoinPlan$tableName)
  tabs <- tabs[tableOrder]
  tabs <- do.call(rbind, tabs)
  list(columnJoinPlan= tabs,
       dependencyGraph= g,
       tableOrder= tableOrder)
}

#' Build a drawable specification of the join diagram
#'
#' Please see \code{vignette('DependencySorting', package = 'rquery')} and \code{vignette('joinController', package= 'rquery')} for more details.
#'
#' @seealso \code{\link{describe_tables}}, \code{\link{build_join_plan}}, \code{\link{actualize_join_plan}}
#'
#' @param columnJoinPlan join plan
#' @param ... force later arguments to bind by name
#' @param groupByKeys logical if true build key-equivalent sub-graphs
#' @param graphOpts options for graphViz
#' @return grViz diagram spec
#'
#' @examples
#'
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   # note: employeeanddate is likely built as a cross-product
#'   #       join of an employee table and set of dates of interest
#'   #       before getting to the join controller step.  We call
#'   #       such a table "row control" or "experimental design."
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   RSQLite::initExtension(my_db)
#'   tDesc <- example_employee_date(my_db)
#'   # fix order by hand, please see rquery::topo_sort_tables for
#'   # how to automate this.
#'   ord <- match(c('employeeanddate', 'orgtable', 'activity', 'revenue'),
#'                tDesc$tableName)
#'   tDesc <- tDesc[ord, , drop=FALSE]
#'   columnJoinPlan <- build_join_plan(tDesc, check= FALSE)
#'   # unify keys
#'   columnJoinPlan$resultColumn[columnJoinPlan$resultColumn=='id'] <- 'eid'
#'   # look at plan defects
#'   print(paste('problems:',
#'               inspect_join_plan(tDesc, columnJoinPlan)))
#'   diagramSpec <- graph_join_plan(columnJoinPlan)
#'   # to render as JavaScript:
#'   if(requireNamespace("DiagrammeR", quietly = TRUE)) {
#'     print(DiagrammeR::grViz(diagramSpec))
#'   }
#'   DBI::dbDisconnect(my_db)
#'   my_db <- NULL
#' }
#'
#' @export
#'
#'
graph_join_plan <- function(columnJoinPlan, ...,
                                groupByKeys= TRUE,
                                graphOpts= NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::graph_join_plan")
  columnJoinPlan <- inspect_and_limit_join_plan(columnJoinPlan, FALSE)
  if(is.character(columnJoinPlan)) {
    stop(columnJoinPlan)
  }
  if(is.null(graphOpts)) {
    graphOpts <- paste(" graph [",
                       "layout = dot, rankdir = LR, overlap = prism,",
                       "compound = true, nodesep = .5, ranksep = .25]\n",
                       " edge [decorate = true, arrowhead = dot]\n",
                       " node [style=filled, fillcolor=lightgrey]\n")
  }
  tabs <- uniqueInOrder(columnJoinPlan$tableName)
  tabIndexes <- seq_len(length(tabs))
  names(tabIndexes) <- tabs
  keysToGroups <- list()
  graph <- paste0("digraph joinplan {\n ", graphOpts, "\n")
  # pass 1: define nodes and groups of nodes
  for(idx in seq_len(length(tabs))) {
    ti <- tabs[[idx]]
    ci <- columnJoinPlan[columnJoinPlan$tableName==ti, ,
                         drop=FALSE]
    keys <- paste('{',
                  paste(sort(ci$resultColumn[ci$isKey]),
                        collapse = ', '),
                  '}')
    if(nchar(keys)<=0) {
      keys <- '.' # can't use '' as a list key
    }
    keysToGroups[[keys]] <- c(keysToGroups[[keys]], idx)
    sourceAnnotations <- paste(' (', ci$sourceColumn, ')')
    ind <- NULL
    if(idx>1) {
      ind <- paste('i:', makeTableIndMap(ti)[[1]])
    }
    cols <- paste0(ifelse(ci$isKey, 'k: ', 'v: '),
                   ci$resultColumn,
                   ifelse(ci$resultColumn==ci$sourceColumn,
                          '', sourceAnnotations))
    cols <- paste(c(ind, cols), collapse ='\\l')
    ndi <- paste0(idx, ': ', ti, '\n\\l', cols)
    shape = 'tab'
    if(idx<=1) {
      shape = 'folder'
    }
    graph <- paste0(graph, "\n  ",
                    'node', idx,
                    " [ shape = '", shape, "' , label = '", ndi, "\\l']\n"
    )
  }
  # pass 2: edges
  columnJoinPlanK <- columnJoinPlan[columnJoinPlan$isKey, ,
                                    drop=FALSE]
  for(tii in seq_len(length(tabs))) {
    ti <- tabs[[tii]]
    ci <- columnJoinPlanK[columnJoinPlanK$tableName==ti &
                            nchar(columnJoinPlanK$joinSource)>0, ,
                          drop=FALSE]
    sources <- sort(unique(ci$joinSource))
    for(si in sources) {
      sii <- tabIndexes[[si]]
      ki <- paste(ci$resultColumn[ci$joinSource==si],
                  collapse = '\\l')
      graph <- paste0(graph, "\n",
                      " node", sii, " -> ", "node", tii,
                      " [ label='", ki, "\\l' ]")
    }
  }
  if(groupByKeys) {
    # assign subgraphs
    for(gii in seq_len(length(names(keysToGroups)))) {
      gi <- names(keysToGroups)[[gii]]
      group <- keysToGroups[[gi]]
      if(length(group)>0) {
        group <- paste0('node', group)
        graph <- paste0(graph, '\n',
                        'subgraph cluster_', gii, ' {\n',
                        'label = "',gi,'"\n',
                        paste(group, collapse=' ; '),
                        '\n}')
      }
    }
  }
  graph <- paste0(graph, '\n', '}\n')
  graph
}







#' check that a join plan is consistent with table descriptions
#'
#' Please see \code{vignette('DependencySorting', package = 'rquery')} and \code{vignette('joinController', package= 'rquery')} for more details.
#' @seealso \code{\link{describe_tables}}, \code{\link{build_join_plan}}, \code{\link{graph_join_plan}}, \code{\link{actualize_join_plan}}
#'
#' @param tDesc description of tables, from \code{\link{describe_tables}} (and likely altered by user).
#' @param columnJoinPlan columns to join, from \code{\link{build_join_plan}} (and likely altered by user). Note: no column names must intersect with names of the form \code{table_CLEANEDTABNAME_present}.
#' @param ... force later arguments to bind by name.
#' @param checkColClasses logical if true check for exact class name matches
#' @return NULL if okay, else a string
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   # example data
#'   DBI::dbWriteTable(my_db,
#'                     "d1",
#'                     data.frame(id= 1:3,
#'                                weight= c(200, 140, 98),
#'                                height= c(60, 24, 12)))
#'   DBI::dbWriteTable(my_db,
#'                     "d2",
#'                     data.frame(pid= 2:3,
#'                                weight= c(130, 110),
#'                                width= 1))
#'   # get the initial description of table defs
#'   tDesc <- describe_tables(my_db, qc(d1, d2))
#'   # declare keys (and give them consistent names)
#'   tDesc$keys[[1]] <- list(PrimaryKey= 'id')
#'   tDesc$keys[[2]] <- list(PrimaryKey= 'pid')
#'   # build the join plan
#'   columnJoinPlan <- build_join_plan(tDesc)
#'   # confirm the plan
#'   print(inspect_join_plan(tDesc, columnJoinPlan,
#'                           checkColClasses= TRUE))
#'   # damage the plan
#'   columnJoinPlan$sourceColumn[columnJoinPlan$sourceColumn=='width'] <- 'wd'
#'   # find a problem
#'   print(inspect_join_plan(tDesc, columnJoinPlan,
#'                           checkColClasses= TRUE))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
inspect_join_plan <- function(tDesc, columnJoinPlan,
                                    ...,
                                    checkColClasses= FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::inspect_join_plan")
  columnJoinPlan <- inspect_and_limit_join_plan(columnJoinPlan,
                                            checkColClasses=checkColClasses)
  if(is.character(columnJoinPlan)) {
    return(columnJoinPlan)
  }
  # sanity check
  if(length(unique(tDesc$tableName)) != length(tDesc$tableName)) {
    return("non-unique table names in tDesc")
  }
  # limit down to things we are using
  tabsC <- unique(columnJoinPlan$tableName)
  if(length(setdiff(tabsC, tDesc$tableName))>0) {
    return("tDesc does not have all the needed tables to join")
  }
  tDesc <- tDesc[tDesc$tableName %in% tabsC, , drop=FALSE]
  if(nrow(tDesc)<=0) {
    return("no tables selected")
  }
  tabsD <- unique(tDesc$tableName)
  columnJoinPlan <- columnJoinPlan[columnJoinPlan$tableName %in% tabsD, ,
                                   drop=FALSE]
  # check a few desired invariants of the plan
  for(i in seq_len(nrow(tDesc))) {
    tnam <- tDesc$tableName[[i]]
    ci <- columnJoinPlan[columnJoinPlan$tableName==tnam, , drop=FALSE]
    # don't check tDesc$keys here, as it isn't used after join plan is constructed.
    if(!all(ci$sourceColumn %in% tDesc$columns[[i]])) {
      probs <- paste(setdiff(ci$sourceColumn, tDesc$columns[[i]]),
                     collapse = ', ')
      return(paste("table description",
                   tnam, "refers to non-column(s):",probs))
    }
  }
  return(NULL) # okay!
}



#' Build a join plan
#'
#' Please see \code{vignette('DependencySorting', package = 'rquery')} and \code{vignette('joinController', package= 'rquery')} for more details.
#' @seealso \code{\link{describe_tables}}, \code{\link{inspect_join_plan}}, \code{\link{graph_join_plan}}, \code{\link{actualize_join_plan}}
#'
#' @param tDesc description of tables from \code{\link{describe_tables}} (and likely altered by user). Note: no column names must intersect with names of the form \code{table_CLEANEDTABNAME_present}.
#' @param ... force later arguments to bind by name.
#' @param check logical, if TRUE check the join plan for consistnecy.
#' @return detailed column join plan (appropriate for editing)
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   d <- data.frame(id=1:3, weight= c(200, 140, 98))
#'   DBI::dbWriteTable(my_db,"d1", d)
#'   DBI::dbWriteTable(my_db,"d2", d)
#'   tDesc <- describe_tables(my_db, c("d1", "d2"))
#'   tDesc$keys[[1]] <- list(PrimaryKey= 'id')
#'   tDesc$keys[[2]] <- list(PrimaryKey= 'id')
#'   print(build_join_plan(tDesc))
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
build_join_plan <- function(tDesc,
                          ...,
                          check= TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::build_join_plan")
  count <- NULL # declare not an unbound ref
  ntab <- nrow(tDesc)
  if(length(unique(tDesc$tableName))!=ntab) {
    stop("rquery::build_join_plan must have unique table name(s)")
  }
  if(any(nchar(tDesc$tableName)<=0)) {
    stop("rquery::build_join_plan empty table name(s)")
  }
  plans <- vector(ntab, mode='list')
  for(i in seq_len(ntab)) {
    cols <- tDesc$columns[[i]]
    keys <- tDesc$keys[[i]]
    tnam <- tDesc$tableName[[i]]
    classes <- tDesc$colClass[[i]]
    if(length(cols)<=0) {
      stop(paste("rquery::build_join_plan table",
                 tnam, "no columns"))
    }
    if((length(keys)<=0)&&(i>1)) {
      stop(paste("rquery::build_join_plan table",
                 tnam, "no keys"))
    }
    if(any(nchar(keys)<=0)) {
      stop(paste("rquery::build_join_plan table",
                 tnam, "empty key columns"))
    }
    if(length(unique(keys))!=length(keys)) {
      stop(paste("rquery::build_join_plan table",
                 tnam, "declares duplicate key columns"))
    }
    if(any(nchar(names(keys))<=0)) {
      stop(paste("rquery::build_join_plan table",
                 tnam, "empty key mappings"))
    }
    if(length(unique(names(keys)))!=length(names(keys))) {
      stop(paste("rquery::build_join_plan table",
                 tnam, "declares duplicate key mappings"))
    }
    if(!all(keys %in% cols)) {
      stop(paste("rquery::build_join_plan table",
                 tnam, "declares a key that is not a column"))
    }
    isKey <- rep(FALSE, length(cols))
    keyIndexes <- match(keys, cols)
    isKey[keyIndexes] <- TRUE
    resultColumn= cols
    resultColumn[keyIndexes] <- names(keys)
    pi <- data.frame(tableName= tnam,
                     sourceColumn= cols,
                     sourceClass= classes,
                     resultColumn= resultColumn,
                     isKey= isKey,
                     want= TRUE,
                     stringsAsFactors = FALSE)
    plans[[i]] <- pi
  }
  plans <- do.call(rbind, plans)
  # disambiguate non-key result columns
  dups <- plans$resultColumn[!plans$isKey]
  dups <- sort(unique(dups[duplicated(dups)]))
  if(length(dups)>0) {
    for(ci in dups) {
      indices <- which(plans$resultColumn==ci)
      for(i in indices) {
        ti <- gsub("[^a-zA-Z0-9]+", '_', plans$tableName[[i]])
        rc <- paste(ti, ci, sep= '_')
        plans$resultColumn[[i]] <- rc
      }
    }
  }
  # catch any remaining duplication
  nonKeyIndexes <- which(!plans$isKey)
  plans$resultColumn[nonKeyIndexes] <- make.unique( plans$resultColumn[nonKeyIndexes],
                                                    sep= '_')
  if(check) {
    # just in case
    problem <- inspect_join_plan(tDesc, plans)
    if(!is.null(problem)) {
      stop(paste("rquery::build_join_plan produced plan issue:",
                 problem))
    }
  }
  plans
}


charArrayToString <- function(v) {
  vl <- vapply(v,
               function(vi) {
                 paste0("'", vi, "'")
               },
               character(1))
  vs <- paste(vl, collapse= ', ')
  paste('c(', vs, ')')
}

strMapToString <- function(m) {
  vl <- vapply(names(m),
               function(ni) {
                 paste0("'", ni, "'='",m[[ni]],"'")
               },
               character(1))
  vs <- paste(vl, collapse= ', ')
  paste('c(', vs, ')')
}


#' Execute an ordered sequence of left joins.
#'
#' Please see \code{vignette('DependencySorting', package = 'rquery')} and \code{vignette('joinController', package= 'rquery')} for more details.
#' @seealso \code{\link{describe_tables}}, \code{\link{build_join_plan}}, \code{\link{inspect_join_plan}}, \code{\link{graph_join_plan}}
#'
#'
#' @param columnJoinPlan columns to join, from \code{\link{build_join_plan}} (and likely altered by user).  Note: no column names must intersect with names of the form \code{table_CLEANEDTABNAME_present}.
#' @param ... force later arguments to bind by name.
#' @param add_ind_cols logical, if TRUE add indicators showing which tables supplied rows.
#' @param jointype character, type of join to perform ("LEFT", "INNER", "RIGHT", ...).
#' @param checkColClasses logical if true check for exact class name matches
#' @return join optree
#'
#' @examples
#'
#' if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("RSQLite", quietly = TRUE)) {
#'   my_db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   # example data
#'   DBI::dbWriteTable(my_db,
#'                     "meas1",
#'                     data.frame(id= c(1,2),
#'                                weight= c(200, 120),
#'                                height= c(60, 14)))
#'   DBI::dbWriteTable(my_db,
#'                     "meas2",
#'                     data.frame(pid= c(2,3),
#'                                weight= c(105, 110),
#'                                width= 1))
#'   # get the initial description of table defs
#'   tDesc <- describe_tables(my_db, qc(meas1, meas2))
#'   # declare keys (and give them consitent names)
#'   tDesc$keys[[1]] <- list(PatientID= 'id')
#'   tDesc$keys[[2]] <- list(PatientID= 'pid')
#'   # build the column join plan
#'   columnJoinPlan <- build_join_plan(tDesc)
#'   # decide we don't want the width column
#'   columnJoinPlan$want[columnJoinPlan$resultColumn=='width'] <- FALSE
#'   # double check our plan
#'   if(!is.null(inspect_join_plan(tDesc, columnJoinPlan,
#'                                 checkColClasses= TRUE))) {
#'     stop("bad join plan")
#'   }
#'   # actualize as left join op_tree
#'   optree <- actualize_join_plan(columnJoinPlan,
#'                                 checkColClasses= TRUE)
#'   cat(format(optree))
#'   print(execute(my_db, optree))
#'   if(requireNamespace("DiagrammeR", quietly = TRUE)) {
#'     DiagrammeR::grViz(op_diagram(optree))
#'   }
#'   DBI::dbDisconnect(my_db)
#' }
#'
#' @export
#'
#'
actualize_join_plan <- function(columnJoinPlan,
                                ...,
                                jointype = "LEFT",
                                add_ind_cols = FALSE,
                                checkColClasses= FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::actualize_join_plan")
  # sanity check (if there is an obvious config problem fail before doing potentially expensive work)
  columnJoinPlan <- inspect_and_limit_join_plan(columnJoinPlan,
                                                checkColClasses=checkColClasses)
  if(is.character(columnJoinPlan)) {
    stop(paste("rquery::actualize_join_plan", columnJoinPlan))
  }
  # get the names of tables in columnJoinPlan order
  tableNameSeq <- uniqueInOrder(columnJoinPlan$tableName)
  tableIndColNames <- makeTableIndMap(tableNameSeq)
  if(length(intersect(tableIndColNames,
                      c(columnJoinPlan$resultColumn, columnJoinPlan$sourceColumn)))>0) {
    stop("actualize_join_plan: column mappings intersect intended table label columns")
  }
  # build pipeline
  res <- NULL
  for(tabnam in tableNameSeq) {
    rows <- columnJoinPlan[columnJoinPlan$tableName==tabnam, , drop= FALSE]
    si <- mk_td(tabnam, rows$sourceColumn)
    if(!isTRUE(all.equal(rows$sourceColumn, rows$resultColumn))) {
      si <- rename_columns(si,  rows$resultColumn %:=% rows$sourceColumn)
    }
    if(add_ind_cols) {
      indcol <-  paste0(tabnam, "_present")
      si <- extend_se(si, indcol %:=% 1)
    }
    if(is.null(res)) {
      res <- si
    } else {
      joinby = rows$resultColumn[rows$isKey]
      res <- natural_join(res, si,
                          jointype = jointype,
                          by = joinby)
    }
  }
  if(add_ind_cols) {
    indcols <-  paste0(tableNameSeq, "_present")
    res <- null_replace(res, indcols, 0)
  }
  res
}

