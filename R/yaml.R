

expr_map_to_expr_array <- function(emap) {
  r = vapply(names(emap), function(k) paste0(k, " %:=% ", emap[[k]]), character(1))
  names(r) <- NULL
  return(r)
}

#' Convert a series of simple objects (from YAML deserializaton) to an rquery pipeline.
#'
#' @param rep input objects
#' @param ... not used, force later arguments to bind by name
#' @param source input rquery node
#' @param env environment to evaluate in
#' @return rquery operator tree
#'
#' @export
#'
convert_yaml_to_pipeline <- function(rep, ..., source=NULL, env = parent.frame()) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::convert_yaml_to_pipeline")
  if(is.null(names(rep))) {
    if(!is.null(source)) {
      stop("Expected source to be NULL")
    }
    # unnamed list, a pipeline
    res <- convert_yaml_to_pipeline(rep[[1]], env = env)
    for(i in wrapr::seqi(2, length(rep))) {
      res <- convert_yaml_to_pipeline(rep[[i]], source = res, env = env)
    }
    return(res)
  } else {
    # named list, a stage
    op = rep$op
    if(op=="TableDescription") {
      if(!is.null(source)) {
        stop("Expected source to be NULL")
      }
      return(mk_td(table_name = rep$table_name,
                   columns = rep$column_names))
    }
    if(is.null(source)) {
      stop("Expected source to not be NULL")
    }
    if(op=="Extend") {
      partitionby = rep$partition_by
      if(is.numeric(partitionby)) {
        partitionby = 1L
      } else {
        partitionby = as.character(partitionby)
      }
      return(extend_se(source,
                       assignments = expr_map_to_expr_array(rep$ops),
                       partitionby = partitionby,
                       orderby = as.character(rep$order_by),
                       reverse = as.character(rep$reverse)))
    }
    if(op=="Project") {
      return(project_se(source,
                       assignments = expr_map_to_expr_array(rep$ops),
                       groupby = as.character(rep$group_by)))
    }
    if(op=="SelectRows") {
      return(select_rows_se(source,
                            expr = rep$expr))
    }
    if(op=="SelectColumns") {
      return(select_columns(source, columns=rep$columns))
    }
    if(op=="DropColumns") {
      return(drop_columns(source, drops = rep$column_deletions, strict = FALSE))
    }
    if(op=="Rename") {
      return(rename_columns(source, cmap=rep$column_remapping))  # TODO: see if map is dumped (or __repr__() call wrote a string)
    }
    if(op=="Order") {
      return(orderby(source, cols=rep$order_columns, reverse=rep$reverse, limit=rep$limit))
    }
    if(op=="NaturalJoin") {
      b = convert_yaml_to_pipeline(rep$b, env = env)
      return(natural_join(a = source, b = b,
                          by = rep$by, jointype = toupper(rep$jointype)))
    }
    stop("Unexpected node type: " + op)
  }
}


to_transport_representation_step <- function(ops) {
  if(is(ops, 'relop_table_source')) {
    return(list(op = 'TableDescription',
                table_name = ops$table_name,
                column_names = ops$columns))
  }
  if(is(ops, 'relop_extend')) {
    partition_by = ops$partitionby
    if(ops$windowed) {
      partition_by <- 1L
    }
    return(list(op = 'Extend',
                ops = ops$assignments,
                partition_by = partition_by,
                order_by = ops$orderby,
                reverse = ops$reverse))
  }
  if(is(ops, 'relop_project')) {
    return(list(op = 'Project',
                ops = ops$assignments,
                group_by = ops$groupby))
  }
  if(is(ops, 'relop_select_rows')) {
    return(list(op = 'SelectRows',
                expr = ops$parsed[[1]]$presentation))
  }
  if(is(ops, 'relop_select_columns')) {
    return(list(op = 'SelectColumns',
                columns = ops$columns))
  }
  # currently no drop_columns node in rquery, it is implemented as an appropriate select_columns()
  if(is(ops, 'relop_drop_columns')) {
    return(list(op = 'SelectColumns',
                columns = ops$columns))
  }
  if(is(ops, 'relop_rename_columns')) {
    return(list(op = 'Rename',
                column_remapping = ops$cmap))
  }
  if(is(ops, 'relop_orderby')) {
    return(list(op = 'Order',
                column_remapping = ops$cmap,
                order_columns = ops$orderby,
                reverse = ops$reverse))
  }
  if(is(ops, 'relop_natural_join')) {
    b <- to_transport_representation(ops$source[[2]])
    return(list(op = 'NaturalJoin',
                b = b,
                by = ops$by,
                jointype = ops$jointype))
  }
  stop(paste("Unexpected op class:", paste(as.character(class(ops)), collapse = ', ')))
}




#' Convert an rquery op diagram to a simple representation, appropriate for conversion to YAML.
#'
#' @param ops rquery operator dag
#' @param ... not used, force later arguments to be by name
#' @param convert_named_vectors_to_lists logical, if TRUE convert named vectors to lists
#' @return represenation structure
#'
#' @export
#'
to_transport_representation <- function(ops,
                                        ...,
                                        convert_named_vectors_to_lists = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::to_transport_representation")
  # linearize primary pipe direction
  steps = list(ops)
  while(length(ops$source) > 0) {
    ops = ops$source[[1]]
    steps = c(list(ops), steps)
  }
  # encode
  res <- lapply(steps, to_transport_representation_step)
  if(convert_named_vectors_to_lists) {
    convert_named_vectors_to_lists_f <- function(obj) {
      if(is.list(obj)) {
        return(lapply(obj, convert_named_vectors_to_lists_f)) # preserves names
      }
      if(is.vector(obj)) {
        if(length(names(obj))<=0) {
          return(obj)
        }
        return(as.list(obj))
      }
      return(obj)
    }
    res <- convert_named_vectors_to_lists_f(res)
  }
  return(res)
}

