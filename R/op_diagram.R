

label_nodes <- function(obj,
                        ...,
                        label_key = "node_id", next_obj_id = 1) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::label_nodes")
  force(label_key)
  clear_node_labels <- function(obj) {
    assign(label_key, NULL, envir = obj$mutable_annotation_space)
    lapply(obj$source, clear_node_labels)
    NULL
  }
  clear_node_labels(obj)
  counter <- new.env(parent = emptyenv())
  assign(label_key, next_obj_id, envir = counter)
  set_node_labels <- function(obj) {
    see <- get(label_key, envir = obj$mutable_annotation_space)
    if(is.null(see)) {
      id <- get(label_key, envir = counter)
      assign(label_key, id + 1, envir = counter)
      assign(label_key, id, envir = obj$mutable_annotation_space)
      lapply(obj$source, set_node_labels)
    }
    NULL
  }
  set_node_labels(obj)
  get(label_key, envir = counter)
}



r_optree_diagram <- function(optree,
                             ...,
                             next_diagram_id,
                             use_table_names,
                             show_table_columns) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery:::r_optree_diagram")
  immed_nodes <- NULL
  prev_nodes <- NULL
  prev_edges <- NULL
  ninputs <- length(optree$source)
  unique_op_node_name <- as.character(optree$mutable_annotation_space$node_id)
  if(ninputs>0) {
    for(i in seq_len(ninputs)) {
      ndi <- r_optree_diagram(optree$source[[i]],
                              next_diagram_id = next_diagram_id,
                              use_table_names = use_table_names,
                              show_table_columns = show_table_columns)
      next_diagram_id <- ndi$next_diagram_id
      immed_nodes <- c(immed_nodes,
                       ndi$nodes[[length(ndi$nodes)]]$name)
      prev_nodes <- c(prev_nodes, ndi$nodes)
      prev_edges <- c(prev_edges, ndi$edges)
    }
  }
  nodeid <- next_diagram_id
  next_diagram_id <- next_diagram_id + 1
  table_name_in <- NULL
  table_name_out <- NULL # TODO: set this
  name = paste0("node_", nodeid)
  label = format_node(optree)
  if("relop_table_source" %in% class(optree)) {
    if(use_table_names) {
      name <- paste("table", optree$table_name, sep = "_")
    }
    table_name_in <- optree$table_name
    if(!show_table_columns) {
      label <- optree$table_name
    }
  }
  label = gsub("\n", "\\l", label, fixed = TRUE)
  label = gsub("'", "", label)
  label = gsub('"', "", label)
  label = gsub("", "", label)
  label_parts <- strsplit(label, "\\l", fixed = TRUE)[[1]]
  max_len <- max(nchar(label_parts))
  width_limit = 80
  if(max_len>width_limit) {
    label_parts <- vapply(label_parts,
                    function(li) {
                      if(nchar(li)>width_limit) {
                        paste(substr(li, 1, width_limit), "[...]")
                      } else {
                        li
                      }
                    }, character(1))
    label <- paste(label_parts, collapse = "\\l")
  }
  node <- list(list(nodeid = nodeid,
                    name = name,
                    table_name_in = table_name_in,
                    table_name_out = table_name_out,
                    unique_op_node_name = unique_op_node_name,
                    label = label,
                    optree = optree))
  edge = NULL
  if(length(immed_nodes)>0) {
    labels = "."
    if(length(immed_nodes)>1) {
      labels = paste0(".",
                      as.character(seq_len(length(immed_nodes))))
    }
    edge <- paste0(immed_nodes, " -> ",
                  name,
                  " [ label = '",
                  labels,
                  "']")
  }
  list(next_diagram_id = next_diagram_id,
       unique_op_node_name = unique_op_node_name,
       nodes = c(prev_nodes, node),
       edges = c(prev_edges, edge))
}

#' Build a diagram of a optree pipeline.
#'
#' @param optree operation tree pipeline (or list of such).
#' @param ... force other argument to be by name.
#' @param merge_tables logical, if TRUE merge all same table references into one node.  rel_op nodes that declare a materialize_as name will be cross-linked.
#' @param show_table_columns logical, if TRUE show table columns.
#' @return character DiagrammeR::grViz() ready text.
#'
#' @examples
#'
#' d <- mk_td('d',
#'              columns = qc(AUC, R2))
#' optree <- d %.>%
#'   extend(., v %:=% ifelse(AUC>0.5, R2, 1.0)) %.>%
#'   quantile_node(.) %.>%
#'   natural_join(., d, jointype = "LEFT", by = "AUC") %.>%
#'   orderby(., "AUC")
#'
#' cat(format(optree))
#'
#' cat(op_diagram(optree))
#'
#' if(requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   optree %.>%
#'     op_diagram(., merge_tables = TRUE) %.>%
#'     DiagrammeR::grViz(.) %.>%
#'     print(.)
#'  # # or to render to png
#'  # optree %.>%
#'  #   op_diagram(., merge_tables = TRUE) %.>%
#'  #   DiagrammeR::DiagrammeR(diagram = ., type = "grViz") %.>%
#'  #     DiagrammeRsvg::export_svg(.) %.>%
#'  #     charToRaw(.) %.>%
#'  #     rsvg::rsvg_png(., file = "diagram1.png")
#' }
#'
#' @export
#'
op_diagram <- function(optree,
                       ...,
                       merge_tables = FALSE,
                       show_table_columns = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::op_diagram")
  diagram <- "
digraph rquery_optree {
  graph [ layout = dot, rankdir = TB, overlap = prism, compound = true, nodesep = .5, ranksep = .25]
  edge [decorate = true, arrowhead = normal]
  node [style=filled, fillcolor=lightgrey]

"
  next_obj_id = 1
  next_diagram_id = 1
  if(!("relop" %in% class(optree))) {
    # list of optrees case
    nodes <- list()
    edges <- list()
    for(opt in optree) {
      next_obj_id <- label_nodes(opt, next_obj_id = next_obj_id)
      graph <- r_optree_diagram(opt,
                                next_diagram_id = next_diagram_id,
                                use_table_names = merge_tables,
                                show_table_columns = show_table_columns)
      next_diagram_id <- graph$next_diagram_id + 1
      nodesi <- graph$nodes
      nodesi[[length(nodesi)]]$table_name_out <- opt$materialize_as
      nodes <- c(nodes, nodesi)
      edges <- c(edges, graph$edges)
    }
    graph <- list(next_diagram_id = next_diagram_id,
                  nodes = nodes,
                  edges = edges)
  } else {
    next_obj_id <- label_nodes(optree, next_obj_id = next_obj_id)
    graph <- r_optree_diagram(optree,
                              next_diagram_id = next_diagram_id,
                              use_table_names = merge_tables,
                              show_table_columns = show_table_columns)
  }
  # de-dup any nodes
  node_names <- vapply(graph$nodes, function(ni) { ni$name }, character(1))
  graph$nodes <- graph$nodes[sort(unique(match(node_names, node_names)))]
  node_names <- vapply(graph$nodes, function(ni) { ni$name }, character(1))
  # add any table to table edges
  incoming_names <- lapply(graph$nodes, function(ni) { ni$table_name_in })
  outgoing_names <- lapply(graph$nodes, function(ni) { ni$table_name_out })
  matches <- match(incoming_names, outgoing_names)
  for(i in seq_len(length(matches))) {
    if((!is.na(i)) && (!is.null(incoming_names[[i]]))) {
      li <- matches[[i]]
      if((!is.null(li))&&(!is.na(li))) {
        edge <- paste0(node_names[[li]], " -> ",
                       node_names[[i]])
        graph$edges <- c(graph$edges, edge)
      }
    }
  }
  counts <- list()
  for(nd in graph$nodes) {
    prev <- counts[[nd$unique_op_node_name]]
    if(is.null(prev)) {
      prev <- 0
    }
    counts[[nd$unique_op_node_name]] <- prev + 1
  }
  shapes <- rep("box", length(graph$nodes))
  colors <- rep("khaki3", length(graph$nodes))
  msgs_seen <- list()
  for(ii in seq_len(length(graph$nodes))) {
    ni <- graph$nodes[[ii]]
    if(!is.null(ni$table_name_in)) {
      shapes[[ii]] <- "folder"
      colors[[ii]] <- "chartreuse3"
    } else {
      if(counts[[ni$unique_op_node_name]]>1) {
        shapes[[ii]] <- "note"
        colors[[ii]] <- "orange"
        if(length(msgs_seen)<5) {
          msg <- paste(format_node(ni$optree), collapse = " ")
          if(!(msg %in% msgs_seen)) {
            warning(paste("possible repeated calculation:\n", msg))
            msgs_seen <- c(msgs_seen, msg)
          }
        }
      }
    }
  }
  ntxts <- lapply(seq_len(length(graph$nodes)),
                  function(ii) {
                    ni <- graph$nodes[[ii]]
                    paste0(ni$name,
                           " [ shape = '",
                           shapes[[ii]],
                           "' , fillcolor = '",
                           colors[[ii]],
                           "' , label = '",
                           ni$label,
                           "']\n")
                  })
  diagram <- paste0(diagram, paste(ntxts, collapse = "\n"))
  diagram <- paste0(diagram,  paste(graph$edges, collapse = "\n"))
  diagram <- paste0(diagram, "\n}")
  diagram
}
