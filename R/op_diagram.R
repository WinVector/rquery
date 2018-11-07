
r_optree_diagram <- function(optree, nextid, use_table_names) {
  immed_nodes <- NULL
  prev_nodes <- NULL
  prev_edges <- NULL
  ninputs <- length(optree$source)
  if(ninputs>0) {
    for(i in seq_len(ninputs)) {
      ndi <- r_optree_diagram(optree$source[[i]], nextid, use_table_names)
      nextid <- ndi$nextid
      immed_nodes <- c(immed_nodes,
                       ndi$nodes[[length(ndi$nodes)]]$name)
      prev_nodes <- c(prev_nodes, ndi$nodes)
      prev_edges <- c(prev_edges, ndi$edges)
    }
  }
  nodeid <- nextid
  nextid <- nextid + 1
  table_name_in <- NULL
  table_name_out <- NULL # TODO: set this
  name = paste0("node_", nodeid)
  if("relop_table_source" %in% class(optree)) {
    if(use_table_names) {
      name <- paste("table", optree$table_name, sep = "_")
    }
    table_name_in <- optree$table_name
  }
  label = format_node(optree)
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
                    label = label))
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
  list(nextid = nextid,
       nodes = c(prev_nodes, node),
       edges = c(prev_edges, edge))
}

#' Build a diagram of a optree pipeline.
#'
#' @param optree operation tree pipeline.
#' @param ... force other argument to be by name.
#' @param merge_tables logical if TRUE merge all same table references into one node.
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
                       merge_tables = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "rquery::op_diagram")
  diagram <- "
digraph rquery_optree {
  graph [ layout = dot, rankdir = TB, overlap = prism, compound = true, nodesep = .5, ranksep = .25]
  edge [decorate = true, arrowhead = normal]
  node [style=filled, fillcolor=lightgrey]

"
  graph <- r_optree_diagram(optree, 1, merge_tables)
  # de-dup any nodes
  node_names <- vapply(graph$nodes, function(ni) { ni$name }, character(1))
  graph$nodes <- graph$nodes[sort(unique(match(node_names, node_names)))]
  node_names <- vapply(graph$nodes, function(ni) { ni$name }, character(1))
  # add any table to table edges
  incoming_names <- lapply(graph$nodes, function(ni) { ni$table_name_in })
  outgoing_names <- lapply(graph$nodes, function(ni) { ni$table_name_out })
  matches <- match(incoming_names, outgoing_names)
  for(i in seq_len(length(matches))) {
    if((!is.null(incoming_names[[i]])) &&
       (!is.na(incoming_names[[i]])) &&
       (!is.null(outgoing_names[[i]])) &&
       (!is.na(outgoing_names[[i]]))) {
      li <- matches[[i]]
      if((!is.null(li))&&(!is.na(li))) {
        edge <- paste0(node_names[[li]], " -> ",
                       node_names[[i]])
        graph$edges <- c(graph$edges, edge)
      }
    }
  }
  ntxts <- lapply(graph$nodes,
                  function(ni) {
                    paste0(ni$name,
                           " [ shape = '",
                           ifelse(is.null(ni$table_name_in),
                                  "tab",
                                  "folder"),
                           "' , label = '",
                           ni$label,
                           "']\n")
                  })
  diagram <- paste0(diagram, paste(ntxts, collapse = "\n"))
  diagram <- paste0(diagram,  paste(graph$edges, collapse = "\n"))
  diagram <- paste0(diagram, "\n}")
  diagram
}
