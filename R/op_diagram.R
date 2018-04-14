
r_optree_diagram <- function(optree, nextid) {
  immed_nodes <- NULL
  prev_nodes <- NULL
  prev_edges <- NULL
  ninputs <- length(optree$source)
  if(ninputs>0) {
    for(i in seq_len(ninputs)) {
      ndi <- r_optree_diagram(optree$source[[i]], nextid)
      nextid <- ndi$nextid
      immed_nodes <- c(immed_nodes,
                       ndi$nodes[[length(ndi$nodes)]]$name)
      prev_nodes <- c(prev_nodes, ndi$nodes)
      prev_edges <- c(prev_edges, ndi$edges)
    }
  }
  nodeid <- nextid
  nextid <- nextid + 1
  name = paste0("node", nodeid)
  label = format_node(optree)
  label = gsub("\n", "\\l", label, fixed = TRUE)
  label = gsub("'", "", label)
  label = gsub("", "", label)
  node <- list(list(nodeid = nodeid,
                    name = name,
                    is_table = "relop_table_source" %in% class(optree),
                    label = label))
  edge = NULL
  if(length(immed_nodes)>0) {
    edge <- paste(immed_nodes, "->", name)
  }
  list(nextid = nextid,
       nodes = c(prev_nodes, node),
       edges = c(prev_edges, edge))
}

#' Build a diagram of a optree pipeline.
#'
#' @param optree operation tree pipeline
#' @return character DiagrammeR::grViz() ready text
#'
#' @examples
#'
#' optree <- table_source('d',
#'                        columns = qc(AUC, R2)) %.>%
#'   extend_nse(., v := ifelse(AUC>0.5, R2, 1.0))
#'
#' cat(format(optree))
#'
#' cat(op_diagram(optree))
#' # optree %.>% op_diagram(.) %.>% DiagrammeR::grViz(.)
#'
#' @export
#'
op_diagram <- function(optree) {
  diagram <- "
digraph rquery_optree {
  graph [ layout = dot, rankdir = LR, overlap = prism, compound = true, nodesep = .5, ranksep = .25]
  edge [decorate = true, arrowhead = normal]
  node [style=filled, fillcolor=lightgrey]

"
  graph <- r_optree_diagram(optree, 1)
  ntxts <- lapply(graph$nodes,
                  function(ni) {
                    paste0(ni$name,
                           " [ shape = '",
                           ifelse(ni$is_table, "folder", "tab"),
                           "' , label = '",
                           ni$label,
                           "']\n")
                  })
  diagram <- paste0(diagram, paste(ntxts, collapse = "\n"))
  diagram <- paste0(diagram,  paste(graph$edges, collapse = "\n"))
  diagram <- paste0(diagram, "\n}")
  diagram
}
