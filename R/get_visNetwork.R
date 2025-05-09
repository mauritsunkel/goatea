#' Get visNetwork graph 
#' 
#' Gets visNetwork graph with ppigraph, and optionally genes overview, metadata
#'
#' @param ppigraph igraph object, get from `get_ppigraph()`
#' @param genes_overview (optional) dataframe, default: NULL, else metadata dataframe for ppigraph proteins/genes aliases 
#' @param sample_name (optional) character, default: NULL, else sample name found in genes_overview columns
#'
#' @returns list of visNetwork nodes and edges and given ppigraph
#'
#' @export
#' 
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph vertex_attr
#' @importFrom igraph edge_attr
#' @importFrom igraph is_igraph
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#'
#' @examples
#' ppi_graph <- get_ppigraph(goatea::example_ppi_data)
#' get_visNetwork(ppi_graph)
get_visNetwork <- function(ppigraph, genes_overview = NULL, sample_name = NULL) {
  if (is(ppigraph, "igraph_constructor_spec")) return(list(nodes = data.frame(), edges = data.frame(), ppigraph = ppigraph))
  stopifnot(
    is_igraph(ppigraph),
    is.null(genes_overview) | is.data.frame(genes_overview),
    is.null(sample_name) | is.character(sample_name)
  )
  
  ## setup node title
  node_names <- names(vertex_attr(ppigraph))
  node_title <- ""
  if ('name' %in% node_names) node_title <- paste0(node_title, paste0("Protein alias: ", V(ppigraph)$name, "<br>"))
  if ( ! is.null(genes_overview)) {
    if ('gene_annotation' %in% colnames(genes_overview)) node_title <- paste0(node_title, paste0("Protein annotation: ", genes_overview$gene_annotation[match(V(ppigraph)$name, toupper(genes_overview$symbol))], "<br>"))
    if ('genelist_overlap' %in% colnames(genes_overview)) node_title <- paste0(node_title, paste0("Genelist overlap: ", genes_overview$genelist_overlap[match(V(ppigraph)$name, toupper(genes_overview$symbol))], "<br>"))
    if ( ! is.null(sample_name)) {
      if (paste0(sample_name, "_efsi") %in% colnames(genes_overview)) node_title <- paste0(node_title, paste0('Effectsize: ', round(genes_overview[[paste0(sample_name, "_efsi")]][match(V(ppigraph)$name, toupper(genes_overview$symbol))], digits = 3), "<br>"))
    }
  }

  if ('cluster' %in% node_names) node_title <- paste0(node_title, paste0("Cluster: ", V(ppigraph)$cluster, "<br>"))
  if ('degree' %in% node_names) node_title <- paste0(node_title, paste0("Degree: ", V(ppigraph)$degree, "<br>"))
  if ('knn' %in% node_names) node_title <- paste0(node_title, paste0("KNN: ", round(V(ppigraph)$knn, digits = 3), "<br>"))
  if ('betweenness' %in% node_names) node_title <- paste0(node_title, paste0("Betweenness: ", round(V(ppigraph)$betweenness, digits = 3), "<br>"))
  if ('closeness' %in% node_names) node_title <- paste0(node_title, paste0("Closeness: ", round(V(ppigraph)$closeness, digits = 3), "<br>"))
  if ('diversity' %in% node_names) node_title <- paste0(node_title, paste0("Diversity: ", round(V(ppigraph)$diversity, digits = 3), "<br>"))
  if ('id' %in% node_names) node_title <- paste0(node_title, paste0("STRINGdb ID: ", V(ppigraph)$id, "<br>"))
  
  ## set nodes
  nodes <- as.data.frame(vertex_attr(ppigraph)) %>% rename('label' = 'name')
  nodes$title <- paste('Betweenness:', V(ppigraph)$betweenness) # tooltip
  if ('cluster' %in% colnames(nodes)) {
    colors <- colorify(n = max(nodes$cluster)+1, colors = 'Okabe-Ito')[-1][nodes$cluster]
    nodes$color.background <- colors
    nodes$color.border <- colors
  }
  ## add metadata
  if ( ! is.null(genes_overview)) {
    ## add sample-wise metadata
    if ( ! is.null(sample_name)) {
      if ('genelist_overlap' %in% colnames(genes_overview)) genes_overview$signif <- ifelse(grepl(sample_name, genes_overview$genelist_overlap), 1, -1)
      genes_overview$updown <- ifelse(genes_overview[[paste0(sample_name, '_efsi')]] > 0, 1, -1)
    }
    ## set color map 
    color_map <- colorify(
      colors = c('blue', 'black', 'red'), 
      colors_breakpoints = c(
        min(genes_overview[[paste0(sample_name, '_efsi')]], na.rm = TRUE), 
        0, 
        max(genes_overview[[paste0(sample_name, '_efsi')]], na.rm = TRUE)))

    nodes <- nodes %>% left_join(mutate(genes_overview, symbol_upper = toupper(.data$symbol)), by = c('label' = 'symbol_upper'))

    ## map values to colors
    if ( ! is.null(sample_name)) {
      color_values <- replace(nodes[[paste0(sample_name, "_efsi")]], is.na(nodes[[paste0(sample_name, "_efsi")]]), 0)
      nodes$color.border <- color_map(color_values)
      nodes$title <- node_title # tooltip
    }
  }
  
  ## setup edge title
  edge_names <- setdiff(names(edge_attr(ppigraph)), c('from', 'to', 'width'))
  edge_title <- ""
  if ('combined_score' %in% edge_names) edge_title <- paste0(edge_title, paste0("STRING score: ", round(E(ppigraph)$combined_score, digits = 4), "<br>"))
  if ('edge_betweenness' %in% edge_names) edge_title <- paste0(edge_title, paste0("Betweenness: ", round(E(ppigraph)$edge_betweenness, digits = 3), "<br>"))
  ## set edges
  edges <- as.data.frame(edge_attr(ppigraph)) %>% 
    rename('width' = 'weight') %>% # visNetwork edge size
    mutate(
      id = paste(.data$from, .data$to, sep = "_"),
      width = .data$width * 10, # edge visual size 
      title = edge_title # tooltip
    ) 
  
  return(list(nodes = nodes, edges = edges, ppigraph = ppigraph))
}
