#' Get PPI igraph
#'
#' @param ppi_data dataframe, PPI by aliases/ids in columns 'from' and 'to'
#' @param vertex_clustering NULL, else numerical vector of cluster IDs
#' 
#' @description
#' Uses Leiden clustering on modularity for community detection.
#' Leiden was chosen as default as expected PPI data is not inherently hierarchical, which is why modularity optimalization is used on the graph topology. 
#' Expected PPI data comes from genes/proteins (of interest) selected from gene set enrichment analysis or differential expression analysis.
#' Using clustering from terms is not possible, as genes can be in multiple terms. 
#' Leiden also scales well to large graphs, has consistent clustering outcomes and provides some inherent guarantees by its method, e.g. locally optimal assignment. 
#' 
#' @references Traag, V.A., Waltman, L. & van Eck, N.J. From Louvain to Leiden: guaranteeing well-connected communities. Sci Rep 9, 5233 (2019). https://doi.org/10.1038/s41598-019-41695-z
#'
#' @returns igraph object of PPI data
#' @export
#' 
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph set_vertex_attr
#' @importFrom igraph set_edge_attr
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph degree
#' @importFrom igraph betweenness
#' @importFrom igraph set_graph_attr 
#' @importFrom igraph cluster_leiden 
#' @importFrom igraph closeness
#' @importFrom igraph knn
#' @importFrom igraph constraint
#' @importFrom igraph edge_betweenness
#' @importFrom igraph diversity
#' @importFrom igraph modularity
#' @importFrom igraph graph_center
#' @importFrom igraph edge_density
#' @importFrom igraph transitivity
#' @importFrom igraph mean_distance
#' @importFrom igraph assortativity_degree
#' @importFrom igraph centr_degree
#' @importFrom igraph centr_betw
#' @importFrom igraph centr_clo
#' @importFrom igraph centr_eigen
#' @importFrom igraph membership
#' @importFrom igraph empty_graph
#' 
#' @examples
#' get_ppigraph(
#'   get(load(system.file("extdata", "example_ppi_data.rda", package = "goatea")))
#' )
get_ppigraph <- function(ppi_data, vertex_clustering = NULL) {
  ## input validation
  stopifnot(
    is.data.frame(ppi_data),
    all(c("from", "to") %in% colnames(ppi_data))
  )
  if (nrow(ppi_data) == 0) return(empty_graph())
    
  ## convert ppi to igraph
  g <- graph_from_data_frame(ppi_data, directed = FALSE)
  
  ## graph clustering
  cl <- cluster_leiden(g, objective_function = 'modularity', n_iterations = -1) # run until convergence
  ## set vertices metrics
  # community ID per vertex
  if ( ! length(unique(vertex_clustering)) %in% c(0, 1)) {
    g <- set_vertex_attr(g, "cluster", value = vertex_clustering)
  } else {
    g <- set_vertex_attr(g, "cluster", value = membership(cl)) 
  }
  
  g <- set_vertex_attr(g, "degree", value = degree(g)) # node edges amount
  g <- set_vertex_attr(g, "betweenness", value = betweenness(g, directed = FALSE)) # node centrality: how often shortest path between all node pairs cross
  g <- set_vertex_attr(g, "closeness", value = closeness(g)) # node closeness centrality: mean weighted steps required to all other nodes
  g <- set_vertex_attr(g, "knn", value = knn(g)$knn) # average nearest neighbour degree
  ## set edge metrics
  g <- set_edge_attr(g, 'edge_betweenness', value = edge_betweenness(g, directed = FALSE)) # edge centrality: how often shortest path between all edge pairs cross 
  if ("combined_score" %in% colnames(ppi_data)) {
    ## set edge weight by STRINGdb combined score
    g <- set_edge_attr(g, 'weight', value = scale_values_between(values = E(g)$combined_score, 
                                                            old_min = min(E(g)$combined_score), old_max = max(E(g)$combined_score),
                                                            new_min = .05, new_max = .25)) # scaled STRING combined score for visual representation
    ## set diversity: requires graph weight
    g <- set_vertex_attr(g, 'diversity', value = diversity(g)) # Shannon entropy of incident edges 
  }
  g <- set_vertex_attr(g, "id", value = unique(c(ppi_data$from, ppi_data$to))) # unique node IDs
  ## set graph reportable metrics
  g <- set_graph_attr(g, 'central gene', V(g)[graph_center(g)]$name) # most central node(s)
  g <- set_graph_attr(g, 'modularity', modularity(g, membership = membership(cl))) # cluster modularity 
  g <- set_graph_attr(g, 'transitivity', transitivity(g, isolates = "zero")) # clustering coefficient: probability of adjacent vertex connectivity
  g <- set_graph_attr(g, 'assortattivity', assortativity_degree(g)) # positive if vertices are similarly connected, otherwise negative
  g <- set_graph_attr(g, 'mean distance', mean_distance(g)) # mean length of all shortest paths 
  g <- set_graph_attr(g, 'edge density', edge_density(g)) # ratio of edges/potential_edges)
  g <- set_graph_attr(g, 'degree centralization', centr_degree(g)$centralization) # high centralization: a few nodes dominate (e.g. star network), low: nodes are more evenly connected (e.g. fully/evenly connected graphs)
  g <- set_graph_attr(g, 'betweenness centralization', centr_betw(g)$centralization) # higher centralization: more shortest paths controlled by fewer nodes
  g <- set_graph_attr(g, 'closeness centralization', centr_clo(g)$centralization) # higher centralization: more shortest paths closer to each other
  g <- set_graph_attr(g, 'eigen centralization', centr_eigen(g)$centralization) # higher centralization: more globally important nodes 
  return(g)
}
