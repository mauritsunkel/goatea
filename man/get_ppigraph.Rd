% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ppi_graph.R
\name{get_ppigraph}
\alias{get_ppigraph}
\title{Get PPI igraph}
\usage{
get_ppigraph(ppi_data, vertex_clustering = NULL)
}
\arguments{
\item{ppi_data}{dataframe, PPI by aliases/ids in columns 'from' and 'to'}

\item{vertex_clustering}{NULL, else numerical vector of cluster IDs}
}
\value{
igraph object of PPI data
}
\description{
Uses Leiden clustering on modularity for community detection.
Leiden was chosen as default as expected PPI data is not inherently hierarchical, which is why modularity optimalization is used on the graph topology.
Expected PPI data comes from genes/proteins (of interest) selected from gene set enrichment analysis or differential expression analysis.
Using clustering from terms is not possible, as genes can be in multiple terms.
Leiden also scales well to large graphs, has consistent clustering outcomes and provides some inherent guarantees by its method, e.g. locally optimal assignment.
}
\examples{
get_ppigraph(goatea::example_ppi_data)
}
\references{
Traag, V.A., Waltman, L. & van Eck, N.J. From Louvain to Leiden: guaranteeing well-connected communities. Sci Rep 9, 5233 (2019). https://doi.org/10.1038/s41598-019-41695-z
}
