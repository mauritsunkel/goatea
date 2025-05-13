#' An example genelist
#'
#' A simulated example of a genelist for testing or demonstration purposes.
#' 
#' @format ## `genelist`
#' A data frame with 100 rows and 4 columns:
#' \describe{
#'   \item{symbol}{Gene_1, Gene_2}
#'   \item{gene}{10000, 10001}
#'   \item{pvalue}{0.05, 1}
#'   \item{effectsize}{2.5, 0}
#'   ...
#' }
#' @source generated with `data-raw/example_data.R`
#' @name example_genelist
#' @docType data
NULL

#' Example genesets
#' 
#' A simulated example of a geneset for testing or demonstration purposes.
#'
#' @format ## `genesets`
#' A data frame with 100 rows and 4 columns:
#' \describe{
#'   \item{source}{origin}
#'   \item{source_version}{org.Xx.eg.db}
#'   \item{id}{DB.001}
#'   \item{name}{geneset name 1, geneset name 2}
#'   \item{parent_id}{DB.010, DB.020}
#'   \item{genes}{10000, 10001}
#'   \item{ngenes}{10, 30}
#'   ...
#' }
#' @source generated with `data-raw/example_data.R`
#' @name example_genesets
#' @docType data
NULL 

#' An example enrichment
#' 
#' A simulated example of an enrichemnt for testing or demonstration purposes.
#'
#' @format ## `enrichment`
#' A data frame with 10 rows and 17 columns:
#' \describe{
#'   \item{source}{origin}
#'   \item{source_version}{org.Xx.eg.db}
#'   \item{id}{DB.001}
#'   \item{name}{geneset name 1, geneset name 2}
#'   \item{parent_id}{DB.010, DB.020}
#'   \item{ngenes_input}{10, 30}
#'   \item{ngenes}{10, 30}
#'   \item{genes}{10000, 10001}
#'   \item{ngenes_signif}{10, 30}
#'   \item{score_type}{effectsize}
#'   \item{pvalue}{0.05, 1}
#'   \item{zscore}{-Inf, 0, Inf}
#'   ...
#' }
#' @source generated with `data-raw/example_data.R`
#' @name example_enrichment
#' @docType data
NULL 

#' An example genes overview
#' 
#' A simulated example of a gene overview for testing or demonstration purposes.
#'
#' @format ## `genes overview`
#' A data frame with 100 rows and ~11 columns:
#' \describe{
#'   \item{gene}{10000, 10001}
#'   \item{symbol}{Gene_1, Gene_2}
#'   \item{sample_efsi}{2.5, 0}
#'   \item{sample_pval}{0.05, 1}
#'   \item{sample_perc}{0, 100}
#'   \item{genelist_overlap}{"", "A", "B", "AB"}
#'   \item{sample_geneSetRatio}{0, 50, 100}
#'   ...
#' }
#' @source generated with `data-raw/example_data.R`
#' @name example_genes_overview
#' @docType data
NULL 

#' An example ppi data
#' 
#' A simulated example of a ppi dataframe for testing or demonstration purposes.
#'
#' @format ## `ppi data`
#' A data frame with 15 rows and 5 columns:
#' \describe{
#'   \item{from_symbol}{gene_A, gene_B}
#'   \item{to_symbol}{gene_A, gene_B}
#'   \item{combined_score}{0, 1000}
#'   \item{from}{gene_A_ID, gene_B_ID}
#'   \item{to}{gene_A_ID, gene_B_ID}
#'   ...
#' }
#' @source generated with `data-raw/example_data.R`
#' @name example_ppi_data
#' @docType data
NULL 
