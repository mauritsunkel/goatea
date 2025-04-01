#' Calculate geneSetRatio per gene per enrichment contrast
#' 
#' Get a percentage of genesets the specific gene is included
#'
#' @param enrichment_results list of enrichment results
#' @param gene_overview_df dataframe with gene-wise information
#'
#' @export
calculate_geneSetRatio <- function (enrichment_results, gene_overview_df) {
  for (name in names(enrichment_results)) {
    df <- enrichment_results[[name]]
    n_terms <- nrow(df)
    gene_counts <- table(unlist(df$symbol))
    geneset_ratio <- ifelse(gene_overview_df$symbol %in% names(gene_counts), gene_counts, 0) / n_terms * 100
    gene_overview_df[paste0(name, "_geneSetRatio")] <- geneset_ratio
  }
  
  return(gene_overview_df)
}
