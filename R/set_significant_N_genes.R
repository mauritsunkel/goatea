#' Set significant and number of genes
#'
#' @param genelist list, loaded genelist with goatea::read_validate_genelist()
#' @param significance_by string, default: 'pvalue_effectsize', else 'pvalue' or 'effectsize' to set gene significance to TRUE/FALSE in 'signif' column 
#' @param pvalue_threshold numeric, default: 0.05, to set gene significance based on pvalue
#' @param effectsize_threshold numeric, default: 1, to set gene significance based on effectsize
#' @param keep_max_n_genes boolean, default: TRUE, filter down by pvalue to max n genes allowed by goat (max(goat::goat_nulldistributions$N))
#' @param keep_max_n_genes_by string, default: 'pvalue', else 'effectsize', order genes based on lowest pvalues or highest absolute effect sizes

#' @returns genelist with added 'signif' column with TRUE/FALSE values
#' 
#' @export
#' 
#' @examples
#' set_significant_N_genes(
#'   get(load(system.file("extdata", "example_genelist.rda", package = "goatea")))
#' )
set_significant_N_genes <- function(
  genelist, 
  significance_by = 'pvalue_effectsize',
  pvalue_threshold = 0.05, 
  effectsize_threshold = 1,
  keep_max_n_genes = FALSE,
  keep_max_n_genes_by = 'pvalue'
) {
  
  ## keep max N genes by ordered genelist 
  max_n_genes <- ifelse(keep_max_n_genes, max(goat::goat_nulldistributions$N), nrow(genelist))
  if (keep_max_n_genes_by == 'pvalue') {
    genelist <- genelist %>% arrange(.data$pvalue)
  } else if (keep_max_n_genes_by == 'effectsize') {
    genelist <- genelist %>% arrange(abs(.data$effectsize))
  }
  genelist <- genelist %>% slice_head(n = max_n_genes)
  ## warn to potentially filter down to max n genes
  if (nrow(genelist) > max(goat::goat_nulldistributions$N)) {
    msg <- paste0("genelist table exceeding ", max(goat::goat_nulldistributions$N), " genes (", length(genelist$gene), "). Limited for method = 'goat', yet might work for other methods.")
    warning(msg)
    if ( ! is.null(shiny::getDefaultReactiveDomain())) shiny::showNotification(msg, type = 'warning')
  }
  
  ## set significane by both or either pvalue/effectsize
  if (significance_by == 'pvalue_effectsize') {
    genelist$signif <- genelist$pvalue <= pvalue_threshold & abs(genelist$effectsize) >= effectsize_threshold
  } else if (significance_by == 'pvalue') {
    genelist$signif <- genelist$pvalue <= pvalue_threshold 
  } else if (significance_by == 'effectsize') {
    genelist$signif <- abs(genelist$effectsize) >= effectsize_threshold
  }
  
  return(genelist)
}
  
