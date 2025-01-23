#' Perform geneset enrichment testing using any supported method
#' 
#' See original documentation at \link[goat]{test_genesets}
#'
#' @param genesets tibble with genesets, must contain columns 'source', 'source_version', 'id', 'name', 'genes', 'ngenes', 'ngenes_signif'
#' @param genelist tibble with genes, must contain column 'gene' and 'test'. gene = character column, which are matched against list column 'genes' in genesets tibble. test = boolean column (you can set all to FALSE if not performing Fisher-exact or hypergeometric test downstream)
#' @param method method for overrepresentation analysis. Options: "goat", "hypergeometric", "fisherexact", "fisherexact_ease", "gsea", "idea"
#' @param score_type string, default: "effectsize", alternatively set to "pvalue", "effectsize_up", "effectsize_down", "effectsize_abs"
#' @param padj_method first step of multiple testing correction; method for p-value adjustment, passed to stats::p.adjust() via padjust_genesets(), e.g. set "BH" to compute FDR adjusted p-values (default) or "bonferroni" for a more stringent procedure
#' @param padj_sources second step of multiple testing correction; apply Bonferroni adjustment to all p-values according to the number of geneset sources that were tested. Boolean parameter, set TRUE to enable (default) or FALSE to disable
#' @param padj_cutoff cutoff for adjusted p-value, signif column is set to TRUE for all values lesser-equals
#' @param padj_min_signifgenes if a value larger than zero is provided, this will perform additional post-hoc filtering; after p-value adjustment, set the pvalue_adjust to NA and signif to FALSE for all genesets with fewer than padj_min_signifgenes 'input genes that were significant' (ngenes_signif column in genesets table). So this does not affect the accuracy of estimated p-values, in contrast to prefiltering genesets prior to p-value computation or adjusting p-values
#' @param ... further parameters are passed to the respective stats method
#'
#' @returns the input genesets, with results stored in columns 'pvalue', 'pvalue_adjust' and 'signif'
#' @export
run_geneset_enrichment <- function(
    genesets, 
    genelist, 
    method = "goat",
    score_type = "effectsize",
    padj_method = "BH",
    padj_sources = TRUE,
    padj_cutoff = 0.01, 
    padj_min_signifgenes = 0L,
    ...) {
  # NOTE: attaching and detaching namespace around function call to fix getting goat::goat_nulldistributions for testing genesets
  ## issued at: https://github.com/ftwkoopmans/goat/issues/8
  if( ! "package:goat" %in% search()) {
    attachNamespace("goat")
  }
  goat_result <- goat::test_genesets(
    genesets = genesets,
    genelist = genelist,
    method = "goat",
    score_type = "effectsize",
    padj_method = "BH",
    padj_sources = TRUE,
    padj_cutoff = 0.01, 
    padj_min_signifgenes = 0L
  )
  if("package:goat" %in% search()) {
    detach("package:goat")
  }
  return(goat_result)
}