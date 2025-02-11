#' Plot semantic similarity termtree 
#'
#' @param enrichment_result GOAT enrichment result dataframe
#' @param Nterms integer, default: 50, sets amount of terms to plot
#' @param Nwords integer, default: 5, sets N summarized words per cluster 
#'
#' @returns ggtree/gg/ggplot object 
#' @export
plot_termtree <- function(enrichment_result, Nterms = 50, Nwords = 5) {
  if ( ! requireNamespace("enrichplot")) stop("Need 'enrichplot' package installed via: BiocManager::install('enrichplot')")
  df <- enrichment_result
  if (nrow(df) == 0) stop("Enrichment result passed to plot GOtermtree has no results.")
  if (Nterms > nrow(df)) Nterms <- nrow(df)
  
  df <- df %>%
    mutate(core_enrichment = purrr::map_chr(genes, ~ paste(.x, collapse = "/"))) %>%
    rename(ID = id, Description = name, p.adjust = pvalue_adjust)
  
  goatsea <- new("gseaResult", result = as.data.frame(df), geneSets = as.list(setNames(lapply(df$genes, as.character), df$ID)))
  rownames(goatsea@result) <- goatsea@result$ID
  
  goatsea_termsim <- enrichplot::pairwise_termsim(goatsea)
  
  p <- enrichplot::treeplot(
    goatsea_termsim,
    showCategory = Nterms,
    label_format = 14,
    cluster.params = list(label_words_n = Nwords)
  )
  return(p)
}
