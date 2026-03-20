#' Plot semantic similarity termtree 
#'
#' @param genelist GOAT current genelist from selected enrichment sample
#' @param genesets GOAT filtered genesets
#' @param map_organism integer, default: 9606 (human) - input organism ID that will be mapped to org.Xx.eg.db 
#' @param effectsize_threshold numerical, default: 1 - genelist effectsize threshold
#' @param Nterms integer, default: NA to plat all terms, integer sets amount of terms to plot
#' @param Nwords integer, default: 5, sets N summarized words per cluster 
#' @param Nclusters integer, default: 1, sets N clusters of terms 
#'
#' @returns ggtree/gg/ggplot object 
#' 
#' @export
#' 
#' @importFrom purrr map_chr 
#' @importFrom enrichplot pairwise_termsim treeplot
#' @importFrom rlang .data
#' 
#' @examples
#' plot_termtree(
#'   genelist = get(load(system.file("extdata", "example_genelist.rda", package = "goatea"))),
#'   genesets = get(load(system.file("extdata", "example_genesets.rda", package = "goatea")))
#' )
plot_termtree <- function(genelist, genesets, map_organism = 9606, effectsize_threshold = 1, Nterms = NA, Nwords = 5, Nclusters = 3) {
  if ( ! requireNamespace("enrichplot")) stop("Need 'enrichplot' package installed via: BiocManager::install('enrichplot')")
  if (length(genelist) == 0) stop("List passed to plot termtree has no genes/proteins")
  if (is.na(Nterms) || Nterms > nrow(genelist)) Nterms <- nrow(genelist)
  if (Nterms < 2) stop("termtree Nterms should be at least 2")
  if (Nclusters < 1) stop("termtree Nclusters should be between 1 and Nterms")
  
  org_pkg <- switch(
    as.character(map_organism),
    '9606' = "org.Hs.eg.db",
    '7227' = "org.Dm.eg.db",
    '9544' = "org.Mmu.eg.db",
    '10116' = "org.Rn.eg.db",
    '6239' = "org.Ce.eg.db",
    '10090' = "org.Mm.eg.db",
    NULL
  )

  ## processing genelist
  genelist <- sort(setNames(genelist$effectsize, genelist$gene), decreasing = TRUE)
  de <- as.numeric(names(genelist)[genelist > effectsize_threshold | genelist < -effectsize_threshold])
  if (length(de) < 2) {
    message("Not enough genes passed the effectsize threshold for termtree plot")
    return('no_genes_significant')
  }
  
  ## processing genesets
  term2gene <- genesets %>%
    dplyr::select(id, genes) %>%
    tidyr::unnest(genes)
  term2name <- genesets[, c("id", "name")]
  
  all_genes_background <- term2gene %>%
    dplyr::pull(genes) %>%    
    as.character() %>% # character needed for new 'enrichit' package function
    unique()
  
  ## run unspecific enrichment
  edo <- clusterProfiler::enricher(
    gene = de, 
    TERM2GENE = term2gene,
    TERM2NAME = term2name,
    universe = all_genes_background,
    pvalueCutoff = 1, 
    qvalueCutoff = 1, 
    minGSSize = 5,
    maxGSSize = 5000
  )
  
  ## formatting enrichment
  edox <- clusterProfiler::setReadable(edo, org_pkg, 'ENTREZID')
  edox2 <- enrichplot::pairwise_termsim(edox)
  edox2@result$p.adjust <- as.numeric(format(edox2@result$p.adjust, scientific = TRUE, digits = 3))
  edox2@result$zscore_sign <- ifelse(edox2@result$zScore >= 0, "Upregulation", "Downregulation")
  edox2@result$zscore_sign <- as.character(edox2@result$zscore_sign)
  if (Nterms > nrow(edox2@termsim)) Nterms <- nrow(edox2@termsim)
  if (Nclusters > Nterms) Nclusters <- Nterms
  
  ## termtreeplot: suppress cli::cli_alert_warning() - https://github.com/YuLab-SMU/tidytree/blob/1115d91fe184856b7d6f72ebb79fa29308dd7b60/R/as-tibble.R#L139
  withCallingHandlers(
    p <- enrichplot::treeplot(
      x = edox2,
      showCategory = Nterms,
      nCluster = Nclusters,
      label_format = 14,
    ), message = function(w) if (grepl("Invaild edge matrix", conditionMessage(w))) invokeRestart("muffleMessage")
  )
  p$data <- dplyr::left_join(
    p$data,
    edox2@result[, c("Description", "zscore_sign", "GeneRatio")],
    by = c("label" = "Description")
  )
  return(p)
}
