#' Plot semantic similarity termtree 
#'
#' @param enrichment GOAT enrichment result dataframe
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
#' 
#' @examples
#' plot_termtree(goatea::example_enrichment, Nclusters = 3)
plot_termtree <- function(enrichment, Nterms = NA, Nwords = 5, Nclusters = 1) {
  if ( ! requireNamespace("enrichplot")) stop("Need 'enrichplot' package installed via: BiocManager::install('enrichplot')")
  if ("source" %in% colnames(enrichment)) if (length(unique(enrichment$source)) > 1) stop("pass a single source for plotting termtree") 
  df <- enrichment
  if (nrow(df) == 0) stop("Enrichment result passed to plot termtree has no results")
  if (is.na(Nterms) || Nterms > nrow(df)) Nterms <- nrow(df)
  if (Nterms < 2) stop("termtree Nterms should be at least 2")
  if (Nclusters < 1) stop("termtree Nclusters should be between 1 and Nterms")
  ## order by adjusted pvalue
  df <- df[order(df$pvalue_adjust), ]
  ## formatting
  df <- df %>%
    mutate(core_enrichment = purrr::map_chr(.data$genes, ~ paste(.x, collapse = "/"))) %>%
    rename(ID = .data$id, Description = .data$name, p.adjust = .data$pvalue_adjust)
  goatsea <- new("gseaResult", result = as.data.frame(df), geneSets = as.list(setNames(lapply(df$genes, as.character), df$ID)))
  rownames(goatsea@result) <- goatsea@result$ID
  ## calculate pairwise term similarity
  goatsea_termsim <- enrichplot::pairwise_termsim(goatsea)
  goatsea_termsim@result$p.adjust <- as.numeric(format(goatsea_termsim@result$p.adjust, scientific = TRUE, digits = 3))
  
  if (Nterms > nrow(goatsea_termsim@termsim)) Nterms <- nrow(goatsea_termsim@termsim)
  if (Nclusters > Nterms) Nclusters <- Nterms
  ## termtreeplot: suppress cli::cli_alert_warning() - https://github.com/YuLab-SMU/tidytree/blob/1115d91fe184856b7d6f72ebb79fa29308dd7b60/R/as-tibble.R#L139
  withCallingHandlers(
    p <- enrichplot::treeplot(
      goatsea_termsim,
      showCategory = Nterms,
      label_format = 14,
      cluster.params = list(label_words_n = Nwords, n = Nclusters)
    ), message = function(w) if (grepl("Invaild edge matrix", conditionMessage(w))) invokeRestart("muffleMessage")
  )
  return(p)
}
