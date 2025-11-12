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
#' @importFrom rlang .data
#' 
#' @examples
#' plot_termtree(
#'   get(load(system.file("extdata", "example_enrichment.rda", package = "goatea"))), 
#'   Nclusters = 3
#' )
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
  ## set Nterms 
  df <- df[seq.int(1, Nterms),]
  ## formatting
  df <- df %>%
    mutate(core_enrichment = purrr::map_chr(.data$genes, ~ paste(.x, collapse = "/"))) %>%
    dplyr::rename(ID = "id", Description = 'name', p.adjust = 'pvalue_adjust')
  goatsea <- new("gseaResult", result = as.data.frame(df), geneSets = as.list(setNames(lapply(df$genes, as.character), df$ID)))
  rownames(goatsea@result) <- goatsea@result$ID
  ## calculate pairwise term similarity
  goatsea_termsim <- enrichplot::pairwise_termsim(goatsea)
  goatsea_termsim@result$p.adjust <- as.numeric(format(goatsea_termsim@result$p.adjust, scientific = TRUE, digits = 3))
  goatsea_termsim@result$geneRatio <- goatsea_termsim@result$ngenes / goatsea_termsim@result$ngenes_input
  goatsea_termsim@result$zscore_sign <- ifelse(goatsea_termsim@result$zscore >= 0, "Upregulation", "Downregulation")
  goatsea_termsim@result$zscore_sign <- as.character(goatsea_termsim@result$zscore_sign)
  
  if (Nterms > nrow(goatsea_termsim@termsim)) Nterms <- nrow(goatsea_termsim@termsim)
  if (Nclusters > Nterms) Nclusters <- Nterms
  ## termtreeplot: suppress cli::cli_alert_warning() - https://github.com/YuLab-SMU/tidytree/blob/1115d91fe184856b7d6f72ebb79fa29308dd7b60/R/as-tibble.R#L139
  withCallingHandlers(
    p <- enrichplot::treeplot(
      goatsea_termsim,
      showCategory = Nterms,
      color = NULL,
      label_format = 14,
      leave_fontsize = 9,
      clade_fontsize = 7,
      cluster.params = list(label_words_n = Nwords, n = Nclusters)
    ), message = function(w) if (grepl("Invaild edge matrix", conditionMessage(w))) invokeRestart("muffleMessage")
  )
  p$data <- dplyr::left_join(
    p$data,
    goatsea_termsim@result[, c("Description", "zscore_sign", "geneRatio")],
    by = c("label" = "Description")
  )
  p <- p +
    ggplot2::geom_point(
      data = subset(p$data, ! is.na(p$data$zscore_sign)),
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        shape = .data$zscore_sign,
        size = .data$count,          # <— size by number of genes
        color = .data$geneRatio
      )
    ) +
    ggplot2::scale_shape_manual(
      values = c(Upregulation = 17, Downregulation = 16),
      na.translate = FALSE
    )
  return(p)
}
