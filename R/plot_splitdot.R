#' Plot splitdot plot
#'
#' @param enrichment GOAT enrichment result
#' @param topN default: Inf to plot all, else integer to plot topN terms by adjusted pvalue
#'
#' @returns ggplot2 object
#' @export
plot_splitdot <- function(enrichment, topN = Inf) {
  if ("source" %in% colnames(enrichment)) {
    if (length(unique(enrichment$source)) > 1) stop("pass a single source for plotting splitdot") 
  }
  if (topN > nrow(enrichment)) topN <- nrow(enrichment)
  
  enrichment$geneRatio <- enrichment$ngenes / enrichment$ngenes_input
  enrichment$zscore_sign <- ifelse(enrichment$zscore >= 0, "Upregulation", "Downregulation")
  
  p <- ggplot(enrichment[1:topN,], aes(x = pvalue_adjust, y = reorder(name, -pvalue_adjust), size = ngenes, color = geneRatio)) +
    geom_point() +
    scale_x_continuous(trans = "log10") +
    scale_color_gradient(low = "blue", high = "red") +
    theme_minimal() +
    labs(x = "Adjusted P-value", y = "", size = "Ngenes", color = "Gene ratio") +
    facet_wrap(~ zscore_sign, scales = "free_x") +
    theme(axis.text.y = element_text(size = 10),
                   legend.position = "right",
                   panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                   strip.background = element_rect(fill = "grey90", color = "black", linewidth = 1),
                   strip.text = element_text(face = "bold"))
  return(p)
}
