#' Plot splitdot plot
#'
#' @param enrichment GOAT enrichment result
#' @param topN default: NA to plot all, else integer to plot topN terms by adjusted pvalue
#'
#' @returns ggplot2 object
#' @export
#' 
#' @importFrom ggplot2 ggplot aes geom_point scale_x_continuous scale_color_gradient scale_size_continuous theme_minimal labs facet_wrap theme element_rect element_text
#' 
#' @examples
#' plot_splitdot(
#'   get(load(system.file("extdata", "example_enrichment.rda", package = "goatea")))
#' )
plot_splitdot <- function(enrichment, topN = NA) {
  if ("source" %in% colnames(enrichment)) if (length(unique(enrichment$source)) > 1) stop("pass a single source for plotting splitdot") 
  if (is.na(topN) || topN > nrow(enrichment)) topN <- nrow(enrichment)
  if (topN < 2) stop("splitdot topN should be at least 2")
  
  enrichment$geneRatio <- enrichment$ngenes / enrichment$ngenes_input
  enrichment$zscore_sign <- ifelse(enrichment$zscore >= 0, "Upregulation", "Downregulation")
  
  p <- ggplot(enrichment[seq_len(topN),], aes(x = .data$geneRatio, y = reorder(.data$name, .data$geneRatio), size = .data$ngenes, color = .data$pvalue_adjust)) +
    geom_point() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_color_gradient(low = "#4B0055FF", high = "#FDE333FF", labels = function(x) format(x, scientific = TRUE, digits = 3)) + # viridis start-end colors
    scale_size_continuous(range = c(2, 10)) +
    theme_minimal() +
    labs(x = "GeneRatio", y = "", size = "Ngenes", color = "Adjusted p-value") +
    facet_wrap(~ .data$zscore_sign, scales = "free_x", drop = FALSE) +
    theme(
      axis.text.y = element_text(size = 17),
      axis.text.x = element_text(size = 17),
      axis.title.x = element_text(size = 18, face = "bold"),
      legend.title = element_text(size = 25, face = "bold"),
      legend.text = element_text(size = 20),
      strip.text = element_text(face = "bold", size = 23),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      strip.background = element_rect(fill = "grey90", color = "black", linewidth = 1),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      legend.position = "right"
    )
  return(p)
}
