# Internal shared theme for splitdot plots
.splitdot_theme <- function() {
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      axis.text.y        = ggplot2::element_text(size = 17),
      axis.text.x        = ggplot2::element_text(size = 17),
      axis.title.x       = ggplot2::element_text(size = 18, face = "bold"),
      legend.title       = ggplot2::element_text(size = 25, face = "bold"),
      legend.text        = ggplot2::element_text(size = 20),
      strip.text         = ggplot2::element_text(face = "bold", size = 23),
      panel.border       = ggplot2::element_rect(color = "black", fill = NA, linewidth = 1),
      strip.background   = ggplot2::element_rect(fill = "grey90", color = "black", linewidth = 1),
      panel.grid.major   = ggplot2::element_line(color = "grey80"),
      panel.grid.minor   = ggplot2::element_line(color = "grey90"),
      legend.position    = "right"
    )
  )
}

# Internal mode-branched axis/color scales for splitdot plots
.splitdot_mode_scales <- function(mode, df) {
  if (mode == "gene_ratio") {
    list(
      x_expr      = quote(.data$geneRatio),
      color_expr  = quote(.data$pvalue_adjust),
      x_label     = "GeneRatio",
      color_label = "Adjusted p-value",
      x_scale     = ggplot2::scale_x_continuous(limits = c(0, 1)),
      color_scale = ggplot2::scale_color_gradient(low = "#4B0055FF", high = "#FDE333FF", labels = function(x) format(x, scientific = TRUE, digits = 3))
    )
  } else {
    list(
      x_expr      = quote(.data$pvalue_adjust),
      color_expr  = quote(.data$geneRatio),
      x_label     = "Adjusted p-value",
      color_label = "GeneRatio",
      x_scale     = ggplot2::scale_x_continuous(limits = c(0, max(df$pvalue_adjust, na.rm = TRUE) * 1.05), labels = function(x) format(x, scientific = TRUE, digits = 3)),
      color_scale = ggplot2::scale_color_gradient(low = "#4B0055FF", high = "#FDE333FF")
    )
  }
}


#' Plot splitdot plot
#'
#' @param enrichment GOAT enrichment result, pre-sorted in the desired display order (top row appears at top of plot)
#' @param topN default: NA to plot all, else integer to plot topN terms (first N rows)
#' @param mode Character. Either \code{"gene_ratio"} (default) or \code{"pvalue"}.
#'   Controls x-axis and color legend only; row order is taken from the input data.
#'   \code{"gene_ratio"}: x = GeneRatio, color = adjusted p-value.
#'   \code{"pvalue"}: x = adjusted p-value, color = GeneRatio.
#' @param show_signif_overlay Logical. When \code{TRUE}, adds an inner black dot representing
#'   \code{ngenes_signif} on the same size scale as \code{ngenes}. Default: \code{TRUE}.
#'
#' @returns ggplot2 object. \code{attr(p, "suggested_width")} contains a suggested render width
#'   in inches based on the longest term label — pass to \code{ggsave(width = ...)} or
#'   \code{renderPlot(width = ... * 96)} in Shiny.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_x_continuous scale_color_gradient scale_size_continuous theme_minimal labs facet_wrap theme element_rect element_text element_line
#'
#' @examples
#' plot_splitdot(
#'   get(load(system.file("extdata", "example_enrichment.rda", package = "goatea")))
#' )
plot_splitdot <- function(enrichment, topN = NA, mode = "gene_ratio", show_signif_overlay = TRUE) {
  if ("source" %in% colnames(enrichment)) if (length(unique(enrichment$source)) > 1) stop("pass a single source for plotting splitdot")
  if (!mode %in% c("gene_ratio", "pvalue")) stop('mode must be "gene_ratio" or "pvalue"')
  if (is.na(topN) || topN > nrow(enrichment)) topN <- nrow(enrichment)
  if (topN < 2) stop("splitdot topN should be at least 2")

  enrichment$geneRatio <- enrichment$ngenes / enrichment$ngenes_input
  enrichment$zscore_sign <- ifelse(enrichment$zscore >= 0, "Upregulation", "Downregulation")

  df <- enrichment[seq_len(topN), ]
  sc <- .splitdot_mode_scales(mode, df)

  # Preserve input row order: first row appears at top of y-axis
  df$name <- factor(df$name, levels = rev(unique(as.character(df$name))))

  size_limits <- c(0, max(df$ngenes, na.rm = TRUE))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = !!sc$x_expr, y = .data$name, size = .data$ngenes, color = !!sc$color_expr)) +
    ggplot2::geom_point() +
    sc$x_scale +
    sc$color_scale +
    ggplot2::scale_size_continuous(range = c(2, 10), limits = size_limits) +
    ggplot2::labs(x = sc$x_label, y = "", size = "Ngenes", color = sc$color_label) +
    ggplot2::facet_wrap(~ .data$zscore_sign, scales = "free_x", drop = FALSE) +
    .splitdot_theme()

  if (show_signif_overlay) {
    p <- p +
      ggnewscale::new_scale("size") +
      ggplot2::geom_point(data = ~ subset(., ngenes_signif > 0), ggplot2::aes(size = .data$ngenes_signif), shape = 21, fill = "white", color = "black", stroke = 0.5) +
      ggplot2::scale_size_continuous(range = c(2, 10), limits = size_limits, breaks = pretty(range(df$ngenes_signif, na.rm = TRUE), n = 4), name = "Signif. genes")
  }

  attr(p, "suggested_width") <- max(10, max(nchar(as.character(df$name)), na.rm = TRUE) * 0.07 + 4)
  return(p)
}


#' Plot combined splitdot plot across GO sources
#'
#' Shows enrichment terms from GO:BP, GO:CC, and GO:MF in a 3-row x 2-column grid
#' (rows = source, columns = Upregulation/Downregulation). Within each source, highlighted
#' terms are shown first (top), followed by auto-filled top terms by adjusted p-value.
#'
#' @param enrichment GOAT enrichment result containing multiple GO sources
#' @param topN Integer. Number of terms per source. Slots filled first by \code{highlight_terms}
#'   matches, then by top terms ranked by adjusted p-value. Default: 5.
#' @param highlight_terms Character vector of term names or IDs to prioritize across all sources.
#'   Matched on \code{name} or \code{id} columns. Default: \code{NULL}.
#' @param mode Character. Either \code{"gene_ratio"} (default) or \code{"pvalue"}.
#'   Controls x-axis and color legend only, same as \code{\link{plot_splitdot}}.
#' @param show_signif_overlay Logical. When \code{TRUE}, adds an inner black dot for
#'   \code{ngenes_signif} on the same size scale as \code{ngenes}. Default: \code{TRUE}.
#'
#' @returns ggplot2 object. \code{attr(p, "suggested_width")} contains a suggested render width
#'   in inches — see \code{\link{plot_splitdot}}.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_x_continuous scale_color_gradient scale_size_continuous scale_y_discrete theme_minimal labs facet_grid theme element_rect element_text element_line
#'
#' @examples
#' get(load(system.file("extdata", "example_enrichment.rda", package = "goatea")))
#' example_enrichment$source <- rep(c("GO_BP", "GO_CC", "GO_MF"), 4)[1:10]
#' plot_splitdot_combined(example_enrichment, topN = 2)
plot_splitdot_combined <- function(enrichment, topN = 5, highlight_terms = NULL, mode = "gene_ratio", show_signif_overlay = TRUE) {
  accepted_sources <- c("GO_BP", "GO_CC", "GO_MF")

  if (!"source" %in% colnames(enrichment)) stop("enrichment must contain a 'source' column for plot_splitdot_combined")
  if (!mode %in% c("gene_ratio", "pvalue")) stop('mode must be "gene_ratio" or "pvalue"')
  if (topN < 1) stop("topN must be at least 1")

  present_sources <- intersect(unique(enrichment$source), accepted_sources)
  if (length(present_sources) == 0) stop("no GO:BP, GO:CC, or GO:MF sources found in enrichment")

  skipped <- setdiff(unique(enrichment$source), accepted_sources)
  if (length(skipped) > 0) warning("ignoring non-GO sources: ", paste(skipped, collapse = ", "))

  enrichment <- enrichment[enrichment$source %in% present_sources, ]
  enrichment$geneRatio <- enrichment$ngenes / enrichment$ngenes_input
  enrichment$zscore_sign <- ifelse(enrichment$zscore >= 0, "Upregulation", "Downregulation")

  subsets <- lapply(present_sources, function(src) {
    src_df <- enrichment[enrichment$source == src, ]
    src_df <- src_df[order(src_df$pvalue_adjust), ]

    highlighted_idx <- integer(0)
    if (!is.null(highlight_terms) && length(highlight_terms) > 0) {
      name_match <- which(src_df$name %in% highlight_terms)
      id_match   <- if ("id" %in% colnames(src_df)) which(src_df$id %in% highlight_terms) else integer(0)
      highlighted_idx <- unique(c(name_match, id_match))
    }

    remaining_idx <- setdiff(seq_len(nrow(src_df)), highlighted_idx)
    src_df[c(highlighted_idx, head(remaining_idx, max(0, topN - length(highlighted_idx)))), ]
  })

  df <- do.call(rbind, subsets)
  df$source <- factor(df$source, levels = accepted_sources[accepted_sources %in% present_sources])

  # Collect highlighted term names (including id-matched names)
  highlighted_names <- character(0)
  if (!is.null(highlight_terms) && length(highlight_terms) > 0) {
    id_matches <- if ("id" %in% colnames(df)) df$name[df$id %in% highlight_terms] else character(0)
    highlighted_names <- unique(c(highlight_terms, id_matches))
  }

  sc <- .splitdot_mode_scales(mode, df)

  # Preserve input row order: first row of each source appears at top of its panel
  df$name <- factor(df$name, levels = rev(unique(as.character(df$name))))

  # Build label vector: expression(bold(...)) for highlighted, plain string for others
  all_names <- levels(df$name)
  y_labels <- setNames(lapply(all_names, function(nm) {
    if (nm %in% highlighted_names) bquote(bold(.(nm))) else nm
  }), all_names)

  size_limits <- c(0, max(df$ngenes, na.rm = TRUE))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = !!sc$x_expr, y = .data$name, size = .data$ngenes, color = !!sc$color_expr)) +
    ggplot2::geom_point() +
    sc$x_scale +
    sc$color_scale +
    ggplot2::scale_size_continuous(range = c(2, 10), limits = size_limits) +
    ggplot2::scale_y_discrete(labels = y_labels) +
    ggplot2::labs(x = sc$x_label, y = "", size = "Ngenes", color = sc$color_label) +
    ggplot2::facet_grid(.data$source ~ .data$zscore_sign, scales = "free_y", drop = FALSE) +
    .splitdot_theme()

  if (show_signif_overlay) {
    p <- p +
      ggnewscale::new_scale("size") +
      ggplot2::geom_point(data = ~ subset(., ngenes_signif > 0), ggplot2::aes(size = .data$ngenes_signif), shape = 21, fill = "white", color = "black", stroke = 0.5) +
      ggplot2::scale_size_continuous(range = c(2, 10), limits = size_limits, breaks = pretty(range(df$ngenes_signif, na.rm = TRUE), n = 4), name = "Signif. genes")
  }

  attr(p, "suggested_width") <- max(10, max(nchar(as.character(df$name)), na.rm = TRUE) * 0.07 + 4)
  return(p)
}
