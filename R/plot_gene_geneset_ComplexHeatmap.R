#' Plot ComplexHeatmap
#'
#' Plot ComplexHeatmap from enrichment analysis results and corresponding genelist
#'
#' @param enrichment_result dataframe containing enrichment analysis results.
#'   Must include `name` (gene set names) and `symbol` (listed genes associated with gene sets)
#' @param genelist dataframe with gene-level statistics, including at least
#'   `symbol`, `pvalue`, `effectsize`, and `signif` columns
#' @param genes character, default: NULL, if genes given, these are prioritized for visualization
#' @param n_cluster default: 1, integer, number of hierarchical clusters to define
#' @param cluster_method default: 'single', else one of \link{hclust} methods
#' @param n_top_terms default: NULL, if integer, plot only top genesets (recommended for visual clarity: 70)
#' @param n_top_genes default: NULL, if integer, plot only top genes (recommended for visual clarity: 150)
#' @param genelist_overlap (Optional) dataframe with gene overlap information, including
#'   `symbol` and `genelist_overlap`, see run_genelists_overlap()
#' @param plot default: FALSE, if TRUE, display drawn ComplexHeatmap
#'
#' @return A **ComplexHeatmap** object displaying genesets (rows) and genes (columns),
#'   potentially clustered based on their binary associations. The heatmap includes:
#'   - Row annotations: Gene set size, p-value, and average effect size.
#'   - Column annotations: Gene p-values, effect sizes, and optional overlap categories.
#'   - Customized row/column labels highlighting significant elements.
#'   - A color-mapped heatmap showing clustering results.
#'
#' @export 
#' 
#' @importFrom tidyr unnest
#' @importFrom ComplexHeatmap Heatmap HeatmapAnnotation draw
#' @importFrom grid gpar
#'
#' @examples
#' \dontrun{
#' enrichment_result <- tibble::tibble(
#' name = c("Pathway1", "Pathway2", "Pathway3", "Pathway4"),
#' symbol = list(c("GeneA", "GeneB"), c("GeneC", "GeneD"),c("GeneA", "GeneB"), c("GeneC", "GeneD")),
#' pvalue_adjust = c(0.01, 0.05, 0.5, 0.8),
#' ngenes = c(10, 20, 30, 100),
#' signif = c(T,T,F,F)
#' )
#' genelist <- data.frame(
#'   symbol = c("GeneA", "GeneB", "GeneC", "GeneD"),
#'   pvalue = c(0.01, 0.05, 0.5, 0.8),
#'   effectsize = c(1.2, -0.5, 0.8, -1.1),
#'   signif = c(TRUE, FALSE, FALSE, FALSE)
#' )
#' plot_ComplexHeatmap(enrichment_result, genelist, n_cluster = 2)
#' }
plot_ComplexHeatmap <- function(
    enrichment_result,
    genelist,
    genes = NULL,
    cluster_method = 'single',
    n_cluster = 1,
    n_top_terms = NA,
    n_top_genes = NA,
    genelist_overlap = NULL,
    plot = FALSE) {
  
  stopifnot(
    is.numeric(n_cluster) & n_cluster > 0,
    nrow(enrichment_result) > 0,
    all(c("name", "symbol", "pvalue_adjust", "ngenes", "signif") %in% colnames(enrichment_result)),
    all(c("symbol", "effectsize", "pvalue", "signif") %in% colnames(genelist)),
    length(unique(unlist(enrichment_result$symbol))) > 1,
    is.na(c(n_top_genes, n_top_terms)) | is.numeric(c(n_top_genes, n_top_terms)),
    is.logical(plot)
  )
  
  ## variable name convenience
  df <- enrichment_result
  rm(enrichment_result)
  
  ## plot only top n genesets
  if ( ! is.na(n_top_terms)) {
    if (n_top_terms > nrow(df)) n_top_terms <- nrow(df)
    df <- df[order(abs(df$zscore), decreasing = TRUE), ]
    df <- df[1:n_top_terms, ]
  }
  
  ## initialize matrix data
  m_data <- df %>%
    select(.data$name, .data$symbol) %>%
    tidyr::unnest(.data$symbol)
  ncount_genes <- table(m_data$symbol)
  unique_names <- unique(m_data$name)
  unique_genes <- unique(m_data$symbol)

  ## plot only top n genes - based on genes showing in most terms and then based on effectsize
  if ( ! is.na(n_top_genes)) {
    # order by count first, then by abs(effectsize)
    matched_genelist <- genelist[match(unique_genes, genelist$symbol), ]
    matched_genelist$ncount <- ncount_genes[matched_genelist$symbol]
    matched_genelist <- matched_genelist[order(-matched_genelist$ncount, -abs(matched_genelist$effectsize)), ]
    if (n_top_genes > nrow(matched_genelist)) n_top_genes <- nrow(matched_genelist)
    if ( ! is.null(genes)) {
      if (length(genes) >= n_top_genes) {
        unique_genes <- genes
      } else {
        unique_genes <- c(genes, matched_genelist$symbol[1:(n_top_genes-length(genes))])
      }
    } else {
      unique_genes <- matched_genelist$symbol[1:n_top_genes]
    }
    
    m_data <- m_data[m_data$symbol %in% unique_genes, ]
  }
  
  ## setup data matrix
  m <- matrix(
    0,
    nrow = length(unique_names),
    ncol = length(unique_genes),
    dimnames = list(unique_names, unique_genes)
  )
  for(i in 1:nrow(m_data)) {
    row_name <- m_data$name[i]
    col_gene <- m_data$symbol[i]
    m[row_name, as.character(col_gene)] <- 1
  }
  
  ## hierarchical clustering and group by tree cut
  if (nrow(m) > 1) {
    hc_terms <- hclust(dist(m, method = "binary"), cluster_method)
    if (n_cluster <= nrow(m)) {
      hc_terms_clusters <- cutree(hc_terms, n_cluster)
      ## set term cluster values in matrix
      for (term in names(hc_terms_clusters)) {
        cluster <- hc_terms_clusters[term]
        m[term, ][m[term, ] == 1] <- cluster
      }
    }
  }
  if (ncol(m) > 1) {
    hc_genes <- hclust(dist(t(m), method = "binary"), cluster_method)
    if (n_cluster <= ncol(m)) {
      hc_genes_clusters <- cutree(hc_genes, n_cluster)
    }
  }
  ## set matrix order by term clustering if applicable, initial 0 to NA, and set values to character for discrete coloring
  if (exists("hc_terms")) {
    m <- m[c(hc_terms$order), ]
    ## order enrichment for coloring
    df <- df[c(hc_terms$order),]
  }
  if (exists("hc_genes")) {
    m <- m[, c(hc_genes$order)]
    ## get unique genes and order by gene clustering
    unique_genes_i <- match(unique_genes, genelist$symbol)[hc_genes$order]
  }
  m[m == 0] <- NA
  m <- matrix(as.character(m), nrow = nrow(m), ncol = ncol(m), dimnames = list(rownames(m), colnames(m)))
  
  ## set annotations
  neg_color <- "blue"
  neutral_color <- "white"
  pos_color <- "red"
  effect_color <- "black"
  ## calculate average effectsize, correlates with zscore, yet holds more intuitive information
  df$effectsize_average <- sapply(df$symbol, function(x) {
    mean(genelist$effectsize[match(x, genelist$symbol)])
  })
  ## set term annotation
  cha_row  <- ComplexHeatmap::HeatmapAnnotation(
    which = "row",
    zscore = df$zscore,
    GeneSetSize = df$ngenes,
    pvalue = df$pvalue_adjust,
    signif = as.numeric(df$signif),
    col = list(
      zscore = colorify(colors = c(neg_color, neutral_color, pos_color), colors_breakpoints = c(min(df$zscore, na.rm = T), 0, max(df$zscore, na.rm = T))),
      GeneSetSize = colorify(colors = c(neutral_color, effect_color), colors_breakpoints = c(0, max(df$ngenes, na.rm = T))),
      pvalue = colorify(colors = c("black", "white"), colors_breakpoints = c(0, 1)),
      signif = colorify(colors = c(effect_color, neutral_color), colors_breakpoints = c(1, 0))
    ),
    na_col = neutral_color
  )
  ## set gene annotation
  cha_column <- ComplexHeatmap::HeatmapAnnotation(
    signif = as.numeric(genelist$signif[unique_genes_i]),
    pvalue = genelist$pvalue[unique_genes_i],
    effectsize = genelist$effectsize[unique_genes_i],
    col = list(
      signif = colorify(colors = c(effect_color, neutral_color), colors_breakpoints = c(1, 0)),
      pvalue = colorify(colors = c(effect_color, neutral_color), colors_breakpoints = c(0, 1)),
      effectsize = colorify(colors = c(neg_color, neutral_color, pos_color), colors_breakpoints = c(min(genelist$effectsize[unique_genes_i], na.rm = T), 0, max(genelist$effectsize[unique_genes_i], na.rm = T)))
    ),
    na_col = neutral_color
  )
  if ( ! is.null(genelist_overlap)) {
    overlap_values <- genelist_overlap$genelist_overlap[match(colnames(m), genelist_overlap$symbol)]
    cha_column_overlap <- ComplexHeatmap::HeatmapAnnotation(
      signif_overlap = overlap_values,
      col = list(
        signif_overlap = colorify(n = length(unique(overlap_values)), colors = "okabe-ito", colors_names = replace(unique(overlap_values), is.na(unique(overlap_values)), "?"))
      ),
      na_col = neutral_color
    )
    cha_column <- c(cha_column, cha_column_overlap)
  }
  
  ## DEPRECATED devnote: highlighting labels in the sub-InteractiveComplexHeatmap mismatches them from the original ICH
  # ## highlight significant terms and genes
  # row_colors <- setNames(rep("black", length(df$name)), df$name)
  # row_colors[df$signif] <- "green4"
  # row_fontface <- rep("plain", length(df$name))
  # row_fontface[df$signif] <- "bold.italic"
  # col_colors <- setNames(rep("black", length(unique_genes)), unique_genes)
  # col_colors[genelist$signif[unique_genes_i]] <- "green4"
  # col_fontface <- rep("plain", length(unique_genes))
  # col_fontface[genelist$signif[unique_genes_i]] <- "bold.italic"
  
  ## set unique matrix values
  m_unique_values <- unique(m[ ! is.na(m)])
  ## create heatmap
  ch <- ComplexHeatmap::Heatmap(
    m,
    col = colorify(n = length(m_unique_values), colors = "viridis", colors_names = sort(m_unique_values)),
    na_col = "grey20",
    name = "clusters", # legend name
    rect_gp = grid::gpar(col = "grey30"),
    show_row_names = TRUE,
    show_column_names = TRUE,
    # row_names_gp = gpar(fontsize = 12), # grid::gpar(col = row_colors, fontface = row_fontface),
    # column_names_gp = gpar(fontsize = 12), # grid::gpar(col = col_colors, fontface = col_fontface),
    column_names_side = "top",
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    bottom_annotation = cha_column,
    left_annotation = cha_row,
  )
  ch <- ComplexHeatmap::draw(ch, heatmap_legend_side = "bottom", annotation_legend_side = "bottom", merge_legend = TRUE)
  if (plot) ch
  
  return(ch)
}
