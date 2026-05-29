#' Plot gene2effectsize ComplexHeatmap
#'
#' @param genes character, genes to visualize
#' @param genes_overview dataframe, containing columns: 'symbol', 'SAMPLE_efsi' and 'SAMPLE_pval'
#' @param rows_dendrogram default: FALSE, TRUE to cluster rows and show dendrogram
#' @param cols_dendrogram default: FALSE, TRUE to cluster columns and show dendrogram
#' @param plot_n_genes integer, default: 50, NULL to plot all genes
#'
#' @returns ComplexHeatmap object
#'
#' @importFrom tibble column_to_rownames
#' @importFrom dplyr select ends_with rename_with mutate across where arrange desc slice_head
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap Legend draw
#' @importFrom purrr set_names
#' @importFrom grid unit
#'
#' @export
plot_gene_effectsize_ComplexHeatmap <- function(
    genes,
    genes_overview,
    rows_dendrogram = TRUE,
    cols_dendrogram = TRUE,
    plot_n_genes = 50
) {
  ## validate
  genes_ind <- match(genes, genes_overview$symbol)
  if (length(genes_ind) < 1) stop("no genes matching the gene overview")
  
  ## subset
  go <- genes_overview[genes_ind, ]
  go_efsi <- go |>
    dplyr::select(.data$symbol, dplyr::ends_with("_efsi")) |>
    dplyr::rename_with(~ gsub("_efsi", "", .x))
  
  ## create heatmap matrix — keep max n genes based on highest abs(effectsize)
  mat <- go_efsi |>
    dplyr::mutate(max_abs_val = do.call(pmax, c(dplyr::across(dplyr::where(is.numeric), ~ abs(.x)), na.rm = TRUE))) |>
    dplyr::arrange(dplyr::desc(abs(.data$max_abs_val))) |>
    dplyr::slice_head(n = ifelse(is.null(plot_n_genes), nrow(go_efsi), plot_n_genes)) |>
    tibble::column_to_rownames("symbol") |>
    dplyr::select(-.data$max_abs_val) |>
    as.matrix()
  
  ## diverging color scale: blue (negative) → white (0) → red (positive)
  efsi_range <- range(mat, na.rm = TRUE)
  col_efsi <- colorify(
    colors = c("blue", "white", "red"),
    colors_breakpoints = c(min(c(efsi_range[1], -1e-9)), 0, max(c(efsi_range[2], 1e-9)))
  )
  
  ## create row p-value annotation
  go_pval <- go[match(rownames(mat), go$symbol), ] |>
    dplyr::select(dplyr::ends_with("_pval")) |>
    dplyr::rename_with(~ gsub("_pval", "", .x))
  
  col_pval <- colorify(colors = c("black", "white"), colors_breakpoints = range(go_pval, na.rm = TRUE))
  
  cha_row <- ComplexHeatmap::HeatmapAnnotation(
    which = "row",
    df = go_pval,
    col = purrr::set_names(rep(list(col_pval), ncol(go_pval)), colnames(go_pval)),
    show_legend = FALSE
  )
  
  ## auto-scale width based on longest row label
  max_label_chars <- max(nchar(rownames(mat)))
  heatmap_width <- grid::unit(max_label_chars * 0.18 + 3, "cm")
  
  ## plot
  ch <- ComplexHeatmap::Heatmap(
    mat,
    col = col_efsi,
    na_col = "white",
    name = "effectsize",
    cluster_rows = rows_dendrogram,
    cluster_columns = cols_dendrogram,
    left_annotation = cha_row,
    width = heatmap_width
  )
  
  range_vals <- pretty(range(go_pval, na.rm = TRUE))
  legend_pval <- ComplexHeatmap::Legend(
    title = "p-value",
    col_fun = col_pval,
    at = range_vals,
    labels = range_vals
  )
  
  ch <- ComplexHeatmap::draw(
    ch,
    heatmap_legend_side = "right",
    annotation_legend_side = "right",
    annotation_legend_list = list(legend_pval)
  )
  
  return(ch)
}
