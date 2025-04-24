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
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap Legend draw
#' @importFrom purrr set_names
#' 
#' @export
plot_gene_effectsize_ComplexHeatmap <- function (
    genes, 
    genes_overview, 
    rows_dendrogram = TRUE, 
    cols_dendrogram = TRUE, 
    plot_n_genes = 50) 
{
  ## validate
  genes_ind <- match(genes, genes_overview$symbol)
  if (length(genes_ind) < 1) stop("no genes matching the gene overview")
  ## subset 
  go <- genes_overview[genes_ind, ]
  go_efsi <- go %>% select(symbol, ends_with("_efsi")) %>% rename_with(~ gsub("_efsi", "", .x))
  ## create heatmap matrix - keep max n genes based on highest abs(effectsize)
  mat <- go_efsi %>%
    mutate(max_abs_val = do.call(pmax, c(across(where(is.numeric), ~ abs(.x)), na.rm = TRUE))) %>%
    arrange(desc(abs(max_abs_val))) %>%
    slice_head(n = ifelse(is.null(plot_n_genes), nrow(go_efsi), plot_n_genes)) %>%
    tibble::column_to_rownames("symbol") %>%
    select(-max_abs_val) %>%
    as.matrix()
  ## create row p-value annotation
  go_pval <- go[match(rownames(mat), go$symbol),] %>% select(ends_with("_pval")) %>% rename_with(~ gsub("_pval", "", .x))
  col_fun <- colorify(colors = c("black", "white"), colors_breakpoints = range(go_pval, na.rm = TRUE))
  cha_row <- ComplexHeatmap::HeatmapAnnotation(
    which = "row",
    df = go_pval,
    col = purrr::set_names(rep(list(col_fun), ncol(go_pval)), colnames(go_pval)),
    show_legend = FALSE
  )
  ## plot
  ch <- ComplexHeatmap::Heatmap(
    mat,
    na_col = "white",
    name = "effectsize", # legend name
    cluster_rows = rows_dendrogram,
    cluster_columns = cols_dendrogram,
    left_annotation = cha_row,
  )
  range_vals <- pretty(range(go_pval, na.rm = TRUE))
  legend_pval <- ComplexHeatmap::Legend(
    title = "p-value",
    col_fun = col_fun,
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
