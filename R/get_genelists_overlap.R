#' Create gene overview through overlapping genelists information by overlapping significant genes
#'
#' @param genelists UI value/list of tibbles/dataframes
#'
#' @export
#' 
#' @importFrom tibble tibble
#'
#' @returns tibble/dataframe with (annotated) genes and p-value/effectsize info for each genelist, concluding with overlapping genelists by significant genes
#'
#' @examples
#' run_genelists_overlap(list(A = goatea::example_genelist, B = goatea::example_genelist))
run_genelists_overlap <- function(genelists) {
  ## initiate df with unique gene symbols (and annotations)
  long_df <- do.call(rbind, genelists)
  df <- tibble::tibble(gene = Reduce(union, lapply(genelists, function(x) x$gene)))
  df$symbol <- long_df$symbol[match(df$gene, long_df$gene)] # match first hits, multiple possible if symbol to gene mapping happened
  if ('gene_annotation' %in% colnames(long_df)) df$gene_annotation <- long_df$gene_annotation[match(df$gene, long_df$gene)]

  ## add expression and significance values of each genelist
  for (name in names(genelists)) {
    gene_i <- match(df$symbol, genelists[[name]]$symbol)
    df[, paste0(name, "_efsi")] <- genelists[[name]]$effectsize[gene_i] # effect size
    df[, paste0(name, "_pval")] <- genelists[[name]]$pvalue[gene_i] # p-value
  }
  ## convert log2 fold effectsize values to percentage change
  df <- df %>%
    mutate(across(ends_with("_efsi"), ~ (2^. - 1) * 100, .names = "{.col}_perc")) %>%
    rename_with(~ gsub("_efsi_perc", "_perc", .), ends_with("_efsi_perc"))
  
  
  # TODO test 
  ## list significant genes for efficiently checking overlap
  genelists_signif_genes <- lapply(genelists, function(df) df$symbol[df$signif == TRUE])
  
  ## get genelist names of overlapping significant genes
  df$genelist_overlap <- sapply(df$symbol, function(symbol) {
    hits <- names(genelists_signif_genes)[
      sapply(genelists_signif_genes, function(lst) {symbol %in% lst})
    ]
    if (length(hits) == 0) "" else paste(hits, collapse = "+")
  })
  
  return(df)
}
