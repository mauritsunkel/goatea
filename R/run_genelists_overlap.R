#' Create gene overview through overlapping genelists information by overlapping significant genes
#'
#' @param genelists UI value/list of tibbles/dataframes
#' @param annotate_genes default: UI value/TRUE, else FALSE to not add small gene descriptive annotations
#' @param annotation_organism default: UI value/"Hs" (Homo Sapiens), otherwise: "Mm" (Mus Musculus)
#'
#' @export
#'
#' @return tibble/dataframe with (annotated) genes and p-value/effectsize info for each genelist, concluding with overlapping genelists by significant genes
run_genelists_overlap <- function(
    genelists,
    annotate_genes = TRUE,
    annotation_organism = "Hs") {
  
  if ( ! all(sapply(genelists, function(x) "symbol" %in% colnames(x)))) {
    return("ERROR: column 'symbol' missing in genelists")
  }
  
  ## initiate df with unique gene symbols (and annotations)
  df <- tibble::tibble(
    symbol = Reduce(union, lapply(genelists, function(x) x$symbol)),
    gene = Reduce(union, lapply(genelists, function(x) x$gene)))
  if (annotate_genes) df$gene_annotation <- get_gene_annotation(gene_symbols = df$symbol, organism = annotation_organism)
  ## add expression and significance values of each genelist
  for (name in names(genelists)) {
    gene_i <- match(df$symbol, genelists[[name]]$symbol)
    df[, paste0(name, "-efsi")] <- genelists[[name]]$effectsize[gene_i] # effect size
    df[, paste0(name, "-pval")] <- genelists[[name]]$pvalue[gene_i] # p-value
  }
  ## list significant genes for efficiently checking overlap
  genelists_signif_genes <- lapply(genelists, function(df) df$symbol[df$signif == TRUE])
  ## get genelist names of overlapping significant genes
  df$genelist_overlap <- sapply(df$symbol, function(symbol) {
    hits <- names(genelists_signif_genes)[
      sapply(genelists_signif_genes, function(lst) symbol %in% lst)
    ]
    if (length(hits) == 0) "" else paste(hits, collapse = "+")
  })
  
  return(df)
}
