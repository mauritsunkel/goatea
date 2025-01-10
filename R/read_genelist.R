#' Read differential gene expression results .csvs/.xlsx to comparisons format
#'
#' @param filepaths named character vector of full filepathss of DE.csvs/DE.xlsx (output by enrichment_analysis())
#' @param remove_NA_ids boolean, default TRUE, if non-integers in gene column, remove
#' @param remove_duplicated boolean, default TRUE, removes duplicated gene symbols/ids
#' @param remove_Rik_genes boolean, default TRUE, grepl("Rik$") search and remove Riken non-canonical mouse genes
#' @param remove_Gm_genes boolean, default TRUE, grepl("^Gm") search and remove Gm non-canonical mouse genes
#' @param keep_maxN_genes boolean, default TRUE, filter down by pvalue to max n genes allowed by goat (max(goat::goat_nulldistributions$N))
#'
#' @export
read_genelist <- function(filepaths, remove_NA_ids = TRUE, remove_duplicated = TRUE,
                          remove_Rik_genes = TRUE, remove_Gm_genes = TRUE, keep_maxN_genes = TRUE) {
  message("Checking file format...")
  genelists <- list()
  
  for (i in seq_along(filepaths)) {
    file <- filepaths[i]
    
    if (file_extension(file) == "xlsx") {
      genelist <- openxlsx::read.xlsx(file)
    } else if (file_extension(file) == "csv") {
      genelist <- utils::read.csv2(file = file)
    } else if (file_extension(file) == "tsv") {
      genelist <- utils::read.delim(file, header = TRUE, sep = "\t")
    }
    genelist <- tibble::as_tibble(genelist)
    
    genelist <- validate_genelist(genelist = genelist)
    # return error message if validation not ok
    if (is.character(genelist)) return(paste(file, genelist))
    
    genelist <- date2gene(gene_names = genelist$symbol)
    
    # remove if NA after integer conversion of gene IDs
    if (remove_NA_ids) genelist <- genelist[ ! is.na(as.integer(genelist$gene)), ]
    if (remove_duplicated) {
      genelist <- genelist[ ! duplicated(genelist$symbol), ]
      genelist <- genelist[ ! duplicated(genelist$gene), ]
    }
    # remove Riken uncanonical mouse genes
    if (remove_Rik_genes) genelist <- genelist %>% filter( ! grepl("Rik$", symbol))
    # remove Gm uncanonical mouse genes
    if (remove_Gm_genes) genelist <- genelist %>% filter( ! grepl("^Gm", symbol))
    # filter down to max n rows based on lowest pvalue
    if (keep_maxN_genes) {
      genelist <- genelist %>% 
        arrange(pvalue) %>% 
        slice_head(n = max(goat::goat_nulldistributions$N))
    }
    
    genelists[[i]] <- genelist
  }
  
  return(genelists)
}
