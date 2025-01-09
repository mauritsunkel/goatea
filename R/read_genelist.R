#' Read differential gene expression results .csvs/.xlsx to comparisons format
#'
#' @param filepaths named character vector of full filepathss of DE.csvs/DE.xlsx (output by enrichment_analysis())
#'
#' @export
read_genelist <- function(filepaths) {
  message("Checking DE files format...")
  genelists <- list()
  
  for (i in seq_along(filepaths)) {
    DE_file <- filepaths[i]
    
    if (file_extension(DE_file) == "xlsx") {
      genelist <- openxlsx::read.xlsx(DE_file)
    } else if (file_extension(DE_file) == "csv") {
      genelist <- utils::read.csv2(file = DE_file)
    } else if (file_extension(DE_file) == "tsv") {
      genelist <- utils::read.delim(DE_file, header = TRUE, sep = "\t")
    }
    
    genelists[[i]] <- genelist
  }
  
  return(genelists)
}
