#' Read and validate a table with genes (that should be tested in overrepresentation-analysis) for compatibility with this R package#'
#' @param file full filepath to gene tibble in .csvs/.xlsx/.tsv 
#' @param remove_NA_ids boolean, default TRUE, if non-integers in gene column, remove
#' @param remove_duplicated boolean, default TRUE, removes duplicated gene symbols/ids
#' @param remove_Rik_genes boolean, default TRUE, grepl("Rik$") search and remove Riken non-canonical mouse genes
#' @param remove_Gm_genes boolean, default TRUE, grepl("^Gm") search and remove Gm non-canonical mouse genes
#' @param keep_maxN_genes boolean, default TRUE, filter down by pvalue to max n genes allowed by goat (max(goat::goat_nulldistributions$N))
#'
#' @export
read_validate_genelist <- function(file, remove_NA_ids = TRUE, remove_duplicated = TRUE,
                          remove_Rik_genes = TRUE, remove_Gm_genes = TRUE, keep_maxN_genes = TRUE) {
  message("Checking file format...")
  
  if (file_extension(file) == "xlsx") {
    genelist <- openxlsx::read.xlsx(file)
  } else if (file_extension(file) == "csv") {
    genelist <- utils::read.csv2(file = file)
  } else if (file_extension(file) == "tsv") {
    genelist <- utils::read.delim(file, header = TRUE, sep = "\t")
  }
  genelist <- tibble::as_tibble(genelist)
  
  # 1) data.frame with all required columns
  ok = is.data.frame(genelist) &&
    nrow(genelist) > 0 &&
    all(c("gene", "signif") %in% colnames(genelist))
  
  # 2) check column types
  if(ok) {
    types = sapply(genelist, typeof)
    ok = all(c("gene", "signif") %in% names(types)) &&
      types["gene"] %in% c("character", "integer", "numeric", "double") &&
      types["signif"] == "logical"
  }
  if(!ok) {
    return("genelist table should be a data.frame/tibble with these columns (and types); gene (character or integer), signif (logical/boolean)")
  }
  
  # 3) check for NA or empty-string values  (note that the 'signif' column can be NA)
  ok = FALSE
  if(is.character(genelist$gene)) {
    ok = !anyNA(genelist$gene) && all(genelist$gene != "")
  }
  if(is.integer(genelist$gene) || is.numeric(genelist$gene)) { # also allow 'numeric' to relax compatability a bit
    ok = all(is.finite(genelist$gene)) # disallow NA and Inf
  }
  if(!ok) {
    return("genelist table should not contain empty/missing/NA/Inf values in the 'gene' column")
  }
  
  # 4) for numeric type gene identifiers we allow double-type but require integer values --> validate & type conversion
  # this is just for user convenience, ideally we'd require strict value types but that might force users into
  # type conversion mistakes so we'll handle this here.
  # e.g. user might have integer IDs from NCBI Entrez, but unwittingly store them in a numeric/double column type
  # here we test that conversion to integer doesn't lose information (i.e. numeric-type IDs have no decimal values)
  if( ! is.integer(genelist$gene) && is.numeric(genelist$gene)) {
    # before conversion to int, we should round() because integer conversion drops all decimals (rounds down)
    # see also the help/documentation @ ?as.integer
    # testcase;  tmp = 1/(1-0.99); sprintf("%.14f", tmp); as.integer(tmp); all.equal(as.integer(tmp), tmp)
    gene_as_int = as.integer(round(genelist$gene, digits = 0))
    if(any(abs(genelist$gene - gene_as_int) > 10^-6)) { # test with some minor tolerance for imprecision
      return("genelist table should contain whole numbers (integers) in the 'gene' column, if numeric identifiers are provided")
    }
    # alternatively, check without tolerance;
    # stopifnot(genelist$gene %% 1 == 0); genelist$gene = as.integer(genelist$gene)
    
    # type conversion
    genelist$gene = gene_as_int
  }
  
  # 5) genes cannot be duplicated
  if (remove_duplicated) genelist <- genelist[ ! duplicated(genelist$gene), ]
  if(anyDuplicated(genelist$gene)) {
    return("genelist table should not contain duplicate values in the 'gene' column")
  }
  
  # 6) genelist length cannot exceed maximum allowed by precomputed null distributions
  if (keep_maxN_genes) {
    genelist <- genelist %>% 
      arrange(pvalue) %>% 
      slice_head(n = max(goat::goat_nulldistributions$N))
  }
  if (length(genelist$gene) > max(goat::goat_nulldistributions$N)) {
    return(paste0("genelist table should not exceed ", max(goat::goat_nulldistributions$N), " genes (", length(genelist$gene), ")"))
  }
  
  # remove if NA after integer conversion of gene IDs
  if (remove_NA_ids) genelist <- genelist[ ! is.na(as.integer(genelist$gene)), ]
  
  # remove Riken uncanonical mouse genes
  if (remove_Rik_genes) genelist <- genelist %>% filter( ! grepl("Rik$", symbol))
  # remove Gm uncanonical mouse genes
  if (remove_Gm_genes) genelist <- genelist %>% filter( ! grepl("^Gm", symbol))
  # filter down to max n rows based on lowest pvalue
  
  return(genelist)
}
