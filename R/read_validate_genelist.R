#' Read and validate a table with genes (that should be tested in overrepresentation-analysis) for compatibility with this R package#'
#' @param file full filepath to gene tibble in .csvs/.xlsx/.tsv 
#' @param remove_non_numerical_ids boolean, default TRUE, if non-numerical in gene column, remove
#' @param remove_duplicated boolean, default TRUE, removes duplicated gene symbols/ids
#' @param remove_Rik_genes boolean, default TRUE, grepl("Rik$") search and remove Riken non-canonical mouse genes
#' @param remove_Gm_genes boolean, default TRUE, grepl("^Gm") search and remove Gm non-canonical mouse genes
#' @param map_organism default: NULL, if numeric taxid, used for selecting org.Xx.eg.db to map gene symbols to gene column via AnnotationDbi::mapIds(keytype = 'ALIAS') - if mapped to NA the genes are removed - need to download org.Xx.eg.db manually! Symbols are set toupper() to match formatting. Protein symbols could be used too. 
#' * 9606 = Human (Homo sapiens) (org.Hs.eg.db)
#' * 9544 = Rhesus monkey (Macaca mulatta) (org.Mmu.eg.db)
#' * 10090 = Mouse (Mus musculus) (org.Mm.eg.db)
#' * 10116 = Rat (Rattus norvegicus) (org.Rn.eg.db)
#' * 7227 = Fruit fly (Drosophila melanogaster) (org.Dm.eg.db)
#' * 6239 = Worm (Caenorhabditis elegans) (org.Ce.eg.db)
#'
#' @description
#' if 'pvalue' is not in the genelist columns, it is set and defaulted to 1 for visualization purposes 
#' if 'effectsize' is not in the genelist columns, it is set and defaulted to 0 for visualization purposes 
#'
#' @export
#' 
#' @importFrom openxlsx read.xlsx 
#' @importFrom tibble as_tibble
#' @importFrom AnnotationDbi mapIds
read_validate_genelist <- function(file, remove_non_numerical_ids = TRUE, remove_duplicated = TRUE,
                          remove_Rik_genes = TRUE, remove_Gm_genes = TRUE, map_organism = NULL) {
  message("Checking file format...")
  if (file_extension(file) == "xlsx") {
    genelist <- openxlsx::read.xlsx(file)
  } else if (file_extension(file) == "csv") {
    genelist <- utils::read.csv2(file = file)
  } else if (file_extension(file) == "tsv") {
    genelist <- utils::read.delim(file, header = TRUE, sep = "\t")
  }
  ## remove unnamed first column 
  if ("X" %in% colnames(genelist)) genelist[, "X"] <- NULL
  genelist <- tibble::as_tibble(genelist)
  
  ## if mapping: note that NA and empty genes will be removed from genelist
  if ( ! is.null(map_organism)) {
    if ( ! 'symbol' %in% colnames(genelist)) return("map_symbol_to_gene parameter on, yet no 'symbol' column found in genelist")

    org_pkg <- switch(
      as.character(map_organism),
      '9606' = "org.Hs.eg.db",
      '7227' = "org.Dm.eg.db",
      '9544' = "org.Mmu.eg.db",
      '10116' = "org.Rn.eg.db",
      '6239' = "org.Ce.eg.db",
      '10090' = "org.Mm.eg.db",
      NULL
    )
    if (is.null(org_pkg)) return("organism package for specified taxid is not available")
    if ( ! requireNamespace(org_pkg, quietly = TRUE)) stop(paste0('install organism package with: BiocManager::install("', org_pkg, '")'))
    org.xx.eg.db <- getExportedValue(org_pkg, org_pkg)
  
    message("mapping gene symbols via AnnotationDbi::mapIds ALIAS to NCBI Entrez IDs")
    genelist$gene <- AnnotationDbi::mapIds(org.xx.eg.db, keys = toupper(genelist$symbol), column = "ENTREZID", keytype = "ALIAS", multiVals = "first")
    warning(round(sum(is.na(genelist$gene)) / nrow(genelist), digits = 2), '% of symbols were not converted to ENTREZID')
    genelist <- genelist[ ! is.na(genelist$gene), ]
  }

  # 1) data.frame with all required columns
  ok = is.data.frame(genelist) && 
    nrow(genelist) > 0 &&
    'gene' %in% colnames(genelist)
  if ( ! 'pvalue' %in% colnames(genelist)) {
    warning("no 'pvalue' column in genelist: initializing with all 'pvalue' = 1")
    genelist$pvalue <- 1
  }
  if ( ! 'effectsize' %in% colnames(genelist)) {
    warning("no 'effectsize' column in genelist: initializing with all 'effectsize' = 0")
    genelist$effectsize <- 0
  }
  if ( ! 'symbol' %in% colnames(genelist)) {
    warning("no 'symbol' column in genelist, initializing with: genelist$symbol = genelist$gene")
    genelist$symbol <- genelist$gene
  }
  
  # 2) check column types
  if(ok) {
    types = sapply(genelist, typeof)
    ok = all(c("gene", "pvalue", "effectsize") %in% names(types)) &&
      types["gene"] %in% c("character", "integer", "numeric", "double") &&
      types["pvalue"] %in% c("integer", "numeric", "double") &&
      types["effectsize"] %in% c("integer", "numeric", "double")
  }
  if( ! ok) {
    return("genelist table should be a data.frame/tibble with these columns (and types); gene (character or integer) - consider mapping symbol to gene parameter")
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
  
  # remove if NA after integer conversion of gene IDs
  if (remove_non_numerical_ids) genelist <- genelist[ ! is.na(as.integer(genelist$gene)), ]
  
  # remove Riken uncanonical mouse genes
  if (remove_Rik_genes) genelist <- genelist %>% filter( ! grepl("Rik$", .data$gene))
  # remove Gm uncanonical mouse genes
  if (remove_Gm_genes) genelist <- genelist %>% filter( ! grepl("^Gm", .data$gene))

  return(genelist)
}
