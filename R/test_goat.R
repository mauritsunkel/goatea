#' My Function
#'
#' This function does something that requires the goat package.
#'
#' @export
test_goat <- function(genesets, genelist) {
  if( ! "package:goat" %in% search()) {
    attachNamespace("goat")
  }
  goat_result <- goat::test_genesets(
    genesets = genesets,
    genelist = genelist,
    method = "goat",
    score_type = "effectsize",
    padj_method = "BH",
    padj_sources = TRUE,
    padj_cutoff = 0.01, 
    padj_min_signifgenes = 0L
  )
  if("package:goat" %in% search()) {
    detach("package:goat")
  }
  return(goat_result)
}

