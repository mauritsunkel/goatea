#' Visualize genelists gene overlap in an interactive UpSet plot
#'
#' @param genelists UI value/list of tibbles/dataframes
#' @param mode string, default: 'intersect', else 'distinct' or 'union' - how to overlap the listed genes
#' @param interactive default: FALSE, else TRUE
#' @param main.color default: 'white' else character hexcolor or colorname
#' @param highlight.color default: 'green' else character hexcolor or colorname
#' 
#' @returns upset plot
#' 
#' @export 
#' 
#' @importFrom upsetjs upsetjs generateIntersections generateDistinctIntersections generateUnions chartTheme interactiveChart
#'
#' @description
#' UpSetJS examples: https://upset.js.org/integrations/r/articles/combinationModes.html#distinct-intersection-mode
#' 
#' @examples
#' plot_genelists_overlap_upsetjs(list(
#'   A = get(load(system.file("extdata", "example_genelist.rda", package = "goatea"))), 
#'   B = get(load(system.file("extdata", "example_genelist.rda", package = "goatea")))
#' ))
plot_genelists_overlap_upsetjs <- function(genelists, mode = 'distinct', interactive = FALSE, main.color = 'black', highlight.color = 'green') {
  col <- c("#ff00ff", "olivedrab", "#7f0000", "#4b0082", "#ff0000",
           "orange", "#ffff00", "#00ff00", "#00fa9a", "#00ffff", "#0000ff",
           "darkslategray", "#6495ed", "#ffe4b5", "#ff69b4")
  
  genelists_gene_overlaps <- lapply(genelists, function(df) df$gene[df$signif == TRUE])
  upset <- upsetjs::upsetjs() %>% upsetjs::fromList(genelists_gene_overlaps)
  
  if (mode == 'intersect') {
    upset <- upset %>% upsetjs::generateIntersections()
  } else if (mode == 'distinct') {
    upset <- upset %>% upsetjs::generateDistinctIntersections()
  } else if (mode == 'union') {
    upset <- upset %>% upsetjs::generateUnions()
  } else {
    upset <- upsetjs::upsetjs()
  }
  
  upset <- upset %>% upsetjs::chartTheme('dark', color = main.color, text.color = main.color, selection.color = highlight.color)

  if (interactive) upset <- upset %>% upsetjs::interactiveChart()
  
  return(upset)
}
