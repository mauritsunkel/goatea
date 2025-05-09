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
#' plot_genelists_overlap_upsetjs(list(A = goatea::example_genelist, B = goatea::example_genelist))
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

#' Visualize genelists gene overlap in a Venn diagram
#' 
#' @param genelists UI value/list of tibbles/dataframes

#' @export 
#' 
#' @importFrom ggVennDiagram Venn process_data venn_regionedge venn_setedge venn_setlabel venn_regionlabel
#' @importFrom ggplot2 ggplot aes geom_polygon geom_path geom_text geom_label coord_equal scale_fill_manual theme_void scale_x_continuous expansion
#' 
#' @returns plot to render
#' 
#' @examples
#' plot_genelists_overlap_venn(list(A = goatea::example_genelist, B = goatea::example_genelist))
plot_genelists_overlap_venn <- function(genelists) {
  
  genelists_gene_overlaps <- lapply(genelists, function(df) df$gene[df$signif == TRUE])
  
  if (length(genelists) < 2 || length(genelists) > 4) {
    return("ERROR: can only plot Venn diagram for 2 to 4 genelists")
  }
  
  col <- c("#ff00ff", "olivedrab", "#7f0000", "#4b0082", "#ff0000",
           "orange", "#ffff00", "#00ff00", "#00fa9a", "#00ffff", "#0000ff",
           "darkslategray", "#6495ed", "#ffe4b5", "#ff69b4")
  
  venn <- ggVennDiagram::Venn(genelists_gene_overlaps)
  d <- ggVennDiagram::process_data(venn)
  
  df <- data.frame(
    name = d$regionData$name[order(d$regionData$name)],
    col = col[c(seq_len(length(d$regionData$name)))]
  )
  
  # DEVNOTE: for coloring edges: 1) where geom_polygon(aes(fill = id)) --> geom_polygon(aes(fill = I(col)), where col = metadata column
  # DEVNOTE: for coloring edges: 2) ggplot() + scale_fill_manual(values = col), where col = metadata column (of venn_region_edge)
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(ggplot2::aes(.data$X, .data$Y, fill = .data$id, group = .data$id), data = ggVennDiagram::venn_regionedge(d), show.legend = FALSE) +
    ggplot2::geom_path(ggplot2::aes(.data$X, .data$Y, color = .data$id, group = .data$id), data = ggVennDiagram::venn_setedge(d), show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(.data$X, .data$Y, label = .data$name), fontface = "bold", data = ggVennDiagram::venn_setlabel(d)) +
    ggplot2::geom_label(ggplot2::aes(.data$X, .data$Y, label = .data$count), data = ggVennDiagram::venn_regionlabel(d)) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::theme_void() + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .2))
  return(p)
}
