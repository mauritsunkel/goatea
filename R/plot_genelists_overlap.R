#' Visualize genelists gene overlap in UpSet plot or Venn diagram
#' 
#' @param genelists UI value/list of tibbles/dataframes

#' @export 
#' 
#' @return plot to render
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
    col = col[c(1:length(d$regionData$name))]
  )
  
  # DEVNOTE: for coloring edges: 1) where geom_polygon(aes(fill = id)) --> geom_polygon(aes(fill = I(col)), where col = metadata column
  # DEVNOTE: for coloring edges: 2) ggplot() + scale_fill_manual(values = col), where col = metadata column (of venn_region_edge)
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(ggplot2::aes(X, Y, fill = id, group = id), data = ggVennDiagram::venn_regionedge(d), show.legend = F) +
    ggplot2::geom_path(ggplot2::aes(X, Y, color = id, group = id), data = ggVennDiagram::venn_setedge(d), show.legend = F) +
    ggplot2::geom_text(ggplot2::aes(X, Y, label = name), fontface = "bold",data = ggVennDiagram::venn_setlabel(d)) +
    ggplot2::geom_label(ggplot2::aes(X, Y, label = count), data = ggVennDiagram::venn_regionlabel(d)) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::theme_void() + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .2))
  return(p)
}

#' Visualize genelists gene overlap in UpSet plot or Venn diagram
#' 
#' @param genelists UI value/list of tibbles/dataframes
#' @param grayscale_colors default: UI valaue/FALSE, else TRUE, for colorful or grayscale degrees in UpSet plot 
#' @param empty_intersections default: UI/value/TRUE, if FALSE, shouldn't print intersects with 0 overlaps, error in UpSetR package
#' 
#' @export 
#' 
#' @return plot to render
plot_genelists_overlap_upset <- function(genelists, grayscale_colors = TRUE, empty_intersections = TRUE) {
  genelists_gene_overlaps <- lapply(genelists, function(df) df$gene[df$signif == TRUE])
  
  col <- c("#ff00ff", "olivedrab", "#7f0000", "#4b0082", "#ff0000",
           "orange", "#ffff00", "#00ff00", "#00fa9a", "#00ffff", "#0000ff",
           "darkslategray", "#6495ed", "#ffe4b5", "#ff69b4")
  
  set <- names(genelists_gene_overlaps)
  
  queries = list()
  combinations <- do.call(c, lapply(seq_along(set), combn, x = set, simplify = FALSE))
  
  for (i in seq_along(combinations)) {
    combi <- combinations[[i]]
    
    if ( ! grayscale_colors) {
      color <- col[i]
    } else {
      ## color by grayscale based on intersection degree
      color <- switch(
        length(combi),
        "grey0",
        "grey20",
        "grey40",
        "grey60",
        "grey80",
        "grey90"
      )
    }
    
    name <- switch(
      length(combi),
      "Degree 1",
      "Degree 2",
      "Degree 3",
      "Degree 4",
      "Degree 5",
      "Degree 6"
    )
    queries[[length(queries)+1]] <- list(query = UpSetR::intersects,
                                         params = combi,
                                         active = TRUE,
                                         color = color,
                                         query.name = name)
  }
  
  upd <- UpSetR::fromList(genelists_gene_overlaps)
  
  ## if intersection is not shown it is empty (no overlap)
  p <- UpSetR::upset(data = upd, sets = set, nintersects = NA, order.by = "freq", shade.color = "grey100",
                     sets.bar.color = "black", main.bar.color = "black", matrix.color = "black",
                     queries = queries, empty.intersections = empty_intersections, query.legend = "none",
                     mb.ratio = c(0.7, 0.3), text.scale = 1,
                     sets.x.label = "Significant genes",
                     set_size.show = T, set_size.scale_max = 1000)
  return(p)
}