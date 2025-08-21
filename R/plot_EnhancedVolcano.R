#' Plot EnhancedVolcano
#'
#' @param genelist UI value/list of tibbles/dataframes
#' @param effectsize_threshold numeric, default: 1, threshold for showing significance on effectsize axis
#' @param pvalue_threshold numeric, default: 0.05, threshold for showing significance on pvalue axis
#' @param background_color default: 'black', else character hexcolor or colorname
#' @param foreground_color default: 'white', else character hexcolor or colorname
#' @param interactive default: FALSE, else TRUE
#' 
#' @param legend_labels character vector, default: c('NS', 'FC', 'P', 'FC & P'), plot legend labels
#' @param x_label character, default: 'effectsize (FC)', plot x-axis label
#' @param y_label character, default: "'-log10(pvalue) (P)', plot y-axis label
#' @param title character, default: 'Volcano plot', plot title
#' @param subtitle character, default: 'EnhancedVolcano', plot subtitle
#' @param caption character, default: paste0("N genes: ", nrow(genelist)), plot caption
#' @param label_size numeric, default: 3, plot variable label size
#' @param legend_label_size numeric, default: 14, plot legend label size
#' @param axes_label_size numeric, default: 18, plot x- and y-axis lable sizes
#' @param point_size numeric, default: 2, plot point size
#'
#' @returns plotly or ggplot2 object
#' 
#' @export
#'
#' @importFrom EnhancedVolcano EnhancedVolcano
#' @importFrom ggplot2 coord_cartesian scale_x_continuous aes element_rect element_line element_text
#' @importFrom plotly ggplotly
#' 
#' @examples
#' plot_EnhancedVolcano(
#'   get(load(system.file("extdata", "example_genelist.rda", package = "goatea")))
#' )
plot_EnhancedVolcano <- function(
    genelist, 
    effectsize_threshold = 1, 
    pvalue_threshold = 0.05, 
    background_color = 'black', 
    foreground_color = 'white', 
    interactive = FALSE,
    legend_labels = c('NS', 'FC', 'P', 'FC & P'),
    x_label = 'effectsize (FC)',
    y_label = '-log10(pvalue) (P)',
    title = 'Volcano plot',
    subtitle = 'EnhancedVolcano',
    caption = paste0("N genes: ", nrow(genelist)),
    label_size = 3,
    legend_label_size = 14,
    axes_label_size = 18,
    point_size = 2) {
  x_axis_min <- floor(min(genelist$effectsize))
  x_axis_max <- ceiling(max(genelist$effectsize))
  
  p <- EnhancedVolcano::EnhancedVolcano(
    genelist,
    lab = genelist$symbol,
    x = "effectsize",
    y = "pvalue",
    title = title,
    subtitle = subtitle,
    caption = caption,
    pCutoff = pvalue_threshold,
    FCcutoff = effectsize_threshold,
    colAlpha = .9,
    labSize = label_size,
    axisLabSize = axes_label_size,
    pointSize = point_size,
    legendPosition = "right",
    legendIconSize = 4.5,
    legendLabSize = legend_label_size,
    legendLabels = legend_labels,
    xlab = x_label,
    ylab = y_label,
    gridlines.major = ifelse(interactive, TRUE, FALSE),
    gridlines.minor = ifelse(interactive, TRUE, FALSE),
    borderWidth = 1.5,
    boxedLabels = TRUE,
    labFace = "bold",
    drawConnectors = TRUE,
    arrowheads = FALSE,
    widthConnectors = 0.25
  ) + ggplot2::coord_cartesian(xlim=c(x_axis_min, x_axis_max)) +
    ggplot2::scale_x_continuous(
      breaks=seq(x_axis_min, x_axis_max, 1))
  
  if (interactive) {
    ## assume Shiny GOATEA environment if interactive
    for (i in seq_along(p$layers)) {
      if (inherits(p$layers[[i]]$geom, "GeomHline")) {
        p$layers[[i]]$aes_params$colour <- foreground_color
      }
      if (inherits(p$layers[[i]]$geom, "GeomText")) {
        p$layers[[i]]$aes_params$colour <- foreground_color
      }
    }
    
    p <- plotly::ggplotly(
      p + ggplot2::aes(key = .data$symbol, x = .data$effectsize, y = -log10(.data$pvalue)) +
        labs(
          x = 'effectsize (FC)',
          y = '-log10(pvalue) (P)',
          title = paste0('EnhancedVolcano - N genes: ', nrow(genelist)),  
          color = ''
        ) +
        theme(
          plot.background = element_rect(fill = background_color, color = NA),
          panel.background = element_rect(fill = background_color, color = NA),
          panel.grid = element_line(color = foreground_color),
          axis.text = element_text(color = foreground_color),
          axis.title = element_text(color = foreground_color),
          plot.title = element_text(color = foreground_color),
          plot.caption = element_text(color = foreground_color),
          legend.background = element_rect(fill = background_color),
          legend.text = element_text(color = foreground_color),
          legend.title = element_text(color = foreground_color)
        ),
      source = 'V'
    )
  }
  return(p)
}