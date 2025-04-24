#' Plot EnhancedVolcano
#'
#' @param genelist UI value/list of tibbles/dataframes
#' @param effectsize_threshold numeric, default: 1, threshold for showing significance on effectsize axis
#' @param pvalue_threshold numeric, default: 0.05, threshold for showing significance on pvalue axis
#' @param interactive default: FALSE, else TRUE
#'
#' @returns plotly or ggplot2 object
#' 
#' @export
#'
#' @importFrom EnhancedVolcano EnhancedVolcano
#' @importFrom ggplot2 coord_cartesian scale_x_continuous aes element_rect element_line element_text
#' @importFrom plotly ggplotly
plot_EnhancedVolcano <- function(genelist, effectsize_threshold = 1, pvalue_threshold = 0.05, background_color = 'black', foreground_color = 'white', interactive = FALSE) {
  x_axis_min <- floor(min(genelist$effectsize))
  x_axis_max <- ceiling(max(genelist$effectsize))
  
  p <- EnhancedVolcano::EnhancedVolcano(
    genelist,
    lab = genelist$symbol,
    x = "effectsize",
    y = "pvalue",
    caption = paste0("N genes: ", nrow(genelist)),
    pCutoff = pvalue_threshold,
    FCcutoff = effectsize_threshold,
    colAlpha = .9,
    labSize = 3,
    legendPosition = "right",
    legendIconSize = 4.5,
    legendLabels = c('NS', 'FC', 'P', 'FC & P'),
    xlab = 'effectsize (FC)',
    ylab = '-log10(pvalue) (P)',
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
      p + ggplot2::aes(key = symbol, x = effectsize, y = -log10(pvalue)) +
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