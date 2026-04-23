suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(shinyjqui)
  library(tidyr)
  library(plyr)
  library(dplyr)
  library(rlang)
  library(goat)
  library(openxlsx)
  library(upsetjs)
  library(ComplexHeatmap)
  library(InteractiveComplexHeatmap)
  library(purrr)
  library(ggplot2)
  library(plotly)
  library(igraph)
  library(visNetwork)
  library(arrow)
  library(htmltools)
  library(methods)
  library(AnnotationDbi)
  library(DT)
  library(tibble)
  library(rlang)
  library(DOSE)
  library(enrichplot)
  library(clusterProfiler)
  library(EnhancedVolcano)
})

## if on server (non-interactive local session): create a permanent 'null' canvas for plot background measurements
if (!interactive()) {
  pdf(NULL)
}

lapply(list.files("R", full.names = TRUE), source)

## manual coloring: light mode
css_colors <- list(
  main_bg = "white",
  darker_bg = "lightgray",
  focus = "#32CD32", 
  hover = "#228B22",
  border = "#999",
  text = "black"
)

## set max file size to ...MB for uploads (default: 10)
options(shiny.maxRequestSize = 1024^2 * 10)

## run the application
shiny::shinyApp(
  ui = goatea_ui,
  server = function(input, output, session) {
    goatea_server(
      input, output, session, 
      css_colors = css_colors)
  }
)
