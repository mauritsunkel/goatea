## if on server (non-interactive local session): create a permanent 'null' canvas for plot background measurements
if (!interactive()) {
  pdf(NULL)
}

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
