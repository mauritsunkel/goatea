library(goatea)

## manual coloring
css_colors <- list(
  main_bg = "#222222",
  darker_bg = "#111111",
  focus = "#32CD32", 
  hover = "#228B22",
  border = "#555555",
  text = "#FFFFFF"
)

## run the application
shiny::shinyApp(
  ui = goatea_ui,
  server = function(input, output, session) {
    goatea_server(
      input, output, session, 
      css_colors = css_colors)
  }
)
