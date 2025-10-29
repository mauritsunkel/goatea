library(goatea)

## manual coloring: dark mode
# css_colors <- list(
#   main_bg = "#222222",
#   darker_bg = "#111111",
#   focus = "#32CD32", 
#   hover = "#228B22",
#   border = "#555555",
#   text = "#FFFFFF"
# )

## manual coloring: light mode
css_colors <- list(
  main_bg = "white",
  darker_bg = "lightgray",
  focus = "#32CD32", 
  hover = "#228B22",
  border = "#999",
  text = "black"
)

## run the application
shiny::shinyApp(
  ui = goatea:::goatea_ui,
  server = function(input, output, session) {
    goatea:::goatea_server(
      input, output, session, 
      css_colors = css_colors)
  }
)

