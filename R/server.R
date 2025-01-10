
#' Server for goatea package
#'
#' @param input Shiny input elements handling
#' @param output Shiny input elements handling
#' @param session Shiny handling reactivity in app
#' 
#' @import shiny
#' @import shinyjs
#' @import shinydashboard
#'
#' @export
server <- function(input, output, session) {
  # Reactive value to store the file path
  filepaths_DE <- shiny::reactiveVal(NULL)
  go <- shiny::reactiveValues()
  
  # Observe the button click and open file browser
  shiny::observeEvent(input$load_DE, {
    filepaths <- utils::choose.files()
    genelists <- read_genelist(
      filepaths, 
      remove_NA_ids = input$cbi_remove_NA_ids, 
      remove_duplicated = input$cbi_remove_duplicated,
      remove_Rik_genes = input$cbi_remove_Rik_genes,
      remove_Gm_genes = input$cbi_remove_Gm_genes,
      keep_maxN_genes = input$cbi_keep_maxN_genes)
    
    if(is.character(genelists)) {
      shinyjs::runjs("$('#filepaths_DE').css('color', 'red');")
      filepaths_DE(paste(genelists))
      shinyjs::hide("run_goat_tab")
      shinyjs::hide("go_to_goat")
    } else {
      shinyjs::runjs("$('#filepaths_DE').css('color', 'green');")
      filepaths_DE(paste(basename(filepaths), sapply(genelists, nrow)))
      shinyjs::show("go_to_goat")
      shinyjs::showElement(id = "run_goat_tab_out")
      go$data_genelist <- genelists
    }
  })
  
  
  # Display the selected file paths
  output$vto_filepaths_DE <- shiny::renderText({
    shiny::req(filepaths_DE())
    paste(filepaths_DE(), collapse = "\n")
  })
  
  # Navigate to the "Run GOAT" tab when the "Run GOAT" button is clicked
  shiny::observeEvent(input$go_to_goat, {
    shinydashboard::updateTabItems(session, "tabs", "run_goat")
  })
  
  # TODO add doc
  shiny::observeEvent(input$run_goat, {
    # TODO run GOAT function
    # TODO save results to go$goat_results
    
  })
}