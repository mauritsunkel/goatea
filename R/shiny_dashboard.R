# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinyjs)


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "GOATEA"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Load data", tabName = "load_data", icon = icon("dashboard")),
      menuItem("Run GOAT", tabName = "run_goat", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    useShinyjs(), # initialize shinyjs
    tabItems(
      tabItem(tabName = "load_data",
              fluidRow(
                column(width = 2, actionButton("load_DE", "Load DE")),
                column(width = 3, checkboxInput("remove_duplicated", "Remove duplicated", value = TRUE)),
                column(width = 12, verbatimTextOutput("filepaths_DE", placeholder = TRUE)),
                column(width = 2, actionButton("load_GOAT", "Load GOAT results (optional)")),
                column(width = 12, verbatimTextOutput("filepaths_GOAT", placeholder = TRUE)),
                box(
                  id = "run_goat_box",
                  title = "Run GOAT",
                  width = 12,
                  column(width = 2, hidden(actionButton("go_to_goat", "Go to GOAT")))
                )
              )
      ),
      tabItem(tabName = "run_goat",
              fluidRow(
                column(width = 2, actionButton("run_goat", "Run GOAT")),
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the file path
  filepaths_DE <- shiny::reactiveVal(NULL)
  go <- shiny::reactiveValues()
  
  # Observe the button click and open file browser
  shiny::observeEvent(input$load_DE, {
    filepaths <- utils::choose.files()
    genelists <- read_genelist(filepaths)
    for (i in seq_along(genelists)) {
      genelist <- genelists[[i]]
      val <- validate_genelist(genelist, remove_duplicated = input$remove_duplicated)
      if(is.character(val)) {
        shinyjs::runjs("$('#filepaths_DE').css('color', 'red');")
        filepaths_DE(paste(basename(filepaths[i]), val))
        shinyjs::hide("run_goat_tab")
        shinyjs::hide("go_to_goat")
        break
      } 
    }
    if(!is.character(val)) {
      shinyjs::runjs("$('#filepaths_DE').css('color', 'green');")
      filepaths_DE(basename(filepaths))
      shinyjs::show("go_to_goat")
      shinyjs::showElement(id = "run_goat_tab_out")
      go$data_genelist <- genelists
    }
  })
  
  
  # Display the selected file paths
  output$filepaths_DE <- shiny::renderText({
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

# Run the application
shiny::shinyApp(ui = ui, server = server)