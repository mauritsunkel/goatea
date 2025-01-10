#' UI for goatea package
#'
#' @export
ui <- function() {
  dashboardPage(
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
                  column(width = 2, checkboxInput("cbi_remove_NA_ids", "Remove NA IDs", value = TRUE)),
                  column(width = 2, checkboxInput("cbi_remove_duplicated", "Remove duplicated", value = TRUE)),
                  column(width = 2, checkboxInput("cbi_remove_Rik_genes", "Remove Rik genes", value = TRUE)),
                  column(width = 2, checkboxInput("cbi_remove_Gm_genes", "Remove Gm genes", value = TRUE)),
                  column(width = 2, checkboxInput("cbi_keep_maxN_genes", "Keep max N genes", value = TRUE)),
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
}
  