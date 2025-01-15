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
                  tags$div(title="Select human or mouse for loading Gene Ontology Bioconductor genesets", # tooltip
                    column(width = 2, radioButtons("rb_human_mouse", "Organism:"))),
                ),
                fluidRow(
                  tags$div(title="Load DE data: .csv, .xlsx, .tsv", # tooltip
                    column(width = 2, actionButton("load_DE", "Load DE"))),
                  tags$div(title="Remove genes with non-available IDs", # tooltip
                  column(width = 2, checkboxInput("cbi_remove_NA_ids", "Remove NA IDs", value = TRUE))),
                  tags$div(title="Remove duplicated genes", # tooltip
                    column(width = 2, checkboxInput("cbi_remove_duplicated", "Remove duplicated", value = TRUE))),
                  tags$div(title="Remove Riken non-canonical mouse genes", # tooltip
                    column(width = 2, checkboxInput("cbi_remove_Rik_genes", "Remove Rik genes", value = TRUE))),
                  tags$div(title="Remove Gm non-canonical mouse genes", # tooltip
                  column(width = 2, checkboxInput("cbi_remove_Gm_genes", "Remove Gm genes", value = TRUE))),
                  tags$div(title=paste0("Filter down to max allowed n genes: ", max(goat::goat_nulldistributions$N)), # tooltip
                  column(width = 2, checkboxInput("cbi_keep_maxN_genes", "Keep max N genes", value = TRUE))),
                  column(width = 12, verbatimTextOutput("vto_filepaths_DE", placeholder = TRUE)),

                  # TODO just run GOAT, no need to load more files for user, always fast and up-to-date
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
  