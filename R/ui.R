#' UI for goatea package
#'
#' @export
#' 
#' @importFrom shiny addResourcePath fluidRow column selectInput textInput actionButton textOutput checkboxInput radioButtons fileInput verbatimTextOutput numericInput downloadButton textAreaInput plotOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom shinydashboard box tabItem tabItems dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem menuSubItem dashboardBody
#' @importFrom htmltools tags div
#' @importFrom plotly plotlyOutput
#' @importFrom upsetjs upsetjsOutput
#' @importFrom DT DTOutput
#' @importFrom shinyjqui jqui_resizable
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapOutput
#' @importFrom visNetwork visNetworkOutput
goatea_ui <- function() {
  ## adds custom css styling and Javascript functionalities
  shiny::addResourcePath("www", system.file("www", package = "goatea"))
  
  dashboardPage(
    dashboardHeader(title = "GOATEA"),
    dashboardSidebar(
      sidebarMenu(
        id = "menu_tabs",
        menuItem("Initialize", tabName = "global_initialize", icon = icon("dashboard"),
                 menuSubItem("Load data", tabName = "menu_initialize", selected = TRUE)
        ),
        menuItem("Pre-enrichment plots", tabName = "global_preplot", icon = icon("dashboard"),
                 menuSubItem("Volcano", tabName = "menu_run_volcano"),
                 menuSubItem("Overlap", tabName = "menu_run_genelist_overlap")
        ),
        menuItem("Run enrichment", tabName = "global_enrichment", icon = icon("dashboard"),
                 menuSubItem("Gene set enrichment", tabName = "menu_run_enrichment")
        ),
        menuItem("Post-enrichment plots", tabName = "global_postplot", icon = icon("dashboard"),
                 menuSubItem("Splitdot", tabName = "menu_plot_splitdot"),
                 menuSubItem("Termtree", tabName = "menu_plot_termtree"),
                 menuSubItem("Heatmap (gene-term)", tabName = "menu_plot_heatmap"),
                 menuSubItem("Heatmap (gene-efsi)", tabName = "menu_plot_genefsi_heatmap"),
                 menuSubItem("Protein-Protein Interactions", tabName = "menu_plot_PPI")
        )
      )
    ),
    
    dashboardBody(
      shinyjs::useShinyjs(), # initialize shinyjs
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"), # load .css stylesheet
        tags$script(src = "www/colors.js"), # load JS initializing .css colors 
        tags$script(src = "www/menu_toggle.js"), # # load JS to keep all menu items visible and disable their toggling-to-close
        tags$script(src = "www/visnetwork_hover_tooltip_style.js")
      ),
      tabItems(
        tabItem(tabName = "menu_initialize",
                fluidRow(
                  box(
                    id = "box_global_options",
                    title = "Global options",
                    width = NULL,
                    column(width = 2, wrap_hovertip(selectInput("si_organism", "Select organism",
                                                  choices = list("Human" = 9606, "Mouse" = 10090, "Chimpanzee" = 9598, "Rhesus monkey" = 9544,
                                                                 "Rat" = 10116, "Zebrafish" = 7955, "Fruit fly" = 7227, "Worm" = 6239),
                                                  selected = "Human"),
                                                  hovertip = "Select organism for loading Gene Ontology Bioconductor AnnotationDbi genesets, gene annotation and STRINGDB protein-protein interactions")), 
                    column(width = 2, wrap_hovertip(textInput("ti_global_base_folder", "Base folder", placeholder = "'R_USER' HOME"),
                                                    hovertip = "Base folder used for download if not user selectable, and for STRING database Protein-Protein-Interactions data, using 'R_user' HOME environment variable if not specified, see ?goatea::set_base_folder")),
                    column(width = 2, wrap_hovertip(radioButtons("rb_global_output_type", "Select output type",
                                                                 choices = list("CSV" = ".csv", "Excel" = ".xlsx"),
                                                                 selected = ".csv",
                                                                 inline = FALSE), 
                                                    hovertip = "Select CSV or Excel for output type when writing tables")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_annotate_genes", "Annotate genes", value = TRUE), 
                                                    hovertip = "Requires 'annotables' package, annotate genes with a small description for available organisms, see '?goatea::get_gene_annotation()'")),
                    column(width = 2),
                    column(width = 2, wrap_hovertip(actionButton("ab_global_modal", label = icon("question"), style = "font-size: 24px;"),
                                                    hovertip = "Click to show tab help overview")),
                  )
                ),
                fluidRow(
                  box(
                    id = "box_load_genelists",
                    title = "Genelists",
                    width = NULL,
                    column(width = 2, wrap_hovertip(fileInput("fi_load_genelists", "Load genelist(s)", multiple = TRUE),
                                                    hovertip = "Load genelist(s), accepted formats: .csv, .xlsx, .tsv")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_remove_non_numerical_ids", "Remove non-numerical IDs", value = FALSE), 
                                                    hovertip = "Remove genes with non-available IDs")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_remove_duplicated", "Remove duplicated", value = TRUE), 
                                                    hovertip = "Remove duplicated genes")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_remove_Rik_genes", "Remove Rik genes", value = TRUE), 
                                                    hovertip = "Remove Riken non-canonical mouse genes")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_remove_Gm_genes", "Remove Gm genes", value = TRUE), 
                                                    hovertip = "Remove Gm non-canonical mouse genes")),
                    column(width = 12, verbatimTextOutput("vto_load_genelists", placeholder = TRUE)),
                    
                    column(width = 2, disabled(actionButton("ab_set_significant_genes", "Set signif and N genes", value = TRUE, width = 115))),
                    column(width = 2, wrap_hovertip(selectInput("si_set_significant_genes", "Set significant genes", choices = c("pvalue_effectsize", "pvalue", "effectsize")),
                                                    hovertip = 'Set significant and N genes by pvalue and/or effectsize')),
                    column(width = 2, wrap_hovertip(numericInput("ni_set_significant_pvalue", "P-value <=", 0.05, min = 0, step = 0.01),
                                                    hovertip = 'Significant genes p-value threshold')),
                    column(width = 2, wrap_hovertip(numericInput("ni_set_significant_effectsize", "Effectsize >=", 1, min = 0, step = 0.1),
                                                    hovertip = 'Significant genes effectsize threshold')),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_keep_maxN_genes", "Keep max N genes", value = FALSE), 
                                                    hovertip = paste0("Filter down to max allowed n genes in goat method: ", max(goat::goat_nulldistributions$N), ". NOTE: other methods, like goat_bootstrap, might not have this n genes limitation."))),
                    
                    column(width = 2, wrap_hovertip(selectInput("si_keep_maxN_genes", "Keep max N genes by pvalue/effectsize", choices = c("pvalue", "effectsize")),
                                                    hovertip = 'When keeping max N genes, do so by lowest p-values or by highest absolute effect sizes')),
                    column(width = 12, verbatimTextOutput("vto_set_significant_genes", placeholder = TRUE)),
                    column(width = 2, disabled(actionButton("ab_set_names", "Set names", value = TRUE, width = 115))),
                    column(width = 9, textInput("ti_set_names", label = NULL, placeholder = "Enter genelist names separated by a space...")),
                    column(width = 12, verbatimTextOutput("vto_set_names", placeholder = TRUE)),
                    column(width = 4, wrap_hovertip(downloadButton("db_run_genelist_overlap", "Download genelists overlap"), 
                                                    hovertip = "Download gene overview metadata"))
                  )
                ),
                fluidRow(
                  box(
                    id = "box_load_genesets",
                    title = "Genesets",
                    width = NULL,
                    wrap_hovertip(column(width = 2, wrap_loader(id = "ab_load_GOB_genesets_loader", actionButton("ab_load_GOB_genesets", "Load GO AnnotationDbi genesets"))), 
                                                    hovertip = "Load Gene Ontology Bioconductor AnnotationDbi genesets using selected organism, NOTE: individual organism packages need to be installed manually, if not, a warning message will be thrown."),
                    column(width = 2),
                    column(width = 2, wrap_hovertip(fileInput("fi_load_genesets_GMT", "Load GMT genesets"),
                                                    hovertip = "Load genesets from .gmt format, for instance downloaded from the Molecular Signatures Database")),
                    column(width = 2, wrap_hovertip(textInput('ti_load_genesets_GMT', 'GMT genesets source label', value = 'GMT'),
                                                    hovertip = 'Set genesets source column label, default: "GMT"')),
                    column(width = 12, verbatimTextOutput("vto_load_genesets", placeholder = TRUE)),
                    column(width = 12, tags$hr()),
                    column(width = 2, wrap_hovertip(disabled(wrap_loader(id = "ab_filter_genesets_loader", actionButton("ab_filter_genesets", "Filter genesets"))), 
                                                    hovertip = "Filter loaded genesets for enrichment analysis")),
                    column(width = 2, wrap_hovertip(numericInput("ni_genesets_min_overlap", "Min overlap", 10L, min = 1), 
                                                    hovertip = "Minimum number of genes in the genelist table that must match a geneset")),
                    column(width = 2, wrap_hovertip(numericInput("ni_genesets_max_overlap", "Max overlap", 1500L, min = 1), 
                                                    hovertip = "Maximum number of genes in the genelist table that must match a geneset")),
                    column(width = 3, wrap_hovertip(numericInput("ni_genesets_max_overlap_fraction", "Max overlap fraction", 0.5, min = 0, max = 1, step = 0.05), 
                                                    hovertip = "Analogous to max_overlap, maximum number of genes as a fraction in the genelist table that must match a geneset")),
                    column(width = 3, wrap_hovertip(checkboxInput("cbi_genesets_dedupe", "Remove duplicate genesets", value = TRUE), 
                                                    hovertip = "Remove duplicate genesets")),
                    column(width = 12, verbatimTextOutput("vto_filter_genesets", placeholder = TRUE))
                  )
                ),
                tags$hr(),
                fluidRow(
                  box(
                    id = "box_go_to_analysis",
                    title = "GO TO analysis",
                    width = NULL,
                    column(width = 2, wrap_hovertip(disabled(actionButton("ab_go_to_volcano", "Go to volcano")), 
                                                    hovertip = "Create DEG EnahandcedVolcano plot of genelist(s)")),
                    column(width = 2, wrap_hovertip(disabled(actionButton("ab_go_to_overlap", "Go to overlap")), 
                                                    hovertip = "Overlap genelists by significant genes, plot overlap and create annotated gene overview")),
                    column(width = 2, wrap_hovertip(disabled(actionButton("ab_go_to_enrichment", "Go to enrichment")), 
                                                    hovertip = "Load genelists and genesets to go to enrichment analysis"))
                  )
                )
        ),
        tabItem(tabName = "menu_run_volcano",
                fluidRow(
                  box(
                    id = "box_run_volcano",
                    title = "EnhancedVolcano",
                    width = NULL,
                    column(width = 2, wrap_hovertip(disabled(wrap_loader(id = "ab_run_volcano_loader", actionButton("ab_run_volcano", "Plot EnhancedVolcano"))),
                                                     hovertip = "Plot EnhancedVolcano plot for selected genelist")),
                    column(width = 2, wrap_hovertip(selectInput("si_volcano_sample", label = "Volcano genelist sample", choices = NULL),
                                                    hovertip = "Select genelist sample to plot EnhandcedVolcano")),
                    column(width = 6),
                    column(width = 2, wrap_hovertip(actionButton("ab_volcano_modal", label = icon("question"), style = "font-size: 24px;"),
                                                    hovertip = "Click to show tab help overview")),
                    column(width = 12, verbatimTextOutput("vto_volcano", placeholder = TRUE)),
                    column(width = 12, plotly::plotlyOutput("po_volcano_plot")),
                    column(width = 2, wrap_hovertip(actionButton("ab_select_volcano_genes", "Add selected genes"),
                                                    hovertip = "Add selected genes of from the EnahncedVolcano plot to the genes selection")),
                    column(width = 2, wrap_hovertip(actionButton("ab_reset_volcano_genes", "Reset gene selection"),
                                                    hovertip = "Reset selected genes for plotting")),
                    column(width = 6),
                    column(width = 2, wrap_hovertip(downloadButton("db_volcano_download", "Export vanilla plot"),
                                                    hovertip = "Export vanilla EnhancedVolcano plot with light colors and Enhanced features"))
                  )
                ),
                tags$hr(),
                fluidRow(
                  box(
                    id = "box_go_to_enrichment_from_volcano",
                    title = "GO TO enrichment",
                    width = NULL,
                    column(width = 2, wrap_hovertip(actionButton("ab_go_to_overlap_from_volcano", "Go to overlap"),
                                                    "Go to overlap analysis")),
                    column(width = 2, wrap_hovertip(actionButton("ab_go_to_enrichment_from_volcano", "Go to enrichment"),
                                                    "Go to enrichment analysis, if genelists overlapped, information will be added to downstream output"))
                  )
                )
        ),
        tabItem(tabName = "menu_run_genelist_overlap",
                fluidRow(
                  box(
                    id = "box_run_genelist_overlap",
                    title = "Genelist overlap",
                    width = NULL,
                    column(width = 3, wrap_hovertip(disabled(wrap_loader(id = "ab_plot_overlap_upset_loader", actionButton("ab_plot_overlap_upset", "Plot significant gene overlap"))), 
                                                    hovertip = "Plot genelists gene overlap in an UpSet plot")),
                    column(width = 3, wrap_hovertip(selectInput("si_plot_overlap_upset", "Overlap mode", choices = c('distinct', 'intersect', 'union')), 
                                                    hovertip = "Mode for overlapping genes: intersect (totals of genes), distinct (unique genes), union (sums of genes)")),
                    column(width = 3),
                    column(width = 3, wrap_hovertip(actionButton("ab_overlap_modal", label = icon("question"), style = "font-size: 24px;"),
                                                    hovertip = "Click to show tab help overview")),
                    column(width = 12, verbatimTextOutput("vto_genelist_overlap", placeholder = TRUE)),
                    column(width = 12, upsetjs::upsetjsOutput("po_genelist_overlap")),
                    column(width = 2, wrap_hovertip(actionButton("ab_select_upset_genes", "Add Set genes"),
                                                    hovertip = "Add genes of selected Set to the genes selection")),
                    column(width = 2, wrap_hovertip(actionButton("ab_reset_upset_genes", "Reset gene selection"),
                                                    hovertip = "Reset selected genes for plotting")),
                    column(width = 8),
                    column(2, "Hovered Set"),
                    column(2, textOutput("to_upset_hovered")),
                    column(width = 8),
                    column(2, "Clicked Set"),
                    column(2, textOutput("to_upset_clicked")),
                    column(width = 8),
                    column(width = 2, hidden(downloadButton("db_overlap_plot", "Save plot")))
                  )
                ),
                tags$hr(),
                fluidRow(
                  box(
                    id = "box_go_to_enrichment_from_overlap",
                    title = "GO TO enrichment",
                    width = NULL,
                    column(width = 2, wrap_hovertip(actionButton("ab_go_to_volcano_from_overlap", "Go to volcano"),
                                                    "Go to EnhancedVolcano plotting of genelist(s)")),
                    column(width = 2, wrap_hovertip(actionButton("ab_go_to_enrichment_from_overlap", "Go to enrichment"),
                                                    "Go to enrichment analysis, if genelists overlapped, information will be added to downstream output"))
                  )
                )
        ),
        tabItem(tabName = "menu_run_enrichment",
                fluidRow(
                  box(
                    id = "box_run_goat",
                    title = "Geneset enrichment",
                    width = NULL,
                    ## geneset enrichment
                    column(width = 2, wrap_hovertip(disabled(wrap_loader(id = "ab_run_enrichment_loader", actionButton("ab_run_enrichment", "Run enrichment"))), 
                                                    hovertip = "Run geneset enrichment")),
                    column(width = 3, wrap_hovertip(selectInput("si_test_method", "Method",
                                                                choices = c("goat", "goat_bootstrap", "hypergeometric", "fisherexact", "fisherexact_ease", "gsea", "idea"),
                                                                selected = "goat"), 
                                                    hovertip = "Enrichment method")),
                    column(width = 3, wrap_hovertip(selectInput("si_test_score_type", "Score type",
                                                                choices = c("effectsize", "pvalue", "effectsize_up", "effectsize_down", "effectsize_abs"),
                                                                selected = "effectsize"), 
                                                    hovertip = "Enrichment score type")),
                    column(width = 3, wrap_hovertip(selectInput("si_test_padj_method", "P-adjust method",
                                                                choices = c("BH", "bonferroni"),
                                                                selected = "BH"), 
                                                    hovertip = "P-value multiple testing correction method")),
                    column(width = 2),
                    column(width = 3, wrap_hovertip(checkboxInput("cbi_test_padj_sources", "P-adjust sources", value = TRUE), 
                                                    hovertip = "Second step of multiple testing correction; apply multiple testing adjustment to all p-values according to the number of geneset sources that were tested")),
                    column(width = 3, wrap_hovertip(numericInput("ni_test_padj_cutoff", "P-adjust cutoff",
                                                                 value = 0.01, min = 0L, max = 1L, step = 0.005), 
                                                    hovertip = "Cutoff for adjusted p-value, signif column is set to TRUE for all values lesser-equals")),
                    column(width = 3, wrap_hovertip(numericInput("ni_test_padj_min_signifgenes", "Min significant genes",
                                                                 value = 0L, min = 0L, step = 1), 
                                                    hovertip = "If > 0, after p-value adjustment, set the pvalue_adjust to NA and signif to FALSE for all genesets with fewer than padj_min_signifgenes 'input genes that were significant'")),
                    column(width = 10, verbatimTextOutput("vto_test_enrichment", placeholder = TRUE)),
                    column(width = 2, wrap_hovertip(actionButton("ab_enrichment_modal", label = icon("question"), style = "font-size: 24px;"),
                                                    hovertip = "Click to show tab help overview")),
                    
                    hidden(div(id = "div_enrichment",
                               column(width = 2, selectInput("si_show_enrichment", "Show enrichment", choices = NULL)),
                               column(width = 2, selectInput("si_show_enrichment_source", "Show enrichment source", choices = NULL)),
                               column(width = 2, downloadButton("db_enrichment_current", "Download shown enrichment")),
                               column(width = 2, wrap_hovertip(downloadButton("db_enrichment_all", "Download all enrichments"), 
                                                               hovertip = "Enrichments will be downloaded to: folder/filepath_name.csv")),
                               column(width = 2),
                               column(width = 2, wrap_hovertip(actionButton("ab_enrichment_reset", "Reset filtered enrichments"), 
                                                               hovertip = "Restore original enrichment result(s), reset any applied filters")),
                               column(width = 12, DT::DTOutput("dto_test_enrichment")),
                               column(width = 2, wrap_hovertip(disabled(wrap_loader(id = "ab_filter_enrichment_loader", actionButton("ab_filter_enrichment", "Filter enrichments"))), 
                                                               hovertip = "Filter all loaded geneset enrichment output tables, order by column arrows")),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_ngenes", "Min n genes", 0, min = 0, step = 1), 
                                                               hovertip = "Minimum number of genes in genesets")),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_ngenes_input", "Min n input genes", 0, min = 0, step = 1), 
                                                               hovertip = "Minimum number of genes in genelists input")),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_ngenes_signif", "Min n significant genes", 0, min = 0, step = 1), 
                                                               hovertip = "Minimum number of significant genes in genesets enrichment")),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_zscore", "Min absolute zscore", 0, min = 0, step = 0.1), 
                                                               hovertip = "Minimum zscore")),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_pvalue_adjust", "Min p-value adjust", 0, min = 0, max = 1, step = 0.05), 
                                                               hovertip = "Minimum p-value adjusted for multiple testing correction")),
                               column(width = 2),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_ngenes_max", "Max n genes", 1E6, min = 0, step = 1), 
                                                               hovertip = "Maximum number of genes in genesets")),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_ngenes_input_max", "Max n input genes", 1E6, min = 0, step = 1), 
                                                               hovertip = "Maximum number of genes in genelists input")),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_ngenes_signif_max", "Max n significant genes", 1E6, min = 0, step = 1), 
                                                               hovertip = "Maximum number of significant genes in genesets enrichment")),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_zscore_max", "Max absolute zscore", 1E6, min = 0, step = 0.1), 
                                                               hovertip = "Maximum zscore")),
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_pvalue_adjust_max", "Max p-value adjust", 1, min = 0, max = 1, step = 0.05), 
                                                               hovertip = "Maximum p-value adjusted for multiple testing correction")),
                               column(width = 2, wrap_hovertip(textAreaInput("tai_enrichment_filter_term_query", label = "Query terms", placeholder = "Enter search words by line..."), 
                                                               hovertip = "Query search words to filter genesets by term")),
                               column(width = 2, wrap_hovertip(radioButtons("rb_enrichment_filter_term_query_allany", label = "Query terms mode", choices = c("Any" = "any", "All" = "all"), selected = "any"), 
                                                               hovertip = "Any/all of the search words should match the term")),
                               column(width = 2, wrap_hovertip(textAreaInput("tai_enrichment_filter_term_antiquery", label = "Query terms exclusion", placeholder = "Enter exclusion words by line..."), 
                                                               hovertip = "Query search words to exclude filtered genesets by term")),
                               column(width = 2, wrap_hovertip(radioButtons("rb_enrichment_filter_term_antiquery_allany", label = "Query terms exclusion mode", choices = c("Any" = "any", "All" = "all"), selected = "any"), 
                                                               hovertip = "Any/all of the exclusion search words should match the filtered term")),
                               column(width = 2, wrap_hovertip(textAreaInput("tai_enrichment_filter_gene_query", label = "Query genes", placeholder = "Enter genes by line..."), 
                                                               hovertip = "Query search words to filter genesets by gene")),
                               column(width = 2, wrap_hovertip(radioButtons("tai_enrichment_filter_gene_query_allany", label = "Query genes mode", choices = c("Any" = "any", "All" = "all"), selected = "any"), 
                                                               hovertip = "Any/all of the genes should match the term by its geneset"))
                    )))
                ),
                fluidRow(
                  box(
                    id = "box_go_to_plotting",
                    title = "GO TO plotting",
                    width = NULL,
                    column(width = 2, disabled(actionButton("ab_go_to_heatmap", "GO TO gene/genesets heatmap"))),
                    column(width = 2, disabled(actionButton("ab_go_to_genefsi_icheatmap", "GO TO gene/effectsize heatmap"))),
                    column(width = 2, disabled(actionButton("ab_go_to_PPI", "GO TO PPI"))),
                    column(width = 2, disabled(actionButton("ab_go_to_splitdot", "GO TO splitdot"))),
                    column(width = 2, disabled(actionButton("ab_go_to_termtree", "GO TO termtree"))),
                  )
                )
        ),
        tabItem(tabName = "menu_plot_splitdot",
                fluidRow(
                  box(
                    id = "box_plot_splitdot",
                    title = "Plot splitdot",
                    width = NULL,
                    column(width = 2, wrap_hovertip(disabled(wrap_loader(id = "ab_splitdot_plot_loader", actionButton("ab_splitdot_plot", "Plot splitdot"))), 
                                                    hovertip = "Plot selected (and filtered) enrichment")),
                    column(width = 2, wrap_hovertip(numericInput("ni_splitdot_topN", "top N terms", 50, min = 2, step = 1), 
                                                    hovertip = "Plot terms ordered by adjusted pvalue")),
                    column(width = 4),
                    column(width = 2, hidden(downloadButton("db_splitdot", "Save plot"))),
                    column(width = 2, wrap_hovertip(actionButton("ab_splitdot_modal", label = icon("question"), style = "font-size: 24px;"),
                                                    hovertip = "Click to show tab help overview")),
                    column(width = 12, shinyjqui::jqui_resizable(plotOutput("po_splitdot", width = 800, height = 600)))
                  )),
                fluidRow(
                  box(
                    id = "box_go_to_plotting_splitdot",
                    title = "GO TO plotting",
                    width = NULL,
                    column(width = 2, disabled(actionButton("ab_go_to_genefsi_icheatmap_splitdot", "GO TO gene/effectsize heatmap"))),
                    column(width = 2, disabled(actionButton("ab_go_to_heatmap_splitdot", "GO TO gene/genesets heatmap"))),
                    column(width = 2, disabled(actionButton("ab_go_to_termtree_splitdot", "GO TO termtree"))),
                    column(width = 2, disabled(actionButton("ab_go_to_PPI_splitdot", "GO TO PPI")))
                  )
                )
        ),
        tabItem(tabName = "menu_plot_termtree",
                fluidRow(
                  box(
                    id = "box_plot_termtree",
                    title = "Plot termtree",
                    width = NULL,
                    column(width = 2, wrap_hovertip(disabled(wrap_loader(id = "ab_termtree_plot_loader", actionButton("ab_termtree_plot", "Plot termtree"))), 
                                                    hovertip = "NOTE: requires 'enrichplot' package - Plot selected (and filtered) enrichment")),
                    column(width = 2, wrap_hovertip(numericInput("ni_termtree_Nterms", "top N terms", 50, min = 2, step = 1), 
                                                    hovertip = "Plot terms ordered by adjusted pvalue")),
                    column(width = 2, wrap_hovertip(numericInput("ni_termtree_Nwords", "N words", 5, min = 0, step = 1), 
                                                    hovertip = "Plot N summarizing words from semantic similarity/overlapping genes")),
                    column(width = 2, wrap_hovertip(numericInput("ni_termtree_Nclusters", "N clusters", 3, min = 1, step = 1), 
                                                    hovertip = "Plot N clusters of terms by semantic similarity/overlapping genes")),
                    column(width = 2, hidden(downloadButton("db_termtree", "Save plot"))),
                    column(width = 2, wrap_hovertip(actionButton("ab_termtree_modal", label = icon("question"), style = "font-size: 24px;"),
                                                    hovertip = "Click to show tab help overview")),
                    column(width = 12, shinyjqui::jqui_resizable(plotOutput("po_termtree", width = 800, height = 600)))
                  )),
                fluidRow(
                  box(
                    id = "box_go_to_plotting_termtree",
                    title = "GO TO plotting",
                    width = NULL,
                    column(width = 2, disabled(actionButton("ab_go_to_genefsi_icheatmap_termtree", "GO TO gene/effectsize heatmap"))),
                    column(width = 2, disabled(actionButton("ab_go_to_heatmap_termtree", "GO TO gene/genesets heatmap"))),
                    column(width = 2, disabled(actionButton("ab_go_to_splitdot_termtree", "GO TO splitdot"))),
                    column(width = 2, disabled(actionButton("ab_go_to_PPI_termtree", "GO TO PPI"))),
                  )
                )
        ),
        tabItem(tabName = "menu_plot_heatmap",
                fluidRow(
                  box(
                    id = "box_plot_heatmap",
                    title = "Plot Interactive Heatmap of genes vs gene sets",
                    width = NULL,
                    column(width = 2, wrap_hovertip(disabled(wrap_loader(id = "ab_icheatmap_plot_loader", actionButton("ab_icheatmap_plot", "Plot heatmap"))), 
                                                    hovertip = "Plot selected (and filtered) enrichment")),
                    column(width = 2, wrap_hovertip(selectInput("si_icheatmap_cluster_method", "Cluster method",
                                                                choices = c("single", "ward.D", "ward.D2", "complete", "average", "mcquitty", "median", "centroid"),
                                                                selected = "single"), hovertip = "Clustering method for terms and genes")),
                    column(width = 2, wrap_hovertip(numericInput("ni_icheatmap_nclusters", "N clusters", 4, min = 1, step = 1), 
                                                    hovertip = "Cluster terms by gene overlap")),
                    column(width = 2, wrap_hovertip(numericInput("ni_icheatmap_nterms", "topN terms", 50, min = 1, step = 1), 
                                                    hovertip = "Top N terms based on effectsize")),
                    column(width = 2, wrap_hovertip(numericInput("ni_icheatmap_ngenes", "topN genes", 100, min = 1, step = 1), 
                                                    hovertip = "Top N genes based on effectsize")),
                    column(width = 2, wrap_hovertip(actionButton("ab_heatmap_modal", label = icon("question"), style = "font-size: 24px;"),
                                                    hovertip = "Click to show tab help overview")),
                    column(width = 12, verbatimTextOutput("vto_icheatmap", placeholder = TRUE)),
                    column(width = 12, InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput(heatmap_id = "icheatmap", layout = "1|2|3", output_ui_float = TRUE, width1 = 1400, height1 = 700, width2 = 1400, height2 = 500)),
                    column(width = 2, wrap_hovertip(actionButton("ab_icheatmap_select_genes", label = "Add visible genes"),
                                                    hovertip = "Add genes visible in subheatmap to selection")),
                    column(width = 2, wrap_hovertip(actionButton("ab_icheatmap_select_terms", "Add all genes of visible terms"),
                                                    hovertip = "Add all genes from terms visible in subheatmap to selection")),
                    column(width = 2, wrap_hovertip(actionButton("ab_icheatmap_select_terms_subgenes", "Add visible genes of visible terms"),
                                                    hovertip = "Add only visible genes from terms visibile in subheatmap to selection")),
                    column(width = 2, wrap_hovertip(actionButton("ab_icheatmap_reset_protgenes", "Reset selected genes"),
                                                    hovertip = "Reset/remove all selected genes"))
                  )),
                fluidRow(
                  box(
                    id = "box_go_to_plotting_icheatmap",
                    title = "GO TO plotting",
                    width = NULL,
                    column(width = 2, actionButton("ab_go_to_genefsi_icheatmap_icheatmap", "GO TO gene/effectsize heatmap")),
                    column(width = 2, actionButton("ab_go_to_PPI_icheatmap", "GO TO PPI")),
                    column(width = 2, actionButton("ab_go_to_splitdot_icheatmap", "GO TO splitdot")),
                    column(width = 2, actionButton("ab_go_to_termtree_icheatmap", "GO TO termtree")),
                  )
                )
        ),
        tabItem(tabName = "menu_plot_genefsi_heatmap",
                fluidRow(
                  box(
                    id = "box_plot_genefsi_heatmap",
                    title = "Plot Interactive Heatmap of genes vs effect sizes",
                    width = NULL,
                    column(width = 2, wrap_hovertip(wrap_loader(id = "ab_genefsi_icheatmap_plot_loader", actionButton("ab_genefsi_icheatmap_plot", "Plot heatmap")), 
                                                    hovertip = "Plot selected genes vs effect sizes of all loaded genelists")),
                    column(width = 2, wrap_hovertip(numericInput("ni_genefsi_icheatmap_ngenes", "Plot N genes", 50, min = 1, step = 1), 
                                                    hovertip = "Maximally plot N genes from selection")),
                    column(width = 2, wrap_hovertip(checkboxInput("ci_genefsi_icheatmap_dendrogram_cols", "Cluster dendrogram columns"),
                                                    hovertip = "Check to cluster and show dendrogram on heatmap columns")),
                    column(width = 2, wrap_hovertip(checkboxInput("ci_genefsi_icheatmap_dendrogram_rows", "Cluster dendrogram rows"),
                                                    hovertip = "Check to cluster and show dendrogram on heatmap rows")),
                    column(width = 2, wrap_hovertip(textAreaInput("tai_genefsi_add_genes", label = "Additional genes", placeholder = "Add genes (to selection)..."),
                                                    hovertip = "Add gene symbols separated by enter (\n) to take into account additionally next to earlier selected genes.")),
                    column(width = 2, wrap_hovertip(actionButton("ab_genefsi_heatmap_modal", label = icon("question"), style = "font-size: 24px;"),
                                                    hovertip = "Click to show tab help overview")),
                    column(width = 12, verbatimTextOutput("vto_genefsi_icheatmap", placeholder = TRUE)),
                    column(width = 12, InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput(heatmap_id = "genefsi_icheatmap", layout = "1|2|3", output_ui_float = TRUE, width1 = 1400, height1 = 700, width2 = 1400, height2 = 500)),
                  )),
                fluidRow(
                  box(
                    id = "box_go_to_plotting_genefsi_icheatmap",
                    title = "GO TO plotting",
                    width = NULL,
                    column(width = 2, actionButton("ab_go_to_PPI_genefsi_icheatmap", "GO TO PPI")),
                    column(width = 2, actionButton("ab_go_to_heatmap_genefsi_icheatmap", "GO TO gene-efsi heatmap")),
                    column(width = 2, actionButton("ab_go_to_splitdot_genefsi_icheatmap", "GO TO splitdot")),
                    column(width = 2, actionButton("ab_go_to_termtree_genefsi_icheatmap", "GO TO termtree")),
                  )
                )
        ),
        tabItem(tabName = "menu_plot_PPI",
                fluidRow(
                  box(
                    id = "box_plot_PPI",
                    title = "Plot Protein-Protein Interations",
                    width = NULL,
                    
                    ## ppigraph
                    column(width = 2, wrap_hovertip(wrap_loader("ab_ppi_graph_loader", actionButton("ab_ppi_graph", "Create PPIgraph")),
                                                    hovertip = "Create protein-protein interaction graph from STRING database interactions of selected proteins, using parameters of UI to the right")),
                    column(width = 2, wrap_hovertip(selectInput("si_ppi_sample", "Select sample", choices = NULL),
                                                    hovertip = "Select sample to get metadata from for selected proteins/genes. Note, sample can be different than sample on the enrichment tab.")), 
                    column(width = 2, wrap_hovertip(numericInput("ni_ppi_score_threshold", "STRINGdb score threshold", value = 0, min = 0, max = 1000, step = 10),
                                                    hovertip = "STRING database PPI score threshold, ranges from 0 (no threshold) to 1000 (threshold all). STRINGdb advises 700 as stringent and 400 as lenient thresholding.")),
                    column(width = 2, wrap_hovertip(selectInput("si_ppi_version", "Select STRINGdb version", choices = NULL),
                                                    hovertip = "Select from STRING database available version. Default and recommended to select latest.")),
                    column(width = 2, wrap_hovertip(textAreaInput("tai_ppi_add_protgenes", label = "Additional proteins/genes", placeholder = "Add proteins/genes (to selection)..."),
                                                    hovertip = "Add proteins/gene symbols separated by enter (\n) to take into account additionally next to earlier selected genes.")),
                    column(width = 2, wrap_hovertip(actionButton("ab_ppi_reset_protgenes", label = "Reset proteins/genes selection"),
                                                    hovertip = "Reset proteins/gene symbols selection")),
                    column(width = 10, verbatimTextOutput("vto_ppi_selection")),
                    column(width = 2, wrap_hovertip(actionButton("ab_ppi_modal", label = icon("question"), style = "font-size: 24px;"),
                                                    hovertip = "Click to show tab help overview")),
                    column(width = 12, visNetwork::visNetworkOutput("vno_ppi_visnetwork", height = "600px")),
                    ## below graph
                    fluidRow(
                      column(width = 2, wrap_hovertip(actionButton("ab_ppi_subset", "Subgraph highlighted nodes"),
                                                      hovertip = "Draw ppisubgraph below from highlighted nodes")),
                      column(width = 2),
                      column(width = 2, wrap_hovertip(actionButton("ab_ppi_highlight_gene", "Highlight protein"),
                                                      hovertip = "Highlight textinput protein to the right")),
                      column(width = 2, wrap_hovertip(textInput("ti_ppi_highlight_gene", "Protein"),
                                                      hovertip = "Gene to highlight")),
                      column(width = 2, wrap_hovertip(actionButton("ab_ppi_highlight_cluster", "Highlight cluster"),
                                                      hovertip = "Highlight genes by selecting cluster to the right")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_highlight_cluster", "Cluster", choices = NULL),
                                                      hovertip = "Cluster to highlight"))
                    ),
                    fluidRow(
                      column(width = 2, wrap_hovertip(actionButton("ab_ppi_highlight_nodes", "Highlight nodes by feature"),
                                                      hovertip = "Highlight nodes by numeric feature filter")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_highlight_nodes", "Numeric feature type", choices = c("Nodes", "Edges")),
                                                      hovertip = "Feature type to highlight")),
                      column(width = 2, wrap_hovertip(numericInput("ni_ppi_highlight_nodes_min", "Minimum value", -1e6),
                                                      hovertip = "Minimum value for feature type highlighting by filtering")),
                      column(width = 2, wrap_hovertip(numericInput("ni_ppi_highlight_nodes_max", "Maximum value", 1e6),
                                                      hovertip = "Maximum value for feature type highlighting by filtering")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_color_nodes", "Nodes feature", choices = NULL),
                                                      hovertip = "Select to color nodes aspect by an igraph feature")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_color_edges", "Edges feature", choices = NULL),
                                                      hovertip = "Select to color interactions by an edge feature")),
                    ),
                    fluidRow(
                      column(width = 2, wrap_hovertip(downloadButton("db_ppigraph_metrics", "Export ppigraph metrics"),
                                                      hovertip = "Export ppigraph metrics to table")),
                      column(width = 6),
                      
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_color_nodes_type", "Nodes visual feature to color", choices = c('Background', 'Border'), selected = 'Background'),
                                                      hovertip = "Select to color nodes background or border by selected a node feature next")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_layout", "Select ppigraph layout", choices = NULL),
                                                      hovertip = "Select igraph layout")),
                    ),
                    column(width = 12, verbatimTextOutput("vto_ppi_metrics")),
                    column(width = 12, div(tags$hr())),
                    
                    ## ppi subgraph
                    column(width = 12, verbatimTextOutput("vto_ppi_selection_subgraph")),
                    column(width = 12, visNetwork::visNetworkOutput("vno_ppi_visnetwork_subgraph", height = "600px")),
                    fluidRow(
                      column(width = 2, wrap_hovertip(actionButton("ab_ppi_delete_nodes_subgraph", "Delete highlighted nodes"),
                                                      hovertip = "Delete highlighted nodes from ppisubgraph")),
                      column(width = 2),
                      column(width = 2, wrap_hovertip(actionButton("ab_ppi_highlight_gene_subgraph", "Highlight protein"),
                                                      hovertip = "Highlight textinput protein to the right")),
                      column(width = 2, wrap_hovertip(textInput("ti_ppi_highlight_gene_subgraph", "Protein"),
                                                      hovertip = "Gene to highlight")),
                      column(width = 2, wrap_hovertip(actionButton("ab_ppi_highlight_cluster_subgraph", "Highlight cluster"),
                                                      hovertip = "Highlight genes by selecting cluster to the right")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_highlight_cluster_subgraph", "Cluster", choices = NULL),
                                                      hovertip = "Cluster to highlight")),
                    ),
                    fluidRow(
                      column(width = 2, wrap_hovertip(actionButton("ab_ppi_highlight_nodes_subgraph", "Highlight nodes by feature"),
                                                      hovertip = "Highlight nodes by numeric feature filter")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_highlight_nodes_subgraph", "Numeric feature type", choices = c("Nodes", "Edges")),
                                                      hovertip = "Feature type to highlight")),
                      column(width = 2, wrap_hovertip(numericInput("ni_ppi_highlight_nodes_min_subgraph", "Minimum value", -1e6),
                                                      hovertip = "Minimum value for feature type highlighting by filtering")),
                      column(width = 2, wrap_hovertip(numericInput("ni_ppi_highlight_nodes_max_subgraph", "Maximum value", 1e6),
                                                      hovertip = "Maximum value for feature type highlighting by filtering")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_color_nodes_subgraph", "Nodes feature", choices = NULL),
                                                      hovertip = "Select to color nodes aspect by an igraph feature")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_color_edges_subgraph", "Edges feature", choices = NULL),
                                                      hovertip = "Select to color interactions by an edge feature")),
                    ),
                    fluidRow(
                      column(width = 2, wrap_hovertip(downloadButton("db_ppigraph_metrics_subgraph", "Export ppigraph metrics"),
                                                      hovertip = "Export ppigraph metrics to table")),
                      column(width = 2),
                      column(width = 2, wrap_hovertip(actionButton("ab_ppi_reset_subgraph", "Reset to original ppigraph"),
                                                      hovertip = "Reset ppisubgraph to original ppigraph")),
                      column(width = 2),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_color_nodes_type_subgraph", "Nodes visual feature to color", choices = c('Background', 'Border'), selected = 'Background'),
                                                      hovertip = "Select to color nodes background or border by selecting a node feature")),
                      column(width = 2, wrap_hovertip(selectInput("si_ppi_layout_subgraph", "Select subgraph layout", choices = NULL),
                                                      hovertip = "Select igraph layout")),
                    ),
                    column(width = 12, verbatimTextOutput("vto_ppi_metrics_subgraph")),
                  )
                ),
                fluidRow(
                  box(
                    id = "box_go_to_plotting_PPI",
                    title = "GO TO plotting",
                    width = NULL,
                    column(width = 2, actionButton("ab_go_to_genefsi_icheatmap_PPI", "GO TO gene/effeectsize heatmap")),
                    column(width = 2, actionButton("ab_go_to_heatmap_PPI", "GO TO gene/genesets heatmap")),
                    column(width = 2, actionButton("ab_go_to_splitdot_PPI", "GO TO splitdot")),
                    column(width = 2, actionButton("ab_go_to_termtree_PPI", "GO TO termtree")),
                  )
                )
        )
      )
    )
  )
}