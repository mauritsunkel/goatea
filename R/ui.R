#' UI for goatea package
#'
#' @export
ui <- function() {
  dashboardPage(
    dashboardHeader(title = "GOATEA"),
    dashboardSidebar(
      sidebarMenu(
        id = "menu_tabs",
        menuItem("Initialize", tabName = "menu_initialize", icon = icon("dashboard")),
        menuItem("Genelists overlap", tabName = "menu_run_genelist_overlap", icon = icon("dashboard")),
        menuItem("Geneset enrichment", tabName = "menu_run_enrichment", icon = icon("dashboard")),
        menuItem("Heatmapping", tabName = "menu_plot_heatmap", icon = icon("dashboard")),
        menuItem("Protein-Protein Interactions", tabName = "menu_plot_PPI", icon = icon("dashboard"))
      )
    ),
    dashboardBody(
      useShinyjs(), # Initialize shinyjs
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
      ),
      tabItems(
        tabItem(tabName = "menu_initialize",
                fluidRow(
                  box(
                    id = "box_global_options",
                    title = "Global options",
                    width = NULL,
                    wrap_hovertip("Select human or mouse for loading Gene Ontology Bioconductor genesets and gene annotation",
                             column(width = 2, radioButtons("rb_human_mouse", "Select organism",
                                                            choices = list("Human" = "Hs", "Mouse" = "Mm"),
                                                            selected = "Mm", # FINAL set to Hs
                                                            inline = FALSE))),
                    wrap_hovertip("If individual genes are output in .csvs, annotate with a small description",
                    column(width = 3, checkboxInput("cbi_annotate_genes", "Annotate genes", value = TRUE)))
                    )
                ),
                fluidRow(
                  box(
                    id = "box_load_genelists",
                    title = "Genelists",
                    width = NULL,
                    wrap_hovertip("Load genelist(s), accepted formats: .csv, .xlsx, .tsv",
                             column(width = 2, actionButton("ab_load_genelists", "Load genelist(s)", width = 115))),
                    wrap_hovertip("Remove genes with non-available IDs",
                             column(width = 2, checkboxInput("cbi_remove_non_numerical_ids", "Remove non-numerical IDs", value = FALSE))),
                    wrap_hovertip("Remove duplicated genes",
                             column(width = 2, checkboxInput("cbi_remove_duplicated", "Remove duplicated", value = TRUE))),
                    wrap_hovertip("Remove Riken non-canonical mouse genes",
                             column(width = 2, checkboxInput("cbi_remove_Rik_genes", "Remove Rik genes", value = TRUE))),
                    wrap_hovertip("Remove Gm non-canonical mouse genes",
                             column(width = 2, checkboxInput("cbi_remove_Gm_genes", "Remove Gm genes", value = TRUE))),
                    wrap_hovertip(paste0("Filter down to max allowed n genes: ", max(goat::goat_nulldistributions$N)),
                             column(width = 2, checkboxInput("cbi_keep_maxN_genes", "Keep max N genes", value = TRUE))),
                    column(width = 12, verbatimTextOutput("vto_load_genelists", placeholder = TRUE)),
                    
                    column(width = 2, disabled(actionButton("ab_set_significant_genes", "Set signif genes", value = TRUE, width = 115))),
                    column(width = 2, numericInput("ni_set_significant_pvalue", "P-value <=", 0.05, min = 0, step = 0.01)),
                    column(width = 2, numericInput("ni_set_significant_effectsize", "Effectsize >=", 1, min = 0, step = 0.1)),
                    column(width = 12, verbatimTextOutput("vto_set_significant_genes", placeholder = TRUE)),
                    
                    column(width = 2, disabled(actionButton("ab_set_names", "Set names", value = TRUE, width = 115))),
                    column(width = 9, textInput("ti_set_names", label = NULL, placeholder = "Enter genelist names separated by a space...")),
                    column(width = 12, verbatimTextOutput("vto_set_names", placeholder = TRUE))
                  )
                ),
                fluidRow(
                  box(
                    id = "box_load_genesets",
                    title = "Genesets",
                    width = NULL,
                    wrap_hovertip("Load Gene Ontology Bioconductor genesets using selected organism",
                             column(width = 2, disabled(wrap_loader(id = "ab_load_GOB_genesets_loader", actionButton("ab_load_GOB_genesets", "Load GO Bioconductor genesets"))))),
                    column(width = 12, verbatimTextOutput("vto_load_genesets", placeholder = TRUE)),
                    column(width = 12,tags$hr()),
                    wrap_hovertip("Filter loaded genesets for enrichment analysis",
                             column(width = 2, disabled(wrap_loader(id = "ab_filter_genesets_loader", actionButton("ab_filter_genesets", "Filter genesets"))),
                                    )),
                    wrap_hovertip("Minimum number of genes in the genelist table that must match a geneset",
                             column(width = 2, numericInput("ni_genesets_min_overlap", "Min overlap", 10L, min = 1))),
                    wrap_hovertip("Maximum number of genes in the genelist table that must match a geneset",
                             column(width = 2, numericInput("ni_genesets_max_overlap", "Max overlap", 1500L, min = 1))),
                    wrap_hovertip("Analogous to max_overlap, maximum number of genes as a fraction in the genelist table that must match a geneset",
                             column(width = 3, numericInput("ni_genesets_max_overlap_fraction", "Max overlap fraction", 0.5, min = 0, max = 1, step = 0.05))),
                    wrap_hovertip("Remove duplicate genesets",
                             column(width = 3, checkboxInput("cbi_genesets_dedupe", "Remove duplicate genesets", value = TRUE))),
                    column(width = 12, verbatimTextOutput("vto_filter_genesets", placeholder = TRUE))
                  )
                ),
                tags$hr(),
                fluidRow(
                  box(
                    id = "box_go_to_analysis",
                    title = "GO TO analysis",
                    width = NULL,
                    wrap_hovertip("Overlap genelists by significant genes, plot overlap and create annotated gene overview",
                                  column(width = 2, disabled(actionButton("ab_go_to_overlap", "Go to overlap")))),
                    wrap_hovertip("Load genelists and genesets to go to enrichment analysis",
                      column(width = 2, disabled(actionButton("ab_go_to_enrichment", "Go to enrichment"))))
                  )
                )
        ),
        tabItem(tabName = "menu_run_genelist_overlap",
                fluidRow(
                  box(
                    id = "box_run_genelist_overlap",
                    title = "Genelist overlap",
                    width = NULL,
                    column(width = 3, disabled(wrap_hovertip("Run genelists overlap for 2 or more loaded genelists", wrap_loader(id = "ab_run_genelist_overlap_loader", actionButton("ab_run_genelist_overlap", "Run genelists overlap"))))),
                    column(width = 12, verbatimTextOutput("vto_genelist_overlap", placeholder = TRUE)),
                    column(width = 3, disabled(wrap_hovertip("Plot genelists gene overlap in a Venn diagram", wrap_loader(id = "ab_plot_overlap_venn_loader", actionButton("ab_plot_overlap_venn", "Plot overlap Venn"))))),
                    column(width = 3, disabled(wrap_hovertip("Plot genelists gene overlap in an UpSet plot", wrap_loader(id = "ab_plot_overlap_upset_loader", actionButton("ab_plot_overlap_upset", "Plot overlap UpSet"))))),
                    column(width = 3, wrap_hovertip("Set UpSet plot coloring to grayscale or colorful", checkboxInput("cbi_plot_overlap_upset_grayscale", "UpSet grayscale colors", value = TRUE))),
                    column(width = 3, wrap_hovertip("Set UpSet plot to (not) show empty intersections", checkboxInput("cbi_plot_overlap_upset_intersections", "UpSet empty intersections", value = TRUE))),
                    column(width = 12, plotOutput("po_genelist_overlap")),
                    column(width = 2, hidden(downloadButton("db_overlap_plot", "Save plot")))
                  )
                ),
                tags$hr(),
                fluidRow(
                  box(
                    id = "box_go_to_enrichment_from_overlap",
                    title = "GO TO enrichment",
                    width = NULL,
                    wrap_hovertip("Load genelists and genesets to go to enrichment analysis",
                                  column(width = 2, disabled(actionButton("ab_go_to_enrichment_from_overlap", "Go to enrichment"))))
                  )
                )
        ),
        tabItem(tabName = "menu_run_enrichment",
                fluidRow(
                  box(
                    id = "box_run_goat",
                    title = "Geneset enrichment",
                    width = NULL,
                    column(width = 2, disabled(wrap_hovertip("Run geneset enrichment", wrap_loader(id = "ab_run_enrichment_loader", actionButton("ab_run_enrichment", "Run enrichment"))))),
                    column(width = 3, wrap_hovertip("Enrichment method", selectInput("si_test_method", "Method",
                                                  choices = c("goat", "hypergeometric", "fisherexact", "fisherexact_ease", "gsea", "idea"),
                                                  selected = "goat"))),
                    column(width = 3, wrap_hovertip("Enrichment score type ", selectInput("si_test_score_type", "Score type",
                                                  choices = c("effectsize", "pvalue", "effectsize_up", "effectsize_down", "effectsize_abs"),
                                                  selected = "effectsize"))),
                    column(width = 3, wrap_hovertip("P-value multiple testing correction method", selectInput("si_test_padj_method", "P-adjust method",
                                                  choices = c("BH", "bonferroni"),
                                                  selected = "BH"))),
                    column(width = 2),
                    column(width = 3, wrap_hovertip("second step of multiple testing correction; apply multiple testing adjustment to all p-values according to the number of geneset sources that were tested", checkboxInput("cbi_test_padj_sources", "P-adjust sources", value = TRUE))),
                    column(width = 3, wrap_hovertip("cutoff for adjusted p-value, signif column is set to TRUE for all values lesser-equals", numericInput("ni_test_padj_cutoff", "P-adjust cutoff",
                                                   value = 0.01, min = 0L, max = 1L, step = 0.005))),
                    column(width = 3, wrap_hovertip("if > 0, after p-value adjustment, set the pvalue_adjust to NA and signif to FALSE for all genesets with fewer than padj_min_signifgenes 'input genes that were significant'", numericInput("ni_test_padj_min_signifgenes", "Min significant genes",
                                                   value = 0L, min = 0L, step = 1))),
                    column(width = 12, verbatimTextOutput("vto_test_enrichment", placeholder = TRUE)),
                    column(width = 3, selectInput("si_show_enrichment", "Show enrichment", choices = NULL)),
                    column(width = 12, DT::DTOutput("dto_test_enrichment")),
                    column(width = 2, disabled(wrap_hovertip("Filter all loaded geneset enrichment output tables", wrap_loader(id = "ab_filter_enrichment_loader", actionButton("ab_filter_enrichment", "Filter enrichments"))))),
                    # TODO enable ab_filter_enrichment_loader
                    # TODO add filter parameters 
                  )
                ),
                fluidRow(
                  box(
                    id = "box_go_to_plotting",
                    title = "GO TO plotting",
                    width = NULL,
                    column(width = 2, disabled(actionButton("ab_go_to_heatmap", "GO TO heatmap"))),
                    column(width = 2, disabled(actionButton("ab_go_to_PPI", "GO TO PPI"))),
                  )
                )
        ),
        tabItem(tabName = "menu_plot_heatmap",
                fluidRow(
                  box(
                    id = "box_plot_heatmap",
                    title = "Plot heatmap",
                    width = NULL,
                    # TODO UI elements
                  ))
                ),
        tabItem(tabName = "menu_plot_PPI",
                fluidRow(
                  box(
                    id = "box_plot_PPI",
                    title = "Plot Protein-Protein Interations",
                    width = NULL,
                    # TODO UI elements
                  )
                )
        )
      )
    )
  )
}