#' UI for goatea package
#'
#' @export
goatea_ui <- function() {
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
                    column(width = 2, wrap_hovertip(radioButtons("rb_human_mouse", "Select organism",
                                                                 choices = list("Human" = "Hs", "Mouse" = "Mm"),
                                                                 selected = "Mm", # FINAL set to Hs
                                                                 inline = FALSE), 
                                                    hovertip = "Select human or mouse for loading Gene Ontology Bioconductor genesets and gene annotation")),
                    column(width = 2, wrap_hovertip(radioButtons("rb_global_output_type", "Select output type",
                                                                 choices = list("CSV" = ".csv", "Excel" = ".xlsx"),
                                                                 selected = ".csv",
                                                                 inline = FALSE), 
                                                    hovertip = "Select CSV or Excel for output type when writing tables")),
                    column(width = 3, wrap_hovertip(checkboxInput("cbi_annotate_genes", "Annotate genes", value = TRUE), 
                                                    hovertip = "If individual genes are output in .csvs, annotate with a small description"))
                  )
                ),
                fluidRow(
                  box(
                    id = "box_load_genelists",
                    title = "Genelists",
                    width = NULL,
                    column(width = 2, wrap_hovertip(actionButton("ab_load_genelists", "Load genelist(s)", width = 115), 
                                                    hovertip = "Load genelist(s), accepted formats: .csv, .xlsx, .tsv")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_remove_non_numerical_ids", "Remove non-numerical IDs", value = FALSE), 
                                                    hovertip = "Remove genes with non-available IDs")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_remove_duplicated", "Remove duplicated", value = TRUE), 
                                                    hovertip = "Remove duplicated genes")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_remove_Rik_genes", "Remove Rik genes", value = TRUE), 
                                                    hovertip = "Remove Riken non-canonical mouse genes")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_remove_Gm_genes", "Remove Gm genes", value = TRUE), 
                                                    hovertip = "Remove Gm non-canonical mouse genes")),
                    column(width = 2, wrap_hovertip(checkboxInput("cbi_keep_maxN_genes", "Keep max N genes", value = TRUE), 
                                                    hovertip = paste0("Filter down to max allowed n genes: ", max(goat::goat_nulldistributions$N)))),
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
                    wrap_hovertip(column(width = 2, disabled(wrap_loader(id = "ab_load_GOB_genesets_loader", 
                                                                         actionButton("ab_load_GOB_genesets", "Load GO Bioconductor genesets")))), 
                                  hovertip = "Load Gene Ontology Bioconductor genesets using selected organism"),
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
                    
                    column(width = 2, wrap_hovertip(disabled(actionButton("ab_go_to_overlap", "Go to overlap")), 
                                                    hovertip = "Overlap genelists by significant genes, plot overlap and create annotated gene overview")),
                    column(width = 2, wrap_hovertip(disabled(actionButton("ab_go_to_enrichment", "Go to enrichment")), 
                                                    hovertip = "Load genelists and genesets to go to enrichment analysis"))
                  )
                )
        ),
        tabItem(tabName = "menu_run_genelist_overlap",
                fluidRow(
                  box(
                    id = "box_run_genelist_overlap",
                    title = "Genelist overlap",
                    width = NULL,
                    column(width = 3, wrap_hovertip(disabled(wrap_loader(id = "ab_run_genelist_overlap_loader", actionButton("ab_run_genelist_overlap", "Run genelists overlap"))), 
                                                    hovertip = "Run genelists overlap for 2 or more loaded genelists")),
                    column(width = 12, verbatimTextOutput("vto_genelist_overlap", placeholder = TRUE)),
                    column(width = 3, wrap_hovertip(disabled(wrap_loader(id = "ab_plot_overlap_venn_loader", actionButton("ab_plot_overlap_venn", "Plot overlap Venn"))), 
                                                    hovertip = "Plot genelists gene overlap in a Venn diagram")),
                    column(width = 3, wrap_hovertip(disabled(wrap_loader(id = "ab_plot_overlap_upset_loader", actionButton("ab_plot_overlap_upset", "Plot overlap UpSet"))), 
                                                    hovertip = "Plot genelists gene overlap in an UpSet plot")),
                    column(width = 3, wrap_hovertip(checkboxInput("cbi_plot_overlap_upset_grayscale", "UpSet grayscale colors", value = TRUE), 
                                                    hovertip = "Set UpSet plot coloring to grayscale or colorful")),
                    column(width = 3, wrap_hovertip(checkboxInput("cbi_plot_overlap_upset_intersections", "UpSet empty intersections", value = TRUE), 
                                                    hovertip = "Set UpSet plot to (not) show empty intersections")),
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
                    column(width = 2, wrap_hovertip(disabled(actionButton("ab_go_to_enrichment_from_overlap", "Go to enrichment")),
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
                                                                choices = c("goat", "hypergeometric", "fisherexact", "fisherexact_ease", "gsea", "idea"),
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
                    column(width = 12, verbatimTextOutput("vto_test_enrichment", placeholder = TRUE)),
                    
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
                               column(width = 2, wrap_hovertip(numericInput("ni_enrichment_filter_pvalue_adjust", "Max p-value adjust", 1, min = 0, max = 1, step = 0.05), 
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
                    column(width = 2, disabled(actionButton("ab_go_to_heatmap", "GO TO heatmap"))),
                    column(width = 2, disabled(actionButton("ab_go_to_PPI", "GO TO PPI"))),
                  )
                )
        ),
        tabItem(tabName = "menu_plot_heatmap",
                fluidRow(
                  box(
                    id = "box_plot_heatmap",
                    title = "Plot Interactive Heatmap",
                    width = NULL,
                    column(width = 2, wrap_hovertip(actionButton("ab_icheatmap_plot", "Plot heatmap"), 
                                                    hovertip = "Plot selected (and filtered) enrichment")),
                    column(width = 3, wrap_hovertip(selectInput("si_icheatmap_cluster_method", "Cluster method",
                                                                choices = c("single", "ward.D", "ward.D2", "complete", "average", "mcquitty", "median", "centroid"),
                                                                selected = "single"), hovertip = "Clustering method for terms and genes")),
                    column(width = 2, wrap_hovertip(numericInput("ni_icheatmap_nclusters", "N clusters", 4, min = 1, step = 1), 
                                                    hovertip = "Cluster terms by gene overlap")),
                    column(width = 2, wrap_hovertip(numericInput("ni_icheatmap_nterms", "topN terms", NULL, min = 1, step = 1), 
                                                    hovertip = "Top N terms based on effectsize")),
                    column(width = 2, wrap_hovertip(numericInput("ni_icheatmap_ngenes", "topN genes", NULL, min = 1, step = 1), 
                                                    hovertip = "Top N genes based on effectsize")),
                    column(width = 12, verbatimTextOutput("vto_icheatmap", placeholder = TRUE)),
                    column(width = 12, InteractiveComplexHeatmapOutput(heatmap_id = "icheatmap", layout = "1|2|3", output_ui_float = TRUE))
                    
                    # TODO GOTO plotting
                  )),
                fluidRow(
                  box(
                    id = "box_go_to_plotting_icheatmap",
                    title = "GO TO plotting",
                    width = NULL,
                    column(width = 2, disabled(actionButton("ab_go_to_PPI_icheatmap", "GO TO PPI"))),
                  )
                )
        ),
        tabItem(tabName = "menu_plot_PPI",
                fluidRow(
                  box(
                    id = "box_plot_PPI",
                    title = "Plot Protein-Protein Interations",
                    width = NULL,
                    # TODO UI elements
                    
                    # TODO server use selected displayed/filtered enrichment data for PPI 
                  )
                )
        )
      )
    )
  )
}