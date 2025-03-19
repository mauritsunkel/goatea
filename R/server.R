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
goatea_server <- function(input, output, session, mm_genesets) {
  # TODO FINALLY give user control of colors from calling app.R 
  # TODO set colors in server via this list, that user at some point can define! 
  css_colors <- list(
    main_bg = "#222222",
    darker_bg = "#111111",
    focus = "#32CD32",
    hover = "#228B22",
    border = "#555555",
    text = "#FFFFFF"
  )
  colors <- shiny::reactiveValues(
    main_bg = css_colors$main_bg,
    darker_bg = css_colors$darker_bg,
    focus = css_colors$focus,
    hover = css_colors$hover,
    border = css_colors$border,
    text = css_colors$text
  )
  ## set css colors
  session$onFlushed(function() session$sendCustomMessage("update_css_colors", css_colors), once = TRUE)
  
  
  
  # FINAL remove else after testing
  if (is.null(mm_genesets)) {
    rv_genesets <- shiny::reactiveValues(
      text = NULL,
      filter_text = NULL,
      success = FALSE,
      succes_filter = FALSE,
      genesets = NULL,
      filtered_genesets = NULL
    )
  } else {
    shinyjs::runjs("$('#vto_load_genesets').css('color', '#32CD32');")
    rv_genesets <- shiny::reactiveValues(
      text = "Successfully loaded: org.Mm.eg.db",
      text_filter = NULL,
      success = TRUE,
      success_filter = FALSE,
      genesets = mm_genesets
    )
  }
  
  #### reactive values ----
  rv_load_genelists <- shiny::reactiveValues(
    text = NULL,
    success = FALSE
  )
  rv_genelists_overlap <- shiny::reactiveValues(
    text = NULL,
    success = FALSE,
    gene_overview = NULL,
    plot = NULL
  )
  rv_set_significant_genes <- shiny::reactiveValues(
    text = NULL,
    success = FALSE
  )
  rv_set_names <- shiny::reactiveValues(
    text = NULL,
    success = FALSE
  )
  rv_enrichment <- shiny::reactiveValues(
    results = list(),
    text = NULL,
    success = FALSE,
    results_filtered = list(),
    current_enrichment = NULL
  )
  rv_genelists <- shiny::reactiveVal(list())
  rv_splitdot <- shiny::reactiveValues(
    plot = NULL
  )
  rv_termtree <- shiny::reactiveValues(
    plot = NULL
  )
  rv_icheatmap <- shiny::reactiveValues(
    heatmap = NULL,
    success = FALSE,
    text = NULL,
    row_i = NULL,
    col_i = NULL,
    row_names = NULL,
    col_names = NULL
  )
  
  #### observers for pathing ----
  shiny::observe({
    if (rv_load_genelists$success == TRUE & rv_genesets$success_filter == TRUE) {
      shinyjs::enable("ab_go_to_enrichment")
      shinyjs::enable("ab_run_enrichment")
      shinyjs::enable("ab_go_to_enrichment_from_overlap")
      if (length(rv_genesets$filtered_genesets) > 1) {
        shinyjs::enable("ab_go_to_overlap")
        shinyjs::enable("ab_run_genelist_overlap")
      }
    } else {
      shinyjs::disable("ab_go_to_enrichment")
      shinyjs::disable("ab_run_enrichment")
      shinyjs::disable("ab_go_to_enrichment_from_overlap")
      shinyjs::disable("ab_go_to_overlap")
      shinyjs::disable("ab_run_genelist_overlap")
    }
  })
  shiny::observe({
    if (rv_genelists_overlap$success) {
      shinyjs::enable("ab_plot_overlap_venn") 
      shinyjs::enable("ab_plot_overlap_upset")
    } else {
      shinyjs::disable("ab_plot_overlap_venn")
      shinyjs::disable("ab_plot_overlap_upset")
    } 
  })
  shiny::observe({
    button_ids <- c(
      "ab_filter_enrichment", "ab_go_to_PPI", "ab_go_to_PPI_icheatmap", "ab_go_to_PPI_splitdot", "ab_go_to_PPI_termtree",
      "ab_go_to_heatmap", "ab_go_to_heatmap_PPI", "ab_go_to_heatmap_splitdot", "ab_go_to_heatmap_termtree",
      "ab_go_to_splitdot", "ab_go_to_splitdot_PPI", "ab_go_to_splitdot_icheatmap", "ab_go_to_splitdot_termtree",
      "ab_go_to_termtree", "ab_go_to_termtree_PPI", "ab_go_to_termtree_icheatmap", "ab_go_to_termtree_splitdot",
      "ab_icheatmap_plot", "ab_splitdot_plot", "ab_termtree_plot"
    )
    if (rv_enrichment$success) {
      purrr::walk(button_ids, shinyjs::enable)
      shinyjs::show("div_enrichment")
    } else {
      purrr::walk(button_ids, shinyjs::disable)
      shinyjs::hide("div_enrichment")
    }
  })
  shiny::observe(if (rv_load_genelists$success) shinyjs::enable("ab_set_significant_genes") else shinyjs::disable("ab_set_significant_genes"))
  shiny::observe(if (rv_set_significant_genes$success) shinyjs::enable("ab_set_names") else shinyjs::disable("ab_set_names"))
  shiny::observe(if (rv_genesets$success) shinyjs::enable("ab_filter_genesets") else shinyjs::disable("ab_filter_genesets"))
  shiny::observe(if ( ! is.null(rv_splitdot$plot)) shinyjs::show("db_splitdot") else shinyjs::hide("db_splitdot"))
  shiny::observe(if ( ! is.null(rv_termtree$plot)) shinyjs::show("db_termtree") else shinyjs::hide("db_termtree"))
  
  #### observe events ----
  shiny::observeEvent(input$ab_icheatmap_plot, {
    shinyjs::show("ab_icheatmap_plot_loader")
    shinyjs::runjs("$('#vto_icheatmap').css('color', '#ff0000');")
    data_genelists <- rv_genelists()
    
    ch <- plot_ComplexHeatmap(
      enrichment_result = rv_enrichment$current_enrichment,
      genelist = data_genelists[[input$si_show_enrichment]], 
      cluster_method = input$si_icheatmap_cluster_method,
      n_cluster = input$ni_icheatmap_nclusters,
      n_top_terms = input$ni_icheatmap_nterms,
      n_top_genes = input$ni_icheatmap_ngenes,
      genelist_overlap = if (is.null(rv_genelists_overlap$gene_overview)) NULL else rv_genelists_overlap$gene_overview,
      plot = FALSE
    )
    rv_icheatmap$heatmap <- ch
    InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, ch, heatmap_id = "icheatmap", click_action = icheatmap_action, brush_action = icheatmap_action)
    rv_icheatmap$success <- TRUE 
    shinyjs::runjs("$('#vto_icheatmap').css('color', '#32CD32');")
    rv_icheatmap$text <- "Successfully plotted InteractiveComplexHeatmap: click or drag cells for info and to select gene(s) or gene(s) by term(s)"
    shinyjs::hide("ab_icheatmap_plot_loader")
  })
  icheatmap_action <- function(df, output) { # on click or brush in InteractiveComplexHeatmap
    if(is.null(df)) {
      shiny::showNotification("Selected nothing, keep last selection")
    } else {
      rv_icheatmap$row_i = unique(unlist(df$row_index))
      rv_icheatmap$col_i = unique(unlist(df$column_index))
      rv_icheatmap$row_names = unique(unlist(df$row_label))
      rv_icheatmap$col_names = unique(unlist(df$column_label))
      rv_icheatmap$text <- paste0("PPI selection: if genes by terms: ", paste(rv_icheatmap$row_names, collapse = ", "), ". If genes: ", paste(rv_icheatmap$col_names, collapse = ", "))
    }
  }
  
  shiny::observeEvent(input$ab_termtree_plot, {
    shinyjs::show("ab_termtree_plot_loader")
    rv_termtree$plot <- plot_termtree(enrichment = rv_enrichment$current_enrichment, Nterms = input$ni_termtree_Nterms, Nwords = input$ni_termtree_Nwords, Nclusters = input$ni_termtree_Nclusters)
    if (is.null(rv_termtree$plot)) shiny::showNotification("'enrichplot' package required for plotting")
    shinyjs::hide("ab_termtree_plot_loader")
  })
  shiny::observeEvent(input$ab_splitdot_plot, {
    shinyjs::show("ab_splitdot_plot_loader")
    rv_splitdot$plot <- plot_splitdot(enrichment = rv_enrichment$current_enrichment, topN = input$ni_splitdot_topN)
    shinyjs::hide("ab_splitdot_plot_loader")
  })
  
  shiny::observeEvent(input$ab_enrichment_reset, {
    rv_enrichment$results_filtered <- rv_enrichment$results
  })
  
  shiny::observeEvent(input$ab_filter_enrichment, {
    shinyjs::show("ab_filter_enrichment_loader")
    dts <- rv_enrichment$results
    for (name in names(dts)) {
      dt <- dts[[name]]
      rv_enrichment$results_filtered[[name]] <- filter_enrichment(
        df = dt,
        genes_input = input$tai_enrichment_filter_gene_query,
        genes_any_all = input$tai_enrichment_filter_gene_query_allany,
        terms_query = input$tai_enrichment_filter_term_query,
        terms_query_all_any = input$rb_enrichment_filter_term_query_allany,
        terms_antiquery = input$tai_enrichment_filter_term_antiquery,
        terms_antiquery_all_any = input$rb_enrichment_filter_term_antiquery_allany,
        min_ngenes = input$ni_enrichment_filter_ngenes,
        min_ngenes_input = input$ni_enrichment_filter_ngenes_input,
        min_ngenes_signif = input$ni_enrichment_filter_ngenes_signif,
        min_abs_zscore = input$ni_enrichment_filter_zscore,
        max_pvalue_adjust = input$ni_enrichment_filter_pvalue_adjust
      )
    }
    shinyjs::hide("ab_filter_enrichment_loader")
  })
  
  observeEvent({
    rv_enrichment$results_filtered
    input$si_show_enrichment
    input$si_show_enrichment_source}, {
      req(length(rv_enrichment$results_filtered) != 0)
      
      if (input$si_show_enrichment == "") {
        dt <- rv_enrichment$results_filtered[[1]]
      } else {
        dt <- rv_enrichment$results_filtered[[input$si_show_enrichment]]
      }
      if (input$si_show_enrichment_source == "") {
        dt <- dt %>% filter(source == unique(dt$source)[1])
      } else {
        dt <- dt %>% filter(source == input$si_show_enrichment_source)
      }
      rv_enrichment$current_enrichment <- dt
      
      dt <- dt[,c("name", "ngenes_input", "ngenes", "ngenes_signif", "zscore", "pvalue_adjust")]
      dt <- dt %>% mutate(
        zscore = round(zscore, 2),
        pvalue_adjust = round(pvalue_adjust, 2)
      )
      output$dto_test_enrichment <- DT::renderDT({
        DT::datatable(dt, options = list(
          pageLength = 5,
          lengthMenu = c(5,10, 25, 50, 100),
          dom = "Bfrtip",
          searching = FALSE,
          selection = 'none',
          colResize = list(resize = TRUE),
          ## set text color of table info and paginators based on CSS style file after rendering datatable
          initComplete = DT::JS("
          function(settings, json) {
            const textColor = getComputedStyle(document.documentElement).getPropertyValue('--text-color').trim();
          
            $(this.api().table().container()).find('.dataTables_info').css({
              'color': textColor,
              'font-weight': 'bold'
            });
            $(this.api().table().container()).find('.dataTables_paginate').css({
              'color': textColor,
              'font-weight': 'bold'
            });
          }
        ")), 
          extensions = c("ColReorder")
        )
      })
      
    })
  
  shiny::observeEvent(input$ab_run_genelist_overlap, {
    rv_genelists_overlap$success <- FALSE
    shinyjs::runjs("$('#vto_genelist_overlap').css('color', '#ff0000');")
    shinyjs::show("ab_run_genelist_overlap_loader")
    ## run genelists overlap with UI parameters and global settings
    genelist_overlap_result <- run_genelists_overlap(
      genelists = rv_genelists(),
      annotate_genes = input$cbi_annotate_genes,
      annotation_organism = input$si_organism
    )
    shinyjs::hide("ab_run_genelist_overlap_loader")
    if (is.character(genelist_overlap_result)) {
      rv_genelists_overlap$text <- genelist_overlap_result
    } else {
      rv_genelists_overlap$gene_overview <- genelist_overlap_result
    }
    req( ! is.character(genelist_overlap_result))
  
    rv_genelists_overlap$success <- TRUE
    rv_genelists_overlap$text <- "Overlapped genelists, created gene overview, information will be added when running enrichment"
    shinyjs::runjs("$('#vto_genelist_overlap').css('color', '#32CD32');")
  })
  
  shiny::observeEvent(input$ab_plot_overlap_venn, {
    data <- rv_genelists()
    shinyjs::hide("db_overlap_plot")
    shinyjs::show("ab_plot_overlap_venn_loader")
    rv_genelists_overlap$plot <- plot_genelists_overlap_venn(
      genelists = data
    )
    shinyjs::hide("ab_plot_overlap_venn_loader")
    shinyjs::show("db_overlap_plot")
  })
  
  shiny::observeEvent(input$ab_plot_overlap_upset, {
    data <- rv_genelists()
    shinyjs::hide("db_overlap_plot")
    shinyjs::show("ab_plot_overlap_upset_loader")
    rv_genelists_overlap$plot <- plot_genelists_overlap_upset(
      genelists = data,
      grayscale_colors = input$cbi_plot_overlap_upset_grayscale,
      empty_intersections = input$cbi_plot_overlap_upset_intersections
    )
    shinyjs::hide("ab_plot_overlap_upset_loader")
    shinyjs::show("db_overlap_plot")
  })
  
  shiny::observeEvent(input$ab_run_enrichment, {
    data_genelists <- rv_genelists()
    data_genesets <- rv_genesets$filtered_genesets
    
    rv_enrichment$success <- FALSE
    shinyjs::runjs("$('#vto_test_enrichment').css('color', '#ff0000');") # failure color
    shinyjs::show("ab_run_enrichment_loader")
    
    for (name in names(data_genelists)) {
      enrichment_results <- goatea::run_geneset_enrichment(
        genesets = data_genesets[[name]], 
        genelist = data_genelists[[name]],
        method = input$si_test_method,
        score_type = input$si_test_score_type,
        padj_method = input$si_test_padj_method,
        padj_sources = input$cbi_test_padj_sources,
        padj_cutoff = input$ni_test_padj_cutoff, 
        padj_min_signifgenes = input$ni_test_padj_min_signifgenes
      )
      rv_enrichment$results[[name]] <- enrichment_results
      rv_enrichment$results_filtered[[name]] <- enrichment_results
    }
    updateSelectInput(
      session,
      "si_show_enrichment",
      choices = names(data_genelists),
      selected = names(data_genelists)[1] 
    )
    updateSelectInput(
      session,
      "si_show_enrichment_source",
      choices = unique(data_genesets[[1]]$source),
      selected = unique(data_genesets[[1]]$source)[1] 
    )
    updateSelectInput(
      session,
      "si_show_enrichment_icheatmap",
      choices = names(data_genelists),
      selected = names(data_genelists)[1] 
    )
    updateSelectInput(
      session,
      "si_show_enrichment_source_icheatmap",
      choices = unique(data_genesets[[1]]$source),
      selected = unique(data_genesets[[1]]$source)[1] 
    )
    
    shinyjs::hide("ab_run_enrichment_loader")
    rv_enrichment$success <- TRUE
    shinyjs::runjs("$('#vto_test_enrichment').css('color', '#32CD32');") # success color
    rv_enrichment$text <- "Enrichment ran successfully"
  })
  
  shiny::observeEvent(input$ab_set_significant_genes, {
    data <- rv_genelists()
    pvalue_threshold <- input$ni_set_significant_pvalue
    effectsize_threshold <- input$ni_set_significant_effectsize
    
    for (name in names(data)) {
      data[[name]]$signif <- data[[name]][,"pvalue"] <= pvalue_threshold & abs(data[[name]][,"effectsize"]) >= effectsize_threshold
    }
    
    rv_genelists(data)
    rv_set_significant_genes$success <- TRUE
    shinyjs::runjs("$('#vto_set_significant_genes').css('color', '#32CD32');")
    rv_set_significant_genes$text <- "Set significant genes in genelists"
  })
  
  shiny::observeEvent(input$ab_set_names, {
    data <- rv_genelists()
    
    names <- strsplit(input$ti_set_names, ' ')[[1]]
    names <- names[nzchar(names)]
    
    if (length(names) == length(names(data)) & ! any(duplicated(names))) {
      names(data) <- names
      rv_genelists(data)
      shinyjs::enable("ab_load_GOB_genesets")
      shinyjs::runjs("$('#vto_set_names').css('color', '#32CD32');")
      rv_set_names$text <- paste0("Successfully set names: ", paste0(names, collapse = ', '))
      rv_set_names$success <- FALSE
    } else {
      shinyjs::disable("ab_load_GOB_genesets")
      shinyjs::disable("ab_load_GOB_genesets")
      shinyjs::runjs("$('#vto_set_names').css('color', '#ff0000');")
      rv_set_names$text <- paste0("Need ", length(names(data)), " unique names, separated by spaces")
      rv_set_names$success <- TRUE
    }
  })
  
  shiny::observeEvent(input$ab_filter_genesets, {
    data <- rv_genelists()
    results <- list()
    
    rv_genesets$success_filter <- FALSE
    shinyjs::runjs("$('#vto_filter_genesets').css('color', 'white');")
    rv_genesets$text_filter <- "Filtering..."
    
    for (name in names(data)) {
      shinyjs::show("ab_filter_genesets_loader")
      results[[name]] <- goat::filter_genesets(
        genesets = rv_genesets$genesets,
        genelist = data[[name]],
        min_overlap = input$ni_genesets_min_overlap,
        max_overlap = input$ni_genesets_max_overlap,
        max_overlap_fraction = input$ni_genesets_max_overlap_fraction,
        dedupe = input$cbi_genesets_dedupe
      )
      shinyjs::hide("ab_filter_genesets_loader")
    }
    rv_genesets$filtered_genesets <- results
    shinyjs::runjs("$('#vto_filter_genesets').css('color', '#32CD32');")
    rv_genesets$text_filter <- "Successfully filtered genesets"
    rv_genesets$success_filter <- TRUE
  })
  
  # Observe the button click and open file browser
  shiny::observeEvent(input$ab_load_genelists, {
    filepaths <- utils::choose.files()
    data <- rv_genelists()
    for (file in filepaths) {
      genelist <- read_validate_genelist(
        file = file, 
        remove_non_numerical_ids = input$cbi_remove_non_numerical_ids, 
        remove_duplicated = input$cbi_remove_duplicated,
        remove_Rik_genes = input$cbi_remove_Rik_genes,
        remove_Gm_genes = input$cbi_remove_Gm_genes,
        keep_maxN_genes = input$cbi_keep_maxN_genes)
      
      if(is.character(genelist)) {
        shinyjs::runjs("$('#vto_load_genelists').css('color', '#ff0000');")
        rv_load_genelists$text <- paste(genelist)
        rv_load_genelists$success <- FALSE
        break
      } else {
        data[[basename(file)]] <- genelist
        shinyjs::runjs("$('#vto_load_genelists').css('color', '#32CD32');")
        rv_load_genelists$text <- paste(basename(filepaths), nrow(genelist))
        rv_load_genelists$success <- TRUE
      }
    }
    rv_genelists(data)
  })
  
  # load Gene Ontology Bioconductor genesets based on selected organism
  shiny::observeEvent(input$ab_load_GOB_genesets, {
    shinyjs::show("ab_load_GOB_genesets_loader")
    rv_genesets$success <- FALSE
    taxid = input$si_organism
    rv_genesets$genesets <- goat::load_genesets_go_bioconductor(taxid = taxid)
    rv_genesets$success <- TRUE
    shinyjs::runjs("$('#vto_load_genesets').css('color', '#32CD32');")
    rv_genesets$text <- paste0("Successfully loaded: org.", input$si_organism, ".eg.db")
    shinyjs::hide("ab_load_GOB_genesets_loader")
  })
  
  #### download button handlers ----
  output$db_termtree <- shiny::downloadHandler(
    filename = "TermTreePlot.png",
    content = function(file) ggplot2::ggsave(file, rv_termtree$plot, bg = "white", width = 30, height = 20, units = "cm"),
    contentType = "png/image"
  )
  output$db_splitdot <- shiny::downloadHandler(
    filename = "SplitDotPlot.png",
    content = function(file) ggplot2::ggsave(file, rv_splitdot$plot, bg = "white", width = 30, height = 20, units = "cm"),
    contentType = "png/image"
  )
  output$db_overlap_plot <- downloadHandler(
    filename = "overlap_plot.png",
    content = function(file) {
      if (class(rv_genelists_overlap$plot)[1] == "upset") {
        png(file)
        print(rv_genelists_overlap$plot)
        dev.off()
      } else {
        ggplot2::ggsave(file, rv_genelists_overlap$plot)
      }
    },
    contentType = "png/image"
  )
  output$db_enrichment_current <- shiny::downloadHandler(
    filename = function() {
      req(input$si_show_enrichment)
      paste0("enrichment_", input$si_show_enrichment)
    },
    content = function(file) {
      req(rv_enrichment$results_filtered)
      req(input$si_show_enrichment)
      dt <- rv_enrichment$results_filtered[[input$si_show_enrichment]]
      dt_clean <- dt %>% mutate(across(where(is.list), ~ sapply(., toString)))
      if (input$rb_global_output_type == ".csv") {
        write.csv2(dt_clean, file = file)
      } else if (input$rb_global_output_type == ".xlsx") {
        openxlsx::write.xlsx(dt_clean, file = file)
      }
    },
    contentType = "text/csv/xlsx"
  )
  output$db_enrichment_all <- shiny::downloadHandler(
    filename = function() {
      paste0("enrichment.zip")
    },
    content = function(file) {
      req(rv_enrichment$results_filtered)
      dts <- rv_enrichment$results_filtered
      
      ## go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      files <- NULL;
      for (name in names(dts)) {
        dt <- dts[[name]]
        dt_clean <- dt %>% mutate(across(where(is.list), ~ sapply(., toString)))
        
        filename <- paste0("enrichment_", name)
        if (input$rb_global_output_type == ".csv") {
          write.csv2(dt_clean, file = filename)
        } else if (input$rb_global_output_type == ".xlsx") {
          openxlsx::write.xlsx(dt_clean, file = filename)
        }
        files <- c(filename, files)
      }
      zip(file, files)
    },
    contentType = "text/csv/xlsx"
  )
  
  #### render texts for verbatimTextOutputs ----
  output$vto_load_genelists <- shiny::renderText({paste(rv_load_genelists$text, collapse = "\n")})
  output$vto_load_genesets <- shiny::renderText({rv_genesets$text})
  output$vto_filter_genesets <- shiny::renderText({rv_genesets$text_filter})
  output$vto_set_significant_genes <- shiny::renderText({rv_set_significant_genes$text})
  output$vto_set_names <- shiny::renderText({rv_set_names$text})
  output$vto_test_enrichment <- shiny::renderText({rv_enrichment$text})
  output$vto_genelist_overlap <- shiny::renderText({rv_genelists_overlap$text})
  output$vto_icheatmap <- shiny::renderText({rv_icheatmap$text})
  
  #### render plots for plotOutputs ----
  output$po_genelist_overlap <- shiny::renderPlot({
    req(rv_genelists_overlap$plot)
    print(rv_genelists_overlap$plot)
    shiny::showNotification("NOTE: plot size is draggable from edges")
  }, bg = "transparent")
  output$po_splitdot <- shiny::renderPlot({
    if ( ! is.null(rv_splitdot$plot)) print(ggplot2::ggplot_build(rv_splitdot$plot))
    shiny::showNotification("NOTE: plot size is draggable from edges")
  })
  output$po_termtree <- shiny::renderPlot({
    if ( ! is.null(rv_termtree$plot)) print(ggplot2::ggplot_build(rv_termtree$plot))
    shiny::showNotification("NOTE: plot size is draggable from edges")
  })
  
  #### GO TO pathing buttons ----
  shiny::observeEvent(c(input$ab_go_to_enrichment, input$ab_go_to_enrichment_from_overlap), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_run_enrichment")
  })
  shiny::observeEvent(input$ab_go_to_overlap, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_run_genelist_overlap")
  })
  shiny::observeEvent(c(input$ab_go_to_heatmap, input$ab_go_to_heatmap_PPI, input$ab_go_to_heatmap_splitdot, input$ab_go_to_heatmap_termtree), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_plot_heatmap")
  })
  shiny::observeEvent(c(input$ab_go_to_PPI, input$ab_go_to_PPI_icheatmap, input$ab_go_to_PPI_splitdot, input$ab_go_to_PPI_termtree), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_plot_PPI")
  })
  shiny::observeEvent(c(input$ab_go_to_splitdot, input$ab_go_to_splitdot_PPI, input$ab_go_to_splitdot_icheatmap, input$ab_go_to_splitdot_termtree), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_plot_splitdot")
  })
  shiny::observeEvent(c(input$ab_go_to_termtree, input$ab_go_to_termtree_PPI, input$ab_go_to_termtree_icheatmap, input$ab_go_to_termtree_splitdot), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_plot_termtree")
  })
}