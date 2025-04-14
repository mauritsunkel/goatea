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
goatea_server <- function(input, output, session, css_colors, stringdb_versions, mm_genesets = NULL) {
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
  rv_volcano <- shiny::reactiveValues(
    text = NULL,
    success = FALSE,
    plot = NULL
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
  rv_genes <- shiny::reactiveValues(
    all_selected = character(0)
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
  rv_genefsi_icheatmap <- shiny::reactiveValues(
    heatmap = NULL,
    success = FALSE,
    text = NULL,
    row_i = NULL,
    col_i = NULL,
    row_names = NULL,
    col_names = NULL
  )
  rv_ppi <- reactiveValues(
    nodes = NULL,
    edges = NULL,
    g = NULL,
    p = character(0), 
    stringdb_versions = stringdb_versions
  )
  rv_ppi_subgraph <- reactiveValues(
    nodes = NULL,
    edges = NULL,
    g = NULL,
    p = NULL,
  )
  observe({ req(rv_ppi$stringdb_versions)
    isolate({updateSelectInput(session, "si_ppi_version", choices = rv_ppi$stringdb_versions, selected = tail(rv_ppi$stringdb_versions, n = 1)
    )})})
  ppi_layout_choices <- list( 
    "Edge-weighted" = "layout_with_mds", # default: edge-weight represent distances
    "General" = "layout_nicely", # general
    "Medium size" = "layout_with_fr", # medium size graph layout
    "Large size" = "layout_with_lgl", # large size graph layout
    "Grid" = "layout_on_grid", # visually different
    "Circle" = "layout_in_circle", # visually different
    "Gem" = "layout_with_gem" # visually different
  )
  updateSelectInput(session, "si_ppi_layout", choices = ppi_layout_choices)
  updateSelectInput(session, "si_ppi_layout_subgraph", choices = ppi_layout_choices)
  
  #### observers for pathing ----
  shiny::observe({
    if (rv_load_genelists$success == TRUE & rv_genesets$success_filter == TRUE) {
      shinyjs::enable("ab_go_to_enrichment")
      shinyjs::enable("ab_go_to_volcano")
      shinyjs::enable("ab_go_to_overlap")
      shinyjs::enable("ab_run_enrichment")
      shinyjs::enable("ab_go_to_enrichment_from_overlap")
      if (length(rv_genesets$filtered_genesets) > 1) {
        shinyjs::enable("ab_run_genelist_overlap")
      }
    } else {
      shinyjs::disable("ab_go_to_enrichment")
      shinyjs::disable("ab_run_enrichment")
      shinyjs::disable("ab_go_to_enrichment_from_overlap")
      shinyjs::disable("ab_go_to_volcano")
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
      "ab_go_to_heatmap", "ab_go_to_genefsi_icheatmap", "ab_go_to_heatmap_PPI", "ab_go_to_heatmap_splitdot", "ab_go_to_heatmap_termtree",
      "ab_go_to_splitdot", "ab_go_to_splitdot_PPI", "ab_go_to_splitdot_icheatmap", "ab_go_to_splitdot_termtree",
      "ab_go_to_termtree", "ab_go_to_termtree_PPI", "ab_go_to_termtree_icheatmap", "ab_go_to_termtree_splitdot",
      "ab_icheatmap_plot", "ab_splitdot_plot", "ab_termtree_plot", "ab_go_to_genefsi_icheatmap_splitdot", "ab_go_to_genefsi_icheatmap_termtree",
      "ab_go_to_genefsi_icheatmap_icheatmap", "ab_go_to_genefsi_icheatmap_PPI"
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
  shiny::observeEvent(input$ab_ppi_graph, {
    rv_ppi$p <- unique(c(rv_genes$all_selected, process_string_input(input$tai_ppi_add_protgenes)))
    req(rv_ppi$p) # cannot be empty: character(0)
    
    shinyjs::show('ab_ppi_graph_loader')
    PPI <- get_string_ppi(aliases = rv_ppi$p, score_threshold = input$ni_ppi_score_threshold, version = input$si_ppi_version, versions = rv_ppi$stringdb_versions, organism = as.numeric(input$si_organism), folder = set_base_folder(input$ti_global_base_folder))
    g <- get_ppigraph(PPI)
    genes_overview <- NULL
    if ( ! is.null(rv_genelists_overlap$gene_overview)) genes_overview <- rv_genelists_overlap$gene_overview
    ## set sample selection
    data_genelists <- rv_genelists()
    vis <- get_visNetwork(g, genes_overview = genes_overview, sample_name = input$si_ppi_sample)
    rv_ppi$nodes <- vis$nodes
    rv_ppi$edges <- vis$edges
    rv_ppi$g <- vis$ppigraph
    ## set select inputs after rendering visnetwork ppigraph
    nodes_sample <- names(rv_ppi$nodes)[grepl(paste0(input$si_ppi_sample, '_'), names(rv_ppi$nodes))]
    nodes_equal <- intersect(names(rv_ppi$nodes), c('cluster', 'degree', 'betweenness', 'closenss', 'knn', 'diversity', 'genelist_overlap', 'signif', 'updown'))
    updateSelectInput(session, "si_ppi_color_nodes", choices = c(nodes_equal, nodes_sample))
    updateSelectInput(session, "si_ppi_color_edges", choices = setdiff(names(rv_ppi$edges), c('id', "from", "to", "width", "title")))
    updateSelectInput(session, "si_ppi_highlight_cluster", choices = unique(rv_ppi$nodes$cluster))
    
    ## set subgraph values
    updateSelectInput(session, "si_ppi_color_nodes_subgraph", choices = c(nodes_equal, nodes_sample))
    updateSelectInput(session, "si_ppi_color_edges_subgraph", choices = setdiff(names(rv_ppi$edges), c('id', "from", "to", "width", "title")))
    rv_ppi_subgraph$nodes <- rv_ppi$nodes
    rv_ppi_subgraph$edges = rv_ppi$edges
    rv_ppi_subgraph$g = rv_ppi$g
    rv_ppi_subgraph$p = rv_ppi$p
    
    shinyjs::hide('ab_ppi_graph_loader')
  })
  observeEvent(input$si_ppi_color_edges, {
    req(input$si_ppi_color_edges)
    values = rv_ppi$edges[[input$si_ppi_color_edges]]
    colors = colorify(colors = c('white', 'red'), colors_breakpoints = c(min(values), max(values)))(values)
    visNetwork::visNetworkProxy("vno_ppi_visnetwork") %>%
      visNetwork::visUpdateEdges(data.frame(
        from = rv_ppi$edges$from,
        to = rv_ppi$edges$to,
        color.color = colors
      ))
  })
  observeEvent(input$si_ppi_color_edges_subgraph, {
    req(input$si_ppi_color_edges_subgraph)
    values <- rv_ppi_subgraph$edges[[input$si_ppi_color_edges_subgraph]]
    colors <- colorify(colors = c('white', 'red'), colors_breakpoints = c(min(values), max(values)))(values)
    visNetwork::visNetworkProxy("vno_ppi_visnetwork_subgraph") %>%
      visNetwork::visUpdateEdges(data.frame(
        from = rv_ppi_subgraph$edges$from,
        to = rv_ppi_subgraph$edges$to,
        color.color = colors
      ))
  })
  
  observeEvent(input$si_ppi_color_nodes, {
    req(c(input$si_ppi_color_nodes, input$si_ppi_color_nodes_type))
    
    values = rv_ppi$nodes[[input$si_ppi_color_nodes]]
    values[is.na(values)] <- 0
    
    colors <- if (input$si_ppi_color_nodes %in% c('updown')) {
      c('1' = 'red', '-1' = 'blue', '0' = 'black')[as.character(values)]
    } else if (input$si_ppi_color_nodes %in% c('signif')) {
      c('1' = 'red', '-1' = 'black', '0' = 'black')[as.character(values)]
    } else if (input$si_ppi_color_nodes %in% c('cluster', 'genelist_overlap')) {
      colors <- setNames(colorify(length(unique(values))+1, colors = 'Okabe-Ito')[2:(length(unique(values))+1)], unique(values))[values]
      colors[is.na(colors)] <- 'black'
      colors
    } else {
      colorify(colors = c('black', 'red'), colors_breakpoints = c(min(values, na.rm = T), max(values, na.rm = T)))(values)
    }
    
    if (input$si_ppi_color_nodes_type == 'Background') {
      visNetwork::visNetworkProxy("vno_ppi_visnetwork") %>%
        visNetwork::visUpdateNodes(data.frame(
          id = rv_ppi$nodes$id, 
          color.background = colors,
          font.background = colors
        ))
    } else if (input$si_ppi_color_nodes_type == 'Border') {
      visNetwork::visNetworkProxy("vno_ppi_visnetwork") %>%
        visNetwork::visUpdateNodes(data.frame(
          id = rv_ppi$nodes$id, 
          color.border = colors
        ))
    }
  }, ignoreInit = TRUE)
  observeEvent(input$si_ppi_color_nodes_subgraph, {
    req(c(input$si_ppi_color_nodes_subgraph, input$si_ppi_color_nodes_type_subgraph))
    
    values = rv_ppi_subgraph$nodes[[input$si_ppi_color_nodes_subgraph]]
    values[is.na(values)] <- 0
    
    colors <- if (input$si_ppi_color_nodes_subgraph %in% c('updown')) {
      c('1' = 'red', '-1' = 'blue', '0' = 'black')[as.character(values)]
    } else if (input$si_ppi_color_nodes_subgraph %in% c('signif')) {
      c('1' = 'red', '-1' = 'black', '0' = 'black')[as.character(values)]
    } else if (input$si_ppi_color_nodes_subgraph %in% c('cluster', 'genelist_overlap')) {
      colors <- setNames(colorify(length(unique(values))+1, colors = 'Okabe-Ito')[2:(length(unique(values))+1)], unique(values))[values]
      colors[is.na(colors)] <- 'black'
      colors
    } else {
      colorify(colors = c('black', 'red'), colors_breakpoints = c(min(values, na.rm = T), max(values, na.rm = T)))(values)
    }
    
    if (input$si_ppi_color_nodes_type_subgraph == 'Background') {
      visNetwork::visNetworkProxy("vno_ppi_visnetwork_subgraph") %>%
        visNetwork::visUpdateNodes(data.frame(
          id = rv_ppi_subgraph$nodes$id, 
          color.background = colors,
          font.background = colors
        ))
    } else if (input$si_ppi_color_nodes_type_subgraph == 'Border') {
      visNetwork::visNetworkProxy("vno_ppi_visnetwork_subgraph") %>%
        visNetwork::visUpdateNodes(data.frame(
          id = rv_ppi_subgraph$nodes$id, 
          color.border = colors
        ))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$ab_ppi_highlight_gene, {
    gene <- input$ti_ppi_highlight_gene
    visNetwork::visNetworkProxy("vno_ppi_visnetwork") %>%
      visNetwork::visSelectNodes(
        id = rv_ppi$nodes$id[rv_ppi$nodes$label == toupper(gene)],
        highlightEdges = FALSE,
        clickEvent = FALSE
      )
  })
  observeEvent(input$ab_ppi_highlight_cluster, {
    cluster <- input$si_ppi_highlight_cluster
    visNetwork::visNetworkProxy("vno_ppi_visnetwork") %>%
      visNetwork::visSelectNodes(
        id = rv_ppi$nodes$id[rv_ppi$nodes$cluster == cluster],
        highlightEdges = FALSE,
        clickEvent = FALSE
      )
  })
  observeEvent(input$ab_ppi_highlight_gene_subgraph, {
    gene <- input$ti_ppi_highlight_gene_subgraph
    visNetwork::visNetworkProxy("vno_ppi_visnetwork_subgraph") %>%
      visNetwork::visSelectNodes(
        id = rv_ppi_subgraph$nodes$id[rv_ppi_subgraph$nodes$label == toupper(gene)],
        highlightEdges = FALSE,
        clickEvent = FALSE
      )
  })
  observeEvent(input$ab_ppi_highlight_cluster_subgraph, {
    cluster <- input$si_ppi_highlight_cluster_subgraph
    visNetwork::visNetworkProxy("vno_ppi_visnetwork_subgraph") %>%
      visNetwork::visSelectNodes(
        id = rv_ppi_subgraph$nodes$id[rv_ppi_subgraph$nodes$cluster == cluster],
        highlightEdges = FALSE,
        clickEvent = FALSE
      )
  })
  
  observeEvent(input$ab_ppi_highlight_nodes, {
    if (input$si_ppi_highlight_nodes == 'Nodes') {
      feature <- input$si_ppi_color_nodes
      values <- rv_ppi$nodes[[feature]]
      ind <- values >= input$ni_ppi_highlight_nodes_min & values <= input$ni_ppi_highlight_nodes_max
      ids <- rv_ppi$nodes$id[ind]
      
      visNetwork::visNetworkProxy("vno_ppi_visnetwork") %>%
        visNetwork::visSelectNodes(
          id = ids,
          highlightEdges = FALSE,
          clickEvent = FALSE
        )
    } else if (input$si_ppi_highlight_nodes == 'Edges') {
      feature <- input$si_ppi_color_edges
      values <- rv_ppi$edges[[feature]]
      ind <- values >= input$ni_ppi_highlight_nodes_min & values <= input$ni_ppi_highlight_nodes_max
      ids <- rv_ppi$edges$id[ind]

      visNetwork::visNetworkProxy("vno_ppi_visnetwork") %>%
        visNetwork::visSetSelection(
          edgesId = ids,
          nodesId = unique(unlist(strsplit(ids, "_"))),
          unselectAll = TRUE,
          highlightEdges = FALSE,
          clickEvent = FALSE
        )
    }
  })
  observeEvent(input$ab_ppi_highlight_nodes_subgraph, {
    if (input$si_ppi_highlight_nodes_subgraph == 'Nodes') {
      feature <- input$si_ppi_color_nodes_subgraph
      values <- rv_ppi_subgraph$nodes[[feature]]
      ind <- values >= input$ni_ppi_highlight_nodes_min_subgraph & values <= input$ni_ppi_highlight_nodes_max_subgraph
      ids <- rv_ppi_subgraph$nodes$id[ind]
      
      visNetwork::visNetworkProxy("vno_ppi_visnetwork_subgraph") %>%
        visNetwork::visSelectNodes(
          id = ids,
          highlightEdges = FALSE,
          clickEvent = FALSE
        )
    } else if (input$si_ppi_highlight_nodes_subgraph == 'Edges') {
      feature <- input$si_ppi_color_edges_subgraph
      values <- rv_ppi_subgraph$edges[[feature]]
      ind <- values >= input$ni_ppi_highlight_nodes_min_subgraph & values <= input$ni_ppi_highlight_nodes_max_subgraph
      ids <- rv_ppi_subgraph$edges$id[ind]
      
      visNetwork::visNetworkProxy("vno_ppi_visnetwork_subgraph") %>%
        visNetwork::visSetSelection(
          edgesId = ids,
          nodesId = unique(unlist(strsplit(ids, "_"))),
          unselectAll = TRUE,
          highlightEdges = FALSE,
          clickEvent = FALSE
        )
    }
  })
  
  observeEvent(input$ab_ppi_subset, {
    runjs("Shiny.setInputValue('ppigraph_update', 'ab_ppi_subset', {priority: 'event'});")
  })
  observeEvent(input$ab_ppi_delete_nodes_subgraph, {
    runjs("Shiny.setInputValue('ppigraph_update', 'ab_ppi_delete_nodes_subgraph', {priority: 'event'});")
  })
  observeEvent(input$ppigraph_update, {
    req(input$ppigraph_update)
    if (input$ppigraph_update == 'ab_ppi_subset') {
      req(input$visNetwork_selected_nodes)
      nodes <- input$visNetwork_selected_nodes
      rv_ppi_subgraph$nodes <- rv_ppi_subgraph$nodes %>% filter(id %in% nodes)
      rv_ppi_subgraph$edges <- rv_ppi_subgraph$edges %>% filter(from %in% nodes & to %in% nodes)
    } else if (input$ppigraph_update == 'ab_ppi_delete_nodes_subgraph') {
      req(input$visNetwork_selected_nodes_subgraph)
      nodes <- input$visNetwork_selected_nodes_subgraph
      rv_ppi_subgraph$nodes <- rv_ppi_subgraph$nodes %>% filter( ! id %in% nodes)
      rv_ppi_subgraph$edges <- rv_ppi_subgraph$edges %>% filter( ! (from %in% nodes | to %in% nodes))
    }
    
    ppi_data <- rv_ppi_subgraph$edges %>%
      left_join(rv_ppi_subgraph$nodes %>% select(id, from_symbol = label), by = c("from" = "id")) %>%
      left_join(rv_ppi_subgraph$nodes %>% select(id, to_symbol = label), by = c("to" = "id")) %>%
      select(from_symbol, to_symbol, combined_score, from, to)
    g <- get_ppigraph(ppi_data, vertex_clustering = vertex_attr(g, 'cluster'))
    genes_overview <- NULL
    if ( ! is.null(rv_genelists_overlap$gene_overview)) genes_overview <- rv_genelists_overlap$gene_overview
    vis <- get_visNetwork(g, genes_overview = genes_overview, sample_name = input$si_ppi_sample)
    
    rv_ppi_subgraph$p <- rv_ppi_subgraph$nodes$label
    rv_ppi_subgraph$g <- g
    rv_ppi_subgraph$nodes <- vis$nodes
    rv_ppi_subgraph$edges <- vis$edges
  })
  observeEvent(input$ab_ppi_reset_subgraph, {
    req(rv_ppi$g)
    rv_ppi_subgraph$nodes <- rv_ppi$nodes
    rv_ppi_subgraph$edges = rv_ppi$edges
    rv_ppi_subgraph$g = rv_ppi$g
    rv_ppi_subgraph$p = rv_ppi$p
  })
  observeEvent(input$visNetwork_selected_edges, {
    node_ids <- unlist(strsplit(input$visNetwork_selected_edges[length(input$visNetwork_selected_edges)], split = "_"))
    
    versions <- read.table(url("https://string-db.org/api/tsv-no-header/available_api_versions"))
    latest_version <- versions$V1[length(versions$V1)]
    version_major <- strsplit(input$si_ppi_version, '\\.')[[1]][1]
    version_minor <- as.numeric(gsub("([0-9]+).*$", "\\1", strsplit(input$si_ppi_version, '\\.')[[1]][2]))
    if (latest_version != paste0(version_major, ".", version_minor)) warning("Returning interaction of latest STRINGdb version as earlier versions are archived, may be that interaction is not found, recommend is to use latest STRINGdb version.")
    version_major <- strsplit(latest_version, '\\.')[[1]][1]
    version_minor <- as.numeric(gsub("([0-9]+).*$", "\\1", strsplit(latest_version, '\\.')[[1]][2]))
    
    url <- paste0("https://version-", version_major, "-", version_minor, ".string-db.org/interaction/", node_ids[1], "/", node_ids[2])
    browseURL(url)
  })
  
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
      shiny::showNotification("Highlighted nothing, keep last highlighted")
      rv_icheatmap$row_i <- integer(0)
      rv_icheatmap$col_i <- integer(0)
      rv_icheatmap$row_names <- character(0)
      rv_icheatmap$col_names <- character(0)
    } else {
      rv_icheatmap$row_i <- unique(unlist(df$row_index))
      rv_icheatmap$col_i <- unique(unlist(df$column_index))
      rv_icheatmap$row_names <- unique(unlist(df$row_label))
      rv_icheatmap$col_names <- unique(unlist(df$column_label))
      rv_icheatmap$text <- paste0("Genes highlighted: if genes by terms: ", paste(rv_icheatmap$row_names, collapse = ", "), ". If genes: ", paste(rv_icheatmap$col_names, collapse = ", "))
    }
  }
  shiny::observeEvent(input$ab_genefsi_icheatmap_plot, {
    rv_ppi$p <- unique(c(rv_genes$all_selected, process_string_input(input$tai_genefsi_add_genes)))
    
    shinyjs::runjs("$('#vto_genefsi_icheatmap').css('color', '#ff0000');")
    rv_genefsi_icheatmap$text <- "Make sure any genes are in global gene selection..."
    req(rv_ppi$p) # cannot be empty: character(0)

    shinyjs::show("ab_genefsi_icheatmap_plot_loader")
    
    data_genelists <- rv_genelists()
    
    ch <- plot_gene_effectsize_ComplexHeatmap(
      genes =  rv_ppi$p, 
      genes_overview = rv_genelists_overlap$gene_overview, 
      rows_dendrogram = input$ci_genefsi_icheatmap_dendrogram_rows, 
      cols_dendrogram = input$ci_genefsi_icheatmap_dendrogram_cols, 
      plot_n_genes = input$ni_genefsi_icheatmap_ngenes
    )
    rv_genefsi_icheatmap$heatmap <- ch

    InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(input, output, session, ht_list = ch, heatmap_id = "genefsi_icheatmap")

    rv_genefsi_icheatmap$success <- TRUE 
    shinyjs::runjs("$('#vto_genefsi_icheatmap').css('color', '#32CD32');")
    rv_genefsi_icheatmap$text <- "Successfully plotted InteractiveComplexHeatmap: click or drag cells for info and to select genes"
    shinyjs::hide("ab_genefsi_icheatmap_plot_loader")
  })
  
  shiny::observeEvent(c(input$ab_ppi_reset_protgenes, input$ab_icheatmap_reset_protgenes), {
    rv_genes$all_selected <- character(0)
  })
  shiny::observeEvent(input$ab_reset_upset_genes, {
    rv_genes$all_selected <- character(0)
    rv_genelists_overlap$text <- "Reset genes selection"
  })
  shiny::observeEvent(input$ab_reset_volcano_genes, {
    rv_genes$all_selected <- character(0)
    rv_volcano$text <- "Reset genes selection"
  })
  shiny::observeEvent(input$ab_icheatmap_select_genes, {
    req(rv_icheatmap$col_names)
    rv_genes$all_selected <- unique(c(rv_genes$all_selected, rv_icheatmap$col_names))
  })
  shiny::observeEvent(input$ab_icheatmap_select_terms, {
    req(rv_icheatmap$row_names)
    selected_terms_i <- which(rv_enrichment$current_enrichment$name %in% rv_icheatmap$row_names)
    selected_genes <- unique(unlist(rv_enrichment$current_enrichment$symbol[selected_terms_i]))
    rv_genes$all_selected <- unique(c(rv_genes$all_selected, selected_genes))
  })
  shiny::observeEvent(input$ab_icheatmap_select_terms_subgenes, {
    req(rv_icheatmap$row_names)
    selected_terms_i <- which(rv_enrichment$current_enrichment$name %in% rv_icheatmap$row_names)
    all_genes <- unique(unlist(rv_enrichment$current_enrichment$symbol[selected_terms_i]))
    selected_genes <- rv_icheatmap$col_names[rv_icheatmap$col_names %in% all_genes]
    rv_genes$all_selected <- unique(c(rv_genes$all_selected, selected_genes))
  })
  
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
        min_pvalue_adjust = input$ni_enrichment_filter_pvalue_adjust,
        max_ngenes = input$ni_enrichment_filter_ngenes_max,
        max_ngenes_input = input$ni_enrichment_filter_ngenes_input_max,
        max_ngenes_signif = input$ni_enrichment_filter_ngenes_signif_max,
        max_abs_zscore = input$ni_enrichment_filter_zscore_max,
        max_pvalue_adjust = input$ni_enrichment_filter_pvalue_adjust_max
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
  
  shiny::observeEvent(input$ab_select_upset_genes, {
    ids <- unlist(input$po_genelist_overlap_click$elems)
    req(ids)
    symbols <- plyr::mapvalues(
      x = ids,
      from = rv_genelists_overlap$gene_overview$gene,
      to = rv_genelists_overlap$gene_overview$symbol,
      warn_missing = FALSE)
    rv_genes$all_selected <- unique(c(rv_genes$all_selected, symbols))
    rv_genelists_overlap$text <- "Genes from Set added to gene selection"
  })
  shiny::observeEvent(input$ab_select_volcano_genes, {
    selected <- event_data("plotly_selected", source = 'V') # selected from EnhancedVolcano plot
    rv_genes$all_selected <- unique(c(rv_genes$all_selected, unique(selected$key)))
    rv_volcano$text <- "Genes from EnhancedVolcano added to gene selection"
    
    print(rv_genes$all_selected)
  })
  
  shiny::observeEvent(input$ab_plot_overlap_upset, {
    data <- rv_genelists()
    shinyjs::hide("db_overlap_plot")
    shinyjs::show("ab_plot_overlap_upset_loader")
    rv_genelists_overlap$plot <- plot_genelists_overlap_upsetjs(
      genelists = data,
      mode = input$si_plot_overlap_upset,
      interactive = TRUE,
      main.color = colors$text, 
      highlight.color = colors$focus
    )
    shinyjs::hide("ab_plot_overlap_upset_loader")
    shinyjs::show("db_overlap_plot")
  })
  
  shiny::observeEvent(input$ab_run_volcano, {
    data <- rv_genelists()
    shinyjs::hide("db_volcano_plot")
    shinyjs::show("ab_run_volcano_loader")
    rv_volcano$plot <- plot_EnhancedVolcano(
      genelist = data[[input$si_volcano_sample]],
      effectsize_threshold = input$ni_set_significant_effectsize,
      pvalue_threshold = input$ni_set_significant_pvalue,
      background_color = colors$main_bg,
      foreground_color = colors$text,
      interactive = TRUE
    )
    print(class(rv_volcano$plot))
    shinyjs::hide("ab_run_volcano_loader")
    shinyjs::show("db_volcano_plot")
  })
  
  shiny::observeEvent(input$ab_run_enrichment, {
    data_genelists <- rv_genelists()
    data_genesets <- rv_genesets$filtered_genesets
    
    rv_enrichment$success <- FALSE
    shinyjs::runjs("$('#vto_test_enrichment').css('color', '#ff0000');") # failure color
    shinyjs::show("ab_run_enrichment_loader")
    
    for (name in names(data_genelists)) {
      if (nrow(data_genelists[[name]]) > max(goat::goat_nulldistributions$N) & input$si_test_method == 'goat') {
        rv_enrichment$text <- paste0("'", name, "' genelist contains N genes: ", nrow(data_genelists[[name]]), ', when using method="goat" there have to be less then ', as.character(max(goat::goat_nulldistributions$N)), " genes, please choose another method or set 'keep max N genes' option in the initialize tab")
        break
      }
      
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
      
      rv_enrichment$success <- TRUE
    }
    if (rv_enrichment$success) {
      updateSelectInput(
        session,
        "si_show_enrichment",
        choices = names(data_genelists),
        selected = names(data_genelists)[1] 
      )
      updateSelectInput(
        session,
        "si_ppi_sample",
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
      
      shinyjs::runjs("$('#vto_test_enrichment').css('color', '#32CD32');") # success color
      rv_enrichment$text <- "Enrichment ran successfully"
    }
    shinyjs::hide("ab_run_enrichment_loader")
  })
  
  shiny::observeEvent(input$ab_set_significant_genes, {
    shinyjs::disable('ab_run_enrichment')
    shinyjs::disable('ab_run_volcano')
    data <- rv_genelists()

    for (name in names(data)) {
      genelist <- data[[name]]
      genelist <- set_significant_N_genes(
        genelist = genelist, 
        significance_by = input$si_set_significant_genes,
        pvalue_threshold = input$ni_set_significant_pvalue, 
        effectsize_threshold = input$ni_set_significant_effectsize,
        keep_max_n_genes = input$cbi_keep_maxN_genes,
        keep_max_n_genes_by = input$si_keep_maxN_genes
      )
      data[[name]] <- genelist
    }

    rv_genelists(data)
    rv_set_significant_genes$success <- TRUE
    shinyjs::runjs("$('#vto_set_significant_genes').css('color', '#32CD32');")
    
    ## run genelists overlap with UI parameters and global settings
    rv_genelists_overlap$success <- FALSE
    genelist_overlap_result <- run_genelists_overlap(genelists = rv_genelists())
    rv_genelists_overlap$gene_overview <- genelist_overlap_result
    rv_genelists_overlap$success <- TRUE
    
    rv_set_significant_genes$text <- "Set genes in genelists & created gene overview metadata used for some plotting"
    
    updateSelectInput(
      session,
      "si_volcano_sample",
      choices = names(data),
      selected = names(data)[1] 
    )
    shinyjs::enable('ab_run_volcano')
    shinyjs::runjs("$('#vto_filter_genesets').css('color', 'white');")
    rv_genesets$text_filter <- "Filter genesets after (re)setting significant genes"
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
  shiny::observeEvent(input$fi_load_genelists, {
    shinyjs::disable('ab_run_volcano')
    data <- rv_genelists()
    for (i in seq_len(nrow(input$fi_load_genelists))) {
      file <- input$fi_load_genelists[i, ]
      genelist <- read_validate_genelist(
        file = file[['datapath']], 
        remove_non_numerical_ids = input$cbi_remove_non_numerical_ids, 
        remove_duplicated = input$cbi_remove_duplicated,
        remove_Rik_genes = input$cbi_remove_Rik_genes,
        remove_Gm_genes = input$cbi_remove_Gm_genes)
      
      if (is.character(genelist)) {
        shinyjs::runjs("$('#vto_load_genelists').css('color', '#ff0000');")
        rv_load_genelists$text <- paste(genelist)
        rv_load_genelists$success <- FALSE
        break
      } else {
        ## annotate genes with small description
        if (input$cbi_annotate_genes) genelist$gene_annotation <- get_gene_annotation(gene_symbols = genelist$symbol, organism = input$si_organism)
        data[[file[['name']]]] <- genelist
        shinyjs::runjs("$('#vto_load_genelists').css('color', '#32CD32');")
        ## bytes to megabytes conversion
        rv_load_genelists$text <- paste(input$fi_load_genelists$name, paste0(round(as.numeric(input$fi_load_genelists$size) / 1024 / 1024, digits = 2), "Mb"))
        rv_load_genelists$success <- TRUE
      }
    }
    rv_genelists(data)
  })
  
  # load Gene Ontology Bioconductor genesets based on selected organism
  shiny::observeEvent(input$ab_load_GOB_genesets, {
    shinyjs::show("ab_load_GOB_genesets_loader")
    rv_genesets$success <- FALSE
    rv_genesets$genesets <- goat::load_genesets_go_bioconductor(taxid = as.numeric(input$si_organism))
    rv_genesets$success <- TRUE
    shinyjs::runjs("$('#vto_load_genesets').css('color', '#32CD32');")
    rv_genesets$text <- paste0("Successfully loaded: org.", input$si_organism, ".eg.db")
    shinyjs::hide("ab_load_GOB_genesets_loader")
  })
  shiny::observeEvent(input$fi_load_genesets_GMT, {
    shinyjs::show("ab_load_GOB_genesets_loader")
    rv_genesets$success <- FALSE
    rv_genesets$genesets <- goat::load_genesets_gmtfile(input$fi_load_genesets_GMT$datapath, label = input$ti_load_genesets_GMT)
    rv_genesets$success <- TRUE
    shinyjs::runjs("$('#vto_load_genesets').css('color', '#32CD32');")
    rv_genesets$text <- paste0("Successfully loaded: ", input$fi_load_genesets_GMT$name, " labelled ", input$ti_load_genesets_GMT)
    shinyjs::hide("ab_load_GOB_genesets_loader")
  })
  
  #### show modal dialogues ----
  observeEvent(input$ab_volcano_modal, {
    showModal(modalDialog(
      easyClose = TRUE, footer = NULL,
      title = "EnhancedVolcano plot",
      tags$h3("Plotting"),
      tags$p("Select the sample to plot, then click the top left button to plot, loading might take a bit longer than the loader icon shows."),
      tags$p("The user is recommended to deselect points that are not of interest, to speed up the interactivity."),
      tags$p("Hovering the mouse over the points shows their metadata"),
      tags$p("plotly provides interactive tools to zoom, pan, export and select elements of the plot in the top right corner of the plot."),
      tags$p("Use the plotly box/lasso selection tool to highlight genes, select them using the 'add gene to selection' button, a message will be displayed, these genes are now usable in plotting the PPI."),
    ))
  })
  observeEvent(input$ab_overlap_modal, {
    showModal(modalDialog(
      easyClose = TRUE, footer = NULL,
      title = "Genelist overlapping and plotting",
      tags$h3("Overlapping"),
      tags$p("Click 'run genelists overlap' to create a gene overview in the background."),
      tags$p("The gene overview can be downloaded by clicking the 'download genelists overlap' button."),
      tags$p("The gene overview is used to add metadata information about overlapping significant genes to the heatmap and gene annotations which can be visualized with the Protein Protein Interactions."),
      tags$hr(),
      tags$h3("Plotting"),
      tags$p("Set the overlap mode: 'intersect' to show overlapping genes in each intersection, 'distinct' (as in Venn diagrams) to show unique genes and 'union' for all genes"),
      tags$p("Click 'plot significant gene overlap' to view the interactive UpSetjs plot."),
      tags$p("Hover and click a set to select it, then press 'add set genes' to add the genes of the selected set to the total gene selection, to be used in plotting."),
      tags$p("The total gene selection can be reset by clicking the 'reset gene selection' button."),
      ))
  })
  observeEvent(input$ab_splitdot_modal, {
    showModal(modalDialog(
      easyClose = TRUE, footer = NULL,
      title = "Splitdot plot - post enrichment",
      tags$h3("Plotting"),
      tags$p("Put a number of terms and click to plot, when the plot is visible you can download by clicking the save button."),
    ))
  })
  observeEvent(input$ab_termtree_modal, {
    showModal(modalDialog(
      easyClose = TRUE, footer = NULL,
      title = "Termtree plot - post enrichment",
      tags$h3("Plotting"),
      tags$p("Put a number of terms/words/clusters and click to plot, when the plot is visible you can download by clicking the save button."),
    ))
  })
  observeEvent(input$ab_heatmap_modal, {
    showModal(modalDialog(
      easyClose = TRUE, footer = NULL,
      title = "Interactive heatmap - post enrichment",
      tags$h3("Plotting"),
      tags$p("Set the clustering method, number of clusters, topN terms/genes and click 'plot heatmap' to plot."),
      tags$p("A box can be drawn with the mouse in order to view a subset of the heatmap, which will be generated in a subgraph below the original heatmap."),
      tags$p("From the selection, genes can be selected."),
      tags$p("Click 'add visible genes' to add all selected genes shown on the top of the subheatmap to the genes selection."),
      tags$p("Click 'add all genes of visible terms' to add all genes of the terms shown on the subheatmap to the genes selection, even the ones that are in the full set on the background."),
      tags$p("Click 'add visible genes of vible terms' to add the shown genes that are within shown terms of the subheatmap to the total gene selection."),
      tags$p("Click 'reset selected genes' to reset the gene selection."),
    ))
  })
  observeEvent(input$ab_genefsi_heatmap_modal, {
    showModal(modalDialog(
      easyClose = TRUE, footer = NULL,
      title = "Interactive heatmap - post enrichment",
      tags$h3("Plotting"),
      tags$p("Make sure some genes are selected, or add them through typing additional genes."),
      tags$p("For speed and clarity, a max N genes out of the selection can be plotted."),
      tags$p("Genes and genelists can be clustered based on effectsize by opting for the dendrograms."),
      tags$p("Exporting the plot can be done via the interactive buttons of the plot itself."),
    ))
  })
  observeEvent(input$ab_global_modal, {
    showModal(modalDialog(
      easyClose = TRUE, footer = NULL,
      title = "Initialization",
      tags$h3("Global parameters"),
      tags$p("Selected organism is used for gene specific annotations and to load Gene Ontology AnnotationDbi gene sets."),
      tags$p("Selected output type is for all exported tables."),
      tags$p("Annotate genes adds a small gene description as metadata. It is shown in exporting tables and when hovering genes in the Protein-Protein Interaction graphs."),
      tags$hr(),
      tags$h3("Loading genelists"),
      tags$p("Browse to your file(s), use the parameters for loading differently."),
      tags$p("Remove Rik(en) and Gm (predicted) mouse genes could be potentially useful for mouse specifically."),
      tags$p("IDs are expected to be in Entrez (NCBI) format."),
      tags$hr(),
      tags$h3("Setting significant genes"),
      tags$p("Adds the $signif column to the data, used by the GOAT algorithm and for visualizations."),
      tags$p("If set to 'pvalue_effectsize' both are used for setting significance, if either is selected the other cutoff is ignored."),
      tags$p("Keep max N genes is useful as the standard goat method can take a maximal number of genes, as defined by the max(goat::null_distributions$N) command."),
      tags$p("If there is need be for more genes, use the goat_bootstrap method, a warning will be given if there are too many genes for the goat method."),
      tags$p("Also here, the pvalue or effectsize can be used to order the genelist before keeping the top genes."),
      tags$hr(),
      tags$h3("Set names"),
      tags$p("Often sample shortnames, useful for visual clarity in the plots."),
      tags$hr(),
      tags$h3("Download genelists overlap"),
      tags$p("After setting significant genes, a gene overview with potential overlap info for multiple samples is generated and used in the background."),
      tags$hr(),
      tags$h3("Loading genesets"),
      tags$p("Either organism specific Gene Ontology AnnotationDbi genesets can be downloaded via goat, or a .gmt file with genesets (e.g. from Molecular Signatures Database) can be provided."),
      tags$p("The loaded genesets will then be filtered by parameter stats and overlapped with the loaded genelists."),
      tags$p("Any time the loaded genelists are re-loaded, the genesets will have to be filtered again."),
    ))
  })
  observeEvent(input$ab_enrichment_modal, {
    showModal(modalDialog(
      easyClose = TRUE, footer = NULL,
      title = "Enrichment",
      tags$h3("Analysis"),
      tags$p("All implemented enrichment methods by goat can be used."),
      tags$p("Some parameters are goat specific, which can be read in its documentation online."),
      tags$p("Once the enrichment has run, all enrichments can be filtered by using the parameters."),
      tags$p("The currently shown enrichment will be used for plotting the gene-geneset heatmap."),
    ))
  })
  ## explain page and function as graph legend
  observeEvent(input$ab_ppi_modal, {
    showModal(modalDialog(
      easyClose = TRUE, footer = NULL,
      title = "PPIgraph: Protein-Protein Interaction igraph",
      tags$h3("Initialization"),
      tags$p("Initialize by clicking 'Create PPIgraph': UI to its right are used as parameters."),
      tags$p("Note that the sample selected here can be different than the sample used in the enrichment tab, and thereby heatmap selected genes!"),
      tags$p("Also note that STRING db PPI information is downloaded to a temporary folder on the first load each session, subsequent loads will likely be instant"),
      tags$p("Hover parameters for additional information."),
      tags$p("The text output below shows which proteins are selected or how to select proteins."),
      tags$p("Underneath the ppigraph will be shown. The current view can be exported by clicking 'Export ppigraph' in the bottom left. Original view can be reset with the button in the bottom right."),
      tags$p("Below the ppigraph are node and edge coloring UI. Edges simply select what to color by. For nodes, first select to color background/border, then select what feature to color by."),
      tags$p("When specific proteins are selected, click 'Subgraph selected nodes' to draw a ppisubgraph, which can be seen by scrolling down. This subgraph is meant to focus on specific proteins and their relations."),
      tags$p("Then, some ppigraph metrics are shown, which can be exported as a table with the 'Export ppigraph metrics' button."),
      tags$p("Finally, the ppisubgraph has some unique functionality. 'Delete selected nodes' to delete selected proteins and 'Reset to original ppigraph' to reset subgraph to main ppigraph."),
      tags$p(""),
      
      tags$hr(),
      tags$h3("Legend"),
      tags$p("Feature information is shown by the different colored nodes (proteins) and edges (interactions)."),
      tags$p("Depending on the feature, a colorscale from blue(-white)-red, decreasing to increasing respectively, is shown. Otherwise, discrete colors are drawn."),
      tags$hr()
    ))
  })
  
  #### download button handlers ----
  output$db_ppigraph_metrics <- shiny::downloadHandler(
    filename = function() {
      req(rv_ppi$g)
      "metrics.csv"
    },
    content = function(file) {
      if (input$rb_global_output_type == ".csv") {
        write.csv2(t(as.data.frame(igraph::graph.attributes(rv_ppi$g))), file = file)
      } else if (input$rb_global_output_type == ".xlsx") {
        openxlsx::write.xlsx(t(as.data.frame(igraph::graph.attributes(rv_ppi$g))), file = file)
      }
    },
    contentType = "text/csv/xlsx"
  )
  output$db_ppigraph_metrics_subgraph <- shiny::downloadHandler(
    filename = function() {
      req(rv_pp_subgraphi$g)
      "metrics_subgraph.csv"
    },
    content = function(file) {
      if (input$rb_global_output_type == ".csv") {
        write.csv2(t(as.data.frame(igraph::graph.attributes(rv_ppi_subgraph$g))), file = file)
      } else if (input$rb_global_output_type == ".xlsx") {
        openxlsx::write.xlsx(t(as.data.frame(igraph::graph.attributes(rv_ppi_subgraph$g))), file = file)
      }
    },
    contentType = "text/csv/xlsx"
  )
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
  output$db_volcano_download <- downloadHandler(
    filename = "volcano_plot.png",
    content = function(file) {
      req(rv_volcano$plot)
      data <- rv_genelists()
      p <- plot_EnhancedVolcano(
        genelist = data[[input$si_volcano_sample]],
        effectsize_threshold = input$ni_set_significant_effectsize,
        pvalue_threshold = input$ni_set_significant_pvalue,
        interactive = FALSE
      )
      ggplot2::ggsave(plot = p, filename = file)
    },
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
  output$db_run_genelist_overlap <- shiny::downloadHandler(
    filename = "genelist_overview.csv",
    content = function(file) {
      if(is.null(rv_genelists_overlap$gene_overview)) {
        rv_genelists_overlap$gene_overview <- run_genelists_overlap(genelists = rv_genelists())
      }
      req(rv_genelists_overlap$gene_overview)

      if (input$rb_global_output_type == ".csv") {
        write.csv2(rv_genelists_overlap$gene_overview, file = file)
      } else if (input$rb_global_output_type == ".xlsx") {
        openxlsx::write.xlsx(rv_genelists_overlap$gene_overview, file = file)
      }
    },
    contentType = "text/csv/xlsx"
  )
  
  #### render texts for (verbatim)TextOutputs ----
  output$vto_volcano <- shiny::renderText({paste(rv_volcano$text, collapse = "\n")})
  output$vto_load_genelists <- shiny::renderText({paste(rv_load_genelists$text, collapse = "\n")})
  output$vto_load_genesets <- shiny::renderText({rv_genesets$text})
  output$vto_filter_genesets <- shiny::renderText({rv_genesets$text_filter})
  output$vto_set_significant_genes <- shiny::renderText({rv_set_significant_genes$text})
  output$vto_set_names <- shiny::renderText({rv_set_names$text})
  output$vto_test_enrichment <- shiny::renderText({rv_enrichment$text})
  output$vto_genelist_overlap <- shiny::renderText({rv_genelists_overlap$text})
  output$vto_icheatmap <- shiny::renderText({rv_icheatmap$text})
  output$vto_genefsi_icheatmap <- shiny::renderText({rv_genefsi_icheatmap$text})
  
  output$to_upset_hovered <- renderText({input$po_genelist_overlap_hover$name})
  output$to_upset_clicked <- renderText({input$po_genelist_overlap_click$name})
  
  output$vto_ppi_selection <- renderText({
    if (is.null(input$visNetwork_selected_nodes) || length(input$visNetwork_selected_nodes) == 0) {
      if (is.null(rv_ppi$g)) return("Initialize graph: graph creation won't work without selecting any proteins/genes!")
      if (class(rv_ppi$g) == "igraph_constructor_spec") return("Empty graph, try adjusting parameters...")
      return("ppigraph: hover for info, click edge to browse STRINGdb interaction, (shift+clickdrag or cntrl+)click to (multi)select node(s)")
    } else paste("Selected Node IDs:", paste(vertex_attr(rv_ppi$g)$name[match(input$visNetwork_selected_nodes, vertex_attr(rv_ppi$g)$id)], collapse = ", "))
  })
  output$vto_ppi_selection_subgraph <- renderText({
    if (is.null(input$visNetwork_selected_nodes_subgraph) || length(input$visNetwork_selected_nodes_subgraph) == 0) {
      if (is.null(rv_ppi_subgraph$g)) return("Initialize graph")
      if (class(rv_ppi_subgraph$g) == "igraph_constructor_spec") return("Empty graph, initialize main graph.")
      return("subgraph: hover for info, click edge to browse STRINGdb interaction, (shift+clickdrag or cntrl+)click to (multi)select node(s)")
    } else paste("Selected Node IDs:", paste(vertex_attr(rv_ppi_subgraph$g)$name[match(input$visNetwork_selected_nodes_subgraph, vertex_attr(rv_ppi_subgraph$g)$id)], collapse = ", "))
  })
  output$vto_ppi_metrics <- renderText({
    if ( ! is.null(rv_ppi$g)) {
      shinyjs::runjs("$('#vto_ppi_metrics').css('color', '#ff0000');")
      if (class(rv_ppi$g) == "igraph_constructor_spec") return("Empty graph, try adjusting parameters...")
      attribute_names <- names(igraph::graph.attributes(rv_ppi$g))
      max_name_width <- max(nchar(attribute_names))
      shinyjs::runjs("$('#vto_ppi_metrics').css('color', '#32CD32');")
      ## format aligned attribute names with their values
      paste(sapply(attribute_names, function(attr) {
        value <- igraph::graph.attributes(rv_ppi$g)[[attr]]
        if (is.numeric(value)) value <- round(value, digits = 3)
        attr <- sprintf("%-*s", max_name_width, attr) # left-align
        attr <- gsub(" ", ".", attr)
        paste0(attr, " : ", value)
      }), collapse = "\n")
    } else "No graph attributes available"
  })
  observeEvent(rv_ppi_subgraph$g, {
    req(rv_ppi_subgraph$g)
    shinyjs::runjs("$('#vto_ppi_metrics_subgraph').css('color', '#ff0000');")
    output$vto_ppi_metrics_subgraph <- renderText({
      "No graph attributes available"
      if (class(rv_ppi_subgraph$g) == "igraph_constructor_spec") return("Empty graph, try adjusting parameters...")
      attribute_names <- names(igraph::graph.attributes(rv_ppi_subgraph$g))
      max_name_width <- max(nchar(attribute_names))
      shinyjs::runjs("$('#vto_ppi_metrics_subgraph').css('color', '#32CD32');")
      ## format aligned attribute names with their values
      paste(sapply(attribute_names, function(attr) {
        value <- igraph::graph.attributes(rv_ppi_subgraph$g)[[attr]]
        if (is.numeric(value)) value <- round(value, digits = 3)
        attr <- sprintf("%-*s", max_name_width, attr) # left-align
        attr <- gsub(" ", ".", attr)
        paste0(attr, " : ", value)
      }), collapse = "\n")
    })
  })
  
  #### render plots for plotOutputs ----
  output$po_volcano_plot <- plotly::renderPlotly({
    req(rv_volcano$plot)
    rv_volcano$plot
  })
  output$po_genelist_overlap <- upsetjs::renderUpsetjs({
    req(rv_genelists_overlap$plot)
    rv_genelists_overlap$plot
  })
  output$po_splitdot <- shiny::renderPlot({
    if ( ! is.null(rv_splitdot$plot)) print(ggplot2::ggplot_build(rv_splitdot$plot))
    shiny::showNotification("NOTE: plot size is draggable from edges")
  })
  output$po_termtree <- shiny::renderPlot({
    if ( ! is.null(rv_termtree$plot)) print(ggplot2::ggplot_build(rv_termtree$plot))
    shiny::showNotification("NOTE: plot size is draggable from edges")
  })
  
  #### render ppigraphs as visnetwork ----
  output$vno_ppi_visnetwork <- visNetwork::renderVisNetwork({
    if ( ! is.null(rv_ppi$g) && class(rv_ppi$g) != "igraph_constructor_spec") {
      visNetwork::visNetwork(rv_ppi$nodes, rv_ppi$edges, width = "100%", height = "100%") %>%
        visNetwork::visOptions(highlightNearest = TRUE) %>%
        visNetwork::visEdges(smooth = FALSE,
                 color = list(
                   color = colors$text,
                   highlight = colors$focus,
                   hover = colors$focus)) %>%
        visNetwork::visNodes(shape = 'box',
                 borderWidth = 5,
                 color = list(
                   background = rv_ppi$nodes$color.background,
                   highlight = list(
                     background = colors$darker_bg,
                     border = colors$text
                   ),
                   hover = list(
                     background = colors$darker_bg,
                     border = colors$text
                   ),
                   border = rv_ppi$nodes$color.border),
                 font = list(
                   color = colors$text,
                   background = rv_ppi$nodes$color.background)) %>%
        visNetwork::visPhysics(stabilization = FALSE) %>%
        visNetwork::visInteraction(
          navigationButtons = TRUE,
          multiselect = TRUE,
          selectConnectedEdges = FALSE,
          hover = TRUE,
          tooltipDelay = 200,
          tooltipStay = 200) %>%
        visNetwork::visIgraphLayout(
          layout = input$si_ppi_layout,
          type = 'full',
          randomSeed = 42
        ) %>% visNetwork::visExport(
          type = "png", 
          name = "ppigraph", 
          float = "left", 
          label = "Export ppigraph") %>%
        visNetwork::visEvents(
          selectNode = "function(params) {
          Shiny.setInputValue('visNetwork_selected_nodes', params.nodes, {priority: 'event'});
        }",
          deselectNode = "function(params) {
          Shiny.setInputValue('visNetwork_selected_nodes', params.nodes, {priority: 'event'});
        }",
          selectEdge = "function(params) {
        if (params.edges.length > 0) {
          Shiny.setInputValue('visNetwork_selected_edges', params.edges, {priority: 'event'});
        }}"
        )
    }
  })
  output$vno_ppi_visnetwork_subgraph <- visNetwork::renderVisNetwork({
    if ( ! is.null(rv_ppi_subgraph$g) && class(rv_ppi_subgraph$g) != "igraph_constructor_spec") {
      visNetwork::visNetwork(rv_ppi_subgraph$nodes, rv_ppi_subgraph$edges, width = "100%", height = "100%") %>%
        visNetwork::visOptions(highlightNearest = TRUE) %>%
        visNetwork::visEdges(smooth = FALSE,
                 color = list(
                   color = colors$text,
                   highlight = colors$focus,
                   hover = colors$focus)) %>%
        visNetwork::visNodes(shape = 'box',
                 borderWidth = 5,
                 color = list(
                   background = rv_ppi_subgraph$nodes$color.background,
                   highlight = list(
                     background = colors$darker_bg,
                     border = colors$text
                   ),
                   hover = list(
                     background = colors$darker_bg,
                     border = colors$text
                   ),
                   border = rv_ppi_subgraph$nodes$color.border),
                 font = list(
                   color = colors$text,
                   background = rv_ppi_subgraph$nodes$color.background)) %>%
        visNetwork::visPhysics(stabilization = FALSE) %>%
        visNetwork::visInteraction(
          navigationButtons = TRUE,
          multiselect = TRUE,
          selectConnectedEdges = FALSE,
          hover = TRUE,
          tooltipDelay = 200,
          tooltipStay = 200) %>%
        visNetwork::visIgraphLayout(
          layout = input$si_ppi_layout_subgraph,
          type = 'full',
          randomSeed = 42
        ) %>%
        visNetwork::visExport(
          type = "png",
          name = "ppisubgraph",
          float = "left",
          label = "Export ppigraph") %>%
        visNetwork::visEvents(
          selectNode = "function(params) {
          Shiny.setInputValue('visNetwork_selected_nodes_subgraph', params.nodes, {priority: 'event'});
        }",
          deselectNode = "function(params) {
          Shiny.setInputValue('visNetwork_selected_nodes_subgraph', params.nodes, {priority: 'event'});
        }",
          selectEdge = "function(params) {
        if (params.edges.length > 0) {
          Shiny.setInputValue('visNetwork_selected_edges_subgraph', params.edges, {priority: 'event'});
        }}"
        )
    }
  })
  
  #### GO TO pathing buttons ----
  shiny::observeEvent(c(input$ab_go_to_enrichment, input$ab_go_to_enrichment_from_overlap, input$ab_go_to_enrichment_from_volcano), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_run_enrichment")
  })
  shiny::observeEvent(c(input$ab_go_to_overlap, input$ab_go_to_overlap_from_volcano), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_run_genelist_overlap")
  })
  shiny::observeEvent(c(input$ab_go_to_volcano, input$ab_go_to_volcano_from_overlap), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_run_volcano")
  })
  shiny::observeEvent(c(input$ab_go_to_heatmap, input$ab_go_to_heatmap_PPI, input$ab_go_to_heatmap_splitdot, input$ab_go_to_heatmap_termtree, input$ab_go_to_heatmap_genefsi_icheatmap), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_plot_heatmap")
  })
  shiny::observeEvent(c(input$ab_go_to_PPI, input$ab_go_to_PPI_icheatmap, input$ab_go_to_PPI_splitdot, input$ab_go_to_PPI_termtree, input$ab_go_to_PPI_genefsi_icheatmap), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_plot_PPI")
  })
  shiny::observeEvent(c(input$ab_go_to_splitdot, input$ab_go_to_splitdot_PPI, input$ab_go_to_splitdot_icheatmap, input$ab_go_to_splitdot_termtree, input$ab_go_to_splitdot_genefsi_icheatmap), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_plot_splitdot")
  })
  shiny::observeEvent(c(input$ab_go_to_termtree, input$ab_go_to_termtree_PPI, input$ab_go_to_termtree_icheatmap, input$ab_go_to_termtree_splitdot, input$ab_go_to_termtree_genefsi_icheatmap), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_plot_termtree")
  })
  shiny::observeEvent(c(input$ab_go_to_genefsi_icheatmap, input$ab_go_to_genefsi_icheatmap_PPI, input$ab_go_to_genefsi_icheatmap_icheatmap, input$ab_go_to_genefsi_icheatmap_splitdot, input$ab_go_to_genefsi_icheatmap_termtree), ignoreInit = TRUE, {
    shinydashboard::updateTabItems(session, "menu_tabs", "menu_plot_genefsi_heatmap")
  })
}