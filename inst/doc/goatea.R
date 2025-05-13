## ----options, include = FALSE, echo = FALSE, results = 'asis'-----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
BiocStyle::markdown()

## ----goatea Shiny, eval=FALSE, message=FALSE, warning=FALSE-------------------
# library(goatea)
# 
# ## customizable coloring
# css_colors <- list(
#   main_bg = "#222222",
#   darker_bg = "#111111",
#   focus = "#32CD32",
#   hover = "#228B22",
#   border = "#555555",
#   text = "#FFFFFF"
# )
# 
# ## run the goatea Shiny application
# shiny::shinyApp(
#   ui = goatea:::goatea_ui,
#   server = function(input, output, session) {
#     goatea:::goatea_server(
#       input, output, session,
#       css_colors = css_colors)
#   }
# )

## ----installation, eval = FALSE-----------------------------------------------
# ## GOATEA installation requires the latest version of R and Rtools
# ## Rtools is needed for package compilation, to download and install visit:
# # R: https://cran.r-project.org/mirrors.html
# # Rtools: https://cran.r-project.org/bin/windows/Rtools/
# 
# # For the GOATEA development version use:
# if ( ! require("pak", quietly = TRUE)) install.packages('pak')
# pak::pkg_install('mauritsunkel/goatea', dependencies = TRUE, upgrade = TRUE)
# 
# ## When GOATEA is on Bioconductor use:
# # pak::pkg_install('goatea', dependencies = TRUE, upgrade = TRUE)
# 
# ## goatea organism (taxid) genome wide annotation packages (org.Xx.eg.dg)
# ## goatea requires at least one of the following available organism packages:
# # Human (9606)--------: org.Hs.eg.db
# # Mouse (10090)-------: org.Mm.eg.db
# # Fruit Fly (7227)----: org.Dm.eg.db
# # Rhesus monkey (9544): org.Mmu.eg.db
# # Rat (10116)---------: org.Rn.eg.db
# # Worm (6239)---------: org.Ce.eg.db
# # Chimpanzee (9598)---: org.Pt.eg.db
# # Zebrafish (7955)----: org.Dr.eg.db
# if ( ! require("pak", quietly = TRUE)) install.packages('pak')
# pak::pkg_install(c(
#   "org.Hs.eg.db",
#   "org.Mm.eg.db",
#   "org.Dm.eg.db",
#   "org.Mmu.eg.db",
#   "org.Rn.eg.db",
#   "org.Ce.eg.db",
#   "org.Pt.eg.db",
#   "org.Dr.eg.db"
# ))
# 
# ## Optional: add gene descriptions to exported tables, install annotables:
# # available only for: human, mouse, rat, worm, fruit fly, Rhesus Monkey
# if ( ! require("pak", quietly = TRUE)) install.packages('pak')
# pak::pkg_install('stephenturner/remotes')

## ----library, message=FALSE, warning=FALSE------------------------------------
# load goatea package library
library(goatea)

## ----initialization-----------------------------------------------------------
# set chosen organism taxid (for genome wide annotation package: org.Hs.eg.db)
taxid <- 9606 # Human (Homo Sapiens)

# set and create output folder
outdir <- tempdir()
dir.create(outdir, recursive = TRUE)

# set significance thresholds
p_value_threshold <- 0.05
absolute_effectsize_threshold <- 1

## ----loading genelists--------------------------------------------------------
# GOAT paper data: download to a specific folder, tempdir() by default
data <- goat::download_goat_manuscript_data(output_dir = outdir)
# We select datasets from Colameo 2021 for our automated workflow.
# Thi is RNA-seq with matched mass-spectrometry data. 
# Rat data is mapped to HUman NCBI Entrez gene identifies with the goat package.
genelists <- list(
  rna = data$`Colameo 2021:RNA-seq:PMID34396684`,
  ms = data$`Colameo 2021:mass-spec:PMID34396684`
)
head(genelists[['rna']])

## ----loading genelists: user data---------------------------------------------
# set the filepath to your own data files 
file <- system.file('extdata','example_genelist.csv', package = 'goatea')
# Check file formatting, see function documentation for additional parameters 
genelist <- goatea::read_validate_genelist(file = file)
# genelists <- list(
  # csv = genelist
# )
head(genelist)

## ----loading genesets---------------------------------------------------------
genesets <- goat::load_genesets_go_bioconductor(taxid = taxid)

## optionally: save and load from .rda to speed up after downloading
# save(genesets, file = file.path(outdir, 'genesets.rda'))
# load(file.path(outdir, 'genesets.rda')) # loading .rda to variable: genesets

## alternatively: load via .gmt file (downloaded from e.g.: MSigDB)
# then use: goat::load_genesets_gmtfile()

head(genesets)

## ----enrichment analysis------------------------------------------------------
filtered_genesets_list <- list()
enrichment_results <- list()
for (name in names(genelists)) {
  genelist <- genelists[[name]]
  ## set signif column by given p-value- and effectsize significance thresholds
  genelist$signif <- genelist$pvalue <= p_value_threshold & 
                     abs(genelist$effectsize) >= absolute_effectsize_threshold
  # order genelist by effectsize 
  genelist <- genelist[order(abs(genelist$effectsize), decreasing = TRUE),]
  ## keep max n genes for regular goat method
  # optionally: remove this line and use goat_bootstrap method 
  if (nrow(genelist) > max(goat::goat_nulldistributions$N)) {
    genelist <- genelist[1:max(goat::goat_nulldistributions$N),]
  }
  # update genelist
  genelists[[name]] <- genelist
  
  ## filter genesets 
  filtered_genesets <- goat::filter_genesets(
    genesets = genesets,
    genelist = genelist,
    min_overlap = 10L,
    max_overlap = NA,
    max_overlap_fraction = 0.5,
    min_signif = NA,
    max_size = NA,
    dedupe = FALSE
  )
  filtered_genesets_list[[name]] <- filtered_genesets

  ## run enrichment 
  enrichment_results[[name]] <- goatea::run_geneset_enrichment(
    genesets = filtered_genesets,
    genelist = genelist,
    method = "goat",
    score_type = "effectsize",
    padj_method = "BH",
    padj_sources = TRUE,
    padj_cutoff = p_value_threshold,
    padj_min_signifgenes = 0L
  )
}
head(enrichment_results[[1]])

## ----genes overview-----------------------------------------------------------
genes_overview <- goatea::run_genelists_overlap(genelists)
genes_overview <- goatea::calculate_geneSetRatio(
  enrichment_results, 
  genes_overview
)
head(genes_overview)

## ----searching and filtering--------------------------------------------------
filtered_enrichment <- goatea::filter_enrichment(
  df = enrichment_results[[1]],
  terms_query = c("synapse", "synaptic"), 
  terms_antiquery = c("asymmetric"),
  min_ngenes = 100, 
  min_ngenes_signif = 10
)
head(filtered_enrichment$name)

## ----selecing genes-----------------------------------------------------------
selected_genes <- filtered_enrichment$genes_signif[[1]]

## ----genelist volcano---------------------------------------------------------
goatea::plot_EnhancedVolcano(
  genelist = genelists[[1]],
  effectsize_threshold = absolute_effectsize_threshold,
  pvalue_threshold = p_value_threshold,
  background_color = 'white',
  foreground_color = 'black'
)

## ----genelist overlap---------------------------------------------------------
if (length(genelists) > 2) {
  goatea::plot_genelists_overlap_upsetjs(
    genelists = genelists, 
    mode = 'distinct'
  )
}

## ----tree plot----------------------------------------------------------------
# filter to keep a single geneset source
single_source_enrichment <- enrichment_results[[1]] %>% 
  dplyr::filter(source == enrichment_results[[1]]$source[1])
goatea::plot_termtree(
  single_source_enrichment, 
  Nterms = 20, 
  Nwords = 5, 
  Nclusters = 3
)

## ----term-split dotplot-------------------------------------------------------
# filter to keep a single geneset source
single_source_enrichment <- enrichment_results[[1]] %>% 
  dplyr::filter(source == enrichment_results[[1]]$source[1])
goatea::plot_splitdot(single_source_enrichment, topN = 20)

## ----gene-genelist heatmap----------------------------------------------------
selected_symbols <- filtered_enrichment$symbol[[1]][
  filtered_enrichment$genes[[1]] %in% filtered_enrichment$genes_signif[[1]]
][1:15]
p <- goatea::plot_gene_effectsize_ComplexHeatmap(
  genes = selected_symbols, 
  genes_overview = genes_overview
)

## ----gene-term heatmap--------------------------------------------------------
p <- goatea::plot_ComplexHeatmap(
  enrichment_result = enrichment_results[[1]],
  genelist = genelists[[1]],
  n_cluster = 5,
  n_top_terms = 20,
  n_top_genes = 50,
  genelist_overlap = genes_overview
)

## ----ppi network--------------------------------------------------------------
# selecting 10 symbols of the searchad and significant first enriched term
selected_symbols <- filtered_enrichment$symbol[[1]][
  filtered_enrichment$genes[[1]] %in% filtered_enrichment$genes_signif[[1]]
][1:10]

## this will download the STRING database protein-protein interaction data 
## to your selected folder 
# downloading should only take a couple of seconds
# downlaod happens only if the files are not found in the specified folder
# if the download does not work, double check if the STRING database is online
ppi_data <- goatea::get_string_ppi(
  aliases = selected_symbols, 
  organism = taxid, 
  folder = file.path(outdir)
)
# creating igraph in order to do network statistics
ppi_graph <- goatea::get_ppigraph(ppi_data = ppi_data)
# converting igraph to visnetwork for (Shiny app) interactive visualization
vis_network <- goatea::get_visNetwork(
  ppigraph = ppi_graph, 
  genes_overview = genes_overview
)
## showing a basic version of the visNetwork
## the goatea Shiny app version has full interactive capability 
visNetwork::visNetwork(
  vis_network$nodes, 
  vis_network$edges, 
  width = "100%", 
  height = "100%") %>% 
  visNetwork::visPhysics(stabilization = FALSE)

## ----session info-------------------------------------------------------------
sessionInfo()

