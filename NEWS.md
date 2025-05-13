# goatea (development version)

* polishing by CRAN standards for publishing
* created NEWS.md from changelog

# v0.2.5

* renamed package to lowercase goatea
* fixed dplyr %>% pipelines with .data$undefined_var_names
* fixed subgraphing/filtering by highlighting clusters/features
  * noted that highlighted but isolated (disconnected) nodes are not graphed
  * noted that clustering of the selected proteins is kept, unless if the proteins selected all belong to the same cluster
* fixed object g not found error by getting it from reactive value
* added save to R base folder and load loaded_taxid_genesets.Rdata functionality 
* added showNotification for ppi subgraph and subheatmaps
* fixed alignment of signif and signif_overlap in gene*geneset heatmap 
* if GO_BP in names, default for 'Show enrichment source' 
* transforming toupper(genelist$symbol) when mapping in order to format mapIds ALIAS
	* warning on %mapping not converted to ENTREZIDs 
* noted to download org.Xx.eg.db manually!
* fixed reloading data by resetting earlier loaded data, requiring to load all wanted data in one upload
* set contentType to NULL on downloadHandlers to use global format parameter for tables to auto use .csv or .xlsx
* kept only explicit ::fromList() calls instead of importFrom UpSetR or upsetjs to remove message for BiocCheck
* added showNotifications when gene selection buttons clicked
	
# v0.2.0

* added priority of selected genes to gene*set heatmap
* fixed setting names to updateSelectInputs and updating names in gene_overview
* set all :: and/or importFrom for R CMD check/BiocChecks
* for gene*effectsize heatmap: fixed setting as many genes in annotation as in matrix 
	
# v0.1.19

* visnetwork ppigraphs hover tooltip background and labels colored via JavaScript such that it is readable (www/visnetwork_hover_tooltip_style.js)
* genes*genesets heatmap: showing top genes based on ordering by highest ncount in terms and then highest effectsize
* fixed interactive overlap: upsetjs::chartTheme()
* added max gene filters to gene*geneset heatmap
* ppi
	* fixed hover node labels with all info: added toupper() to match gene and protein symbols
	* fixed coloring by L2FC: added mutate(toupper()) to match gene and protein symbols
	* enabled entering gene/cluster to highlight or center on 
	* using set_base_folder() for PPI STRINGdb downloads
	* added functionality to subgraph/filter/delete highlighted nodes/edges by numeric feature
* added modal dialogues per tab
* set_significant_N_genes: changed code order to not display warning when keeping max N genes
	
# v0.1.18

* ppi subgraph: keep original graph clustering
* added download button for vanilla EnhancedVolcano plot
* Set and calculate gene_overview when setting significant genes
	* needed for ppi metadata coloring
	* moved download gene overview metadata button to 'load data' tab below set names
* added interactive gene*L2FC heatmap figure for all loaded samples and user selected genes
	
# v0.1.17

* set term n=50 and gene n=100 defaults for heatmap
* kevinblighe/EnhancedVolcano answered issues 77/114 and Biostars 421750 by making minimal reproducible example of shiny + EnhancedVolcano + plotly for other users
* added interactive EnhancedVolcano plot via ploty::ggplotly()

# v0.1.16 

* added interactive UpSet plot (removed Venn completely)
* added overlap gene selection

# v0.1.15

* started changelog
* added to load from GMT file: goat::load_genesets_gmtfile(), label = "GMT")
	* checked that genesets cannot be loaded without setting 'source' column, for downstream processing
* fixed run_geneset_enrichment to actually use the given parameters
* added set_significant_N_genes() function

## Future steps
* sort heatmap by zscore (up/down regulation)
* genefsi_icheatmap_action functionalize for selecting/removing/resetting genes (selection)
* plot: similarity_heatmap(genesets/termsets) (see R/testing/_plot_similarity_heatmap.R)
* volcano user label axes
* interactive termtree
* interactive splitdot
* selected genes tab
* selected terms tab (if used for plotting)
* select genes by frequency of them popping up in terms 
* add genes to selection via input file (& custom genes selection tab?)
* ppigraph: add terms as different node shape, edges to those could be dashes to differentiate
* network graph
	* terms overlap by ngenes graph (connection shows overlap size, node size = ngenes)
		* may be able to add genes to this graph? 
* make menu_tab for colourpicker::colourInput to have user defined colors for plots
* heatmap: significant genes, saturated (alpha) hue, otherwise less saturated
* check heatmap y axis label lengths (going out of plot ?)