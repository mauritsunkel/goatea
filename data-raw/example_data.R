dir.create("inst/data/", recursive = TRUE, showWarnings = FALSE)

example_genelist <- data.frame(
  symbol = paste0("gene_", seq_len(100)),
  gene = sample(seq.int(10000, 20000), 100),
  pvalue = c(runif(50, 0, 0.05), runif(50, 0.05, 1))[sample(100)],
  effectsize = sample(seq(-5, 5, 0.1), 100, replace = TRUE)
)
example_genelist <- set_significant_N_genes(example_genelist)
example_genelist <- example_genelist[order(abs(example_genelist$effectsize), decreasing = TRUE),]
usethis::use_data(example_genelist, overwrite = TRUE)
save(example_genelist, file = "inst/extdata/example_genelist.rda")
write.csv2(example_genelist, file = "inst/extdata/example_genelist.csv", row.names = FALSE) ## for read_validate_genelist() @examples code 

example_genesets <- tibble::tibble(
  source = rep("origin", 10),
  source_version = rep("org.Xx.eg.db", 10),
  id = paste0("DB.", formatC(seq.int(1, 10), width = 3, flag = "0")),
  name = paste("geneset name", seq.int(1, 10)),
  parent_id = rep(list("DB:010"), 10),
  genes = lapply(seq.int(1, 10), function(x) sample(example_genelist$gene, size = sample(seq.int(10, 20), 1))),
  ngenes = sapply(genes, length)
)
example_genesets <- goat::filter_genesets(example_genesets, example_genelist)
usethis::use_data(example_genesets, overwrite = TRUE)
save(example_genesets, file = "inst/extdata/example_genesets.rda")

example_enrichment <- goatea::run_geneset_enrichment(example_genesets, example_genelist)
usethis::use_data(example_enrichment, overwrite = TRUE)
save(example_enrichment, file = "inst/extdata/example_enrichment.rda")

example_genes_overview <- goatea::run_genelists_overlap(list(A = example_genelist, B = example_genelist))
example_genes_overview <- goatea::calculate_geneSetRatio(list(A = example_enrichment, B = example_enrichment), example_genes_overview)
usethis::use_data(example_genes_overview, overwrite = TRUE)
save(example_genes_overview, file = "inst/extdata/example_genes_overview.rda")

example_ppi_data <- goatea::get_string_ppi(c("TP53", "EGFR", "BRCA1", "MTOR", "MYC", "SOX2"))
usethis::use_data(example_ppi_data, overwrite = TRUE)
save(example_ppi_data, file = "inst/extdata/example_ppi_data.rda")

## set to R/sysdata.rda - have internally available datasets
usethis::use_data(
  example_genelist, 
  example_genesets, 
  example_enrichment, 
  example_genes_overview,
  example_ppi_data,
  internal = TRUE, overwrite = TRUE
)
