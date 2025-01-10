#' human/mouse gene (NCBI entrez ID) annotations from the GO database using the 'org.Hs.eg.db'/'org.Mm.eg.db' Bioconductor packages
#'
#' @description
#' Download and import genesets from the GO database using the Bioconductor infrastructure.
#' Use the `goat::load_genesets_go_fromfile` function for more fine-grained control over the GO database version that you use; it allows you to import NCBI gene2go files
#'
#' @details
#' Note that only org.Hs.eg.db pulls data semi-annually from NCBI gene2go,
#' but the GO database version returned by this function is tied to the version of the org.Hs.eg.db on your computer (this is controlled by the Bioconductor infrastructure).
#'
#' The actual GO database version that is retrieved is returned by this function in the `source_version` column.
#' @param include_child_annotations boolean; include annotations against child terms? In most situations, TRUE (default) is the desired setting
#' @param organism string: default "Hs" for human (Homo Sapiens), or "Mm" for mouse (Mus Musculus)
#' @return table with columns; source (character), source_version (character), id (character), name (character), genes (list), ngenes (int)
#' @export
load_genesets_go_bioconductor = function(include_child_annotations = TRUE, organism = "Hs") {
  if(organism == "Hs") {
    org.xx.eg.db <- org.Hs.eg.db::org.Hs.eg.db
  } else if (organism == "Mm") {
    org.xx.eg.db <- org.Mm.eg.db::org.Mm.eg.db
  }
  genes = go_id = GOID = TERM = ONTOLOGY = go_domain = source_version = go_name = ngenes = parent_id = child_id = relation = name = NULL # fix invisible bindings R package NOTE
  check_dependency("AnnotationDbi", "loading GO data via Bioconductor")
  check_dependency("GO.db", "loading GO data via Bioconductor")
  check_dependency(paste0("org.", organism, ".eg.db"), "loading GO data via Bioconductor")
  
  ### Bioconductor GO terms and respective annotations (Entrez gene format)
  
  # using Bioconductor package 'GO.db', get a table of GO ID, name, ontology (CC/BP/MF)
  go_terms = suppressMessages(AnnotationDbi::select(GO.db::GO.db, keys = AnnotationDbi::keys(GO.db::GO.db), columns = c("TERM","ONTOLOGY"), keytype = "GOID", multiVals = "first"))
  # using Bioconductor package 'org.Hs.eg.db', get a list of GO ID with values entrez ID
  keys = setdiff(unique(go_terms$GOID), c("GO:0003674", "GO:0008150", "GO:0005575")) # exclude top-level ontologies like "molecular function" and CC/BP counterparts (basically entire realm)
  # bugfix; previously we used `AnnotationDbi::keys(org.Hs.eg.db, "GO")` to extract all unique GO term IDs but this seems to be bugged; it leaves out many legit terms (e.g. ribosomal subunit GO:0044391) that do have annotations in org.Hs.eg.db !
  # extract annotations
  go_annotations_entrez = suppressMessages(AnnotationDbi::mapIds(org.xx.eg.db, keys = keys, column = "ENTREZID", keytype = ifelse(include_child_annotations, "GOALL", "GO"), multiVals = "list"))
  # GO DB version
  go_annotations_metadata = AnnotationDbi::metadata(org.xx.eg.db)
  go_annotations_metadata = paste(go_annotations_metadata$value[match(c("GOSOURCENAME", "ORGANISM", "GOSOURCEDATE"), go_annotations_metadata$name)], collapse = " - ")
  
  
  ### convert Bioconductor data into a table compatible with this R package
  
  result = tibble::tibble(go_id = rep(names(go_annotations_entrez), lengths(go_annotations_entrez)),
                          genes = unlist(go_annotations_entrez, recursive = FALSE, use.names = FALSE)) |>
    # enforce entrez gene IDs to be integers by stripping non-numeric parts
    # (not strictly needed atm, just a safeguard against future upstream changes, e.g. prefixing entrez IDs with 'entrez:')
    mutate(genes = gsub("\\D+","", genes)) |>
    filter(genes != "") |>
    mutate(genes = as.integer(genes)) |>
    # remove duplicate goterm*genes entries, if any
    distinct(go_id, genes, .keep_all = TRUE) |>
    # back to list format
    tidyr::chop(cols = genes) |>
    # add goterm metadata; name and domain
    left_join(tibble::as_tibble(go_terms) |> select(go_id = GOID, go_name = TERM, go_domain = ONTOLOGY), by = "go_id") |>
    # package-specific metadata we'll use downstream
    mutate(
      source = paste0("GO_", go_domain),
      source_version = go_annotations_metadata,
      ngenes = lengths(genes)
    ) |>
    rename(id = go_id, name = go_name)
  
  
  ### ontology DAG
  
  extract_links = function(GODBLINKS, relation_accept) {
    # extract direct parents
    GOdata = as.list(GODBLINKS)
    # unlist the named vector per GO term into a long-format table
    links = dplyr::bind_rows(sapply(names(GOdata), function(n)
      data.frame(child_id=n, parent_id=unname(GOdata[[n]]), relation=names(GOdata[[n]]), row.names = NULL), simplify = FALSE, USE.NAMES = FALSE))
    links |>
      # remove unsupported relation types
      filter(relation %in% relation_accept) |>
      # retain only unique parent/child links, discarding relation types
      distinct(parent_id, child_id) |>
      tibble::as_tibble()
  }
  
  # accepted relation types (as specified in GO.obo) + variations that the Bioconductor GO.db might use (e.g. "isa" or "part of")
  relation_accept = c("is_a","part_of", "regulates", "positively_regulates", "negatively_regulates")
  relation_accept = unique(c(relation_accept, sub("_", "", relation_accept), sub("_", " ", relation_accept)))
  # links from GO.db::GOCCPARENTS are supposed to be within-GO-domain (e.g. no links from BP to CC)
  links_cc = extract_links(GO.db::GOCCPARENTS, relation_accept)
  links_bp = extract_links(GO.db::GOBPPARENTS, relation_accept)
  links_mf = extract_links(GO.db::GOMFPARENTS, relation_accept)
  
  
  ### compose final result
  
  result = result |>
    left_join(
      bind_rows(links_cc, links_bp, links_mf) |>  tidyr::chop(cols = parent_id),
      by = c("id"="child_id")
    ) |>
    # column ordering and renaming
    select(source, source_version, id, name, parent_id, genes, ngenes)
  
  attr(result, "settings") <- sprintf("load_genesets_go_bioconductor(include_child_annotations=%s) = '%s'",
                                      include_child_annotations, go_annotations_metadata)
  
  message(paste("load_genesets_go_bioconductor(): data version =", go_annotations_metadata))
  return(result)
}
