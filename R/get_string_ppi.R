#' Get STRING database Protein-Protein Interactions
#'
#' @param aliases character, vector with protein/gene symbols/aliases
#' @param score_threshold integer, default: 0, to get all PPI, ranges between \code{[0-1000]}, 200 for low, 400 for medium and 700 for high/stringent scoring PPI
#' @param organism integer, default: 9606 (Homo Sapiens), see `?goat::load_genesets_go_bioconductor` taxid parameter for possible organism taxIDs
#' @param version character, default: 'latest', else a version to check availability, e.g. "12.0", if version not available the available versions are printed
#' @param versions NULL, else character vector with versions to choose from with version
#' @param network_type character, default: 'full', else 'physical' for only STRING documented physical interactions
#' @param link_data character, default: 'combined_only', else 'full' or 'detailed', see STRING documentation
#' @param folder character, default: tempdir(), else given folder path for where to download STRING files, converted to .parquet for compression and query efficiency, if tempdir() the temporary directory with the downloaded files are removed after the R session 
#' 
#' @description
#' STRING documentation: https://string-db.org/cgi/help?sessionId=baEZCS5u1RdM
#' 
#' Protocol used for downloading STRING files is https
#'
#' @returns dataframe (tibble) with protein-protein interactions (symbols and STRING IDs) and STRING combined score
#' @export
#' 
#' @import dplyr
#' @importFrom arrow read_delim_arrow write_parquet open_dataset
#'
#' @examplesIf FALSE
#' get_string_ppi(c("TP53", "EGFR", "BRCA1", "MTOR", "MYC", "SOX2"))
get_string_ppi <- function(aliases, score_threshold = 0L, organism = 9606L, network_type = 'full', link_data = 'combined_only', folder = tempdir(),
                           version = 'latest', versions = NULL) {
  ## validate input
  stopifnot(
    is.character(c(aliases, version, folder)),
    is.numeric(c(organism, score_threshold)),
    network_type %in% c("full", "physical"),
    link_data %in% c("full", "detailed", "combined_only")
  )
  
  ## aliases in STRING database are in upper format
  aliases <- toupper(aliases)
  
  ## set STRINGdb version
  if (is.null(versions)) {
    versions <- read.table(url("https://string-db.org/api/tsv-no-header/available_api_versions"))$V1
  }
  if (version == 'latest') {
    version <- versions[length(versions)]
  } else if ( ! version %in% versions) {
    stop(message("provided versions not available - possible STRINGdb versions: ", paste(versions, collapse = " ")))
  }
  if (version == "11.0b") version <- "11.0"
  ## format STRINGdb network type and link data
  if (tolower(network_type) == 'full') network_type <- ""
  if (tolower(link_data) == "combined_only") link_data <- "links.v"
  if (tolower(link_data) == "detailed") link_data <- "links.detailed.v"
  if (tolower(link_data) == "full") link_data <- "links.full.v"
  ## set folder, and download file urls, convert to .parquet, remove original download file
  if(file.access(folder, 0) != 0) dir.create(folder, recursive = TRUE)
  urls <- c(
    aliases_url = paste0("https://stringdb-downloads.org/download/protein.aliases.v", version, "/", organism, ".protein.aliases.v", version, ".txt.gz"),
    interactions_url = paste0("https://stringdb-downloads.org/download/protein.", network_type, link_data, version, "/", organism, ".protein.", network_type, link_data, version, ".txt.gz")
  )
  for (url in urls) {
    filename <- basename(url)
    filepath <- file.path(folder, filename)
    filepath_parquet <- paste0(filepath, ".parquet")
    if( ! file.exists(filepath_parquet)) {
      download.file(url, filepath)
      
      if (version %in% c("11.0", "11.0b")) arrow::write_parquet(arrow::read_delim_arrow(filepath, skip = 1L, delim = ifelse(grepl("aliases", url), "\t", " ")), filepath_parquet)
      else arrow::write_parquet(arrow::read_delim_arrow(filepath, delim = ifelse(grepl("aliases", url), "\t", " ")), filepath_parquet)
      unlink(filepath)
    }
  }
  
  ## map protein symbols/aliases to STRING IDs
  map_df <- arrow::open_dataset(paste0(file.path(folder, basename(urls["aliases_url"])), ".parquet")) %>%
    select(all_of(seq_len(3))) %>%
    rename_with(~ c("#string_protein_id", "alias", "source")[seq_along(.)]) %>%
    filter(.data$alias %in% aliases) %>%
    collect()
  map_df <- map_df[match(aliases, map_df$alias),]
  string_ids <- map_df$`#string_protein_id`
  
  ## return dataframe after collecting unique undirected protein-protein interactions
  return(arrow::open_dataset(paste0(file.path(folder, basename(urls["interactions_url"])), ".parquet")) %>%
           select(all_of(seq_len(3))) %>%
           rename_with(~ c("protein1", "protein2", "combined_score")[seq_along(.)]) %>%
           filter(.data$protein1 %in% string_ids & .data$protein2 %in% string_ids & .data$combined_score >= score_threshold) %>%
           collect() %>%
           mutate(protein_min = pmin(.data$protein1, .data$protein2), protein_max = pmax(.data$protein1, .data$protein2)) %>%
           distinct(.data$protein_min, .data$protein_max, .keep_all = TRUE) %>%
           left_join(map_df, by = c("protein1" = "#string_protein_id"), multiple = 'first') %>%
           rename(from_symbol = .data$alias) %>%
           left_join(map_df, by = c("protein2" = "#string_protein_id"), multiple = 'first') %>%
           rename(to_symbol = .data$alias) %>%
           select(.data$from_symbol, .data$to_symbol, .data$combined_score, .data$protein1, .data$protein2, -.data$protein_min, -.data$protein_max) %>%
           rename(from = .data$protein1, to = .data$protein2)
  )
}
