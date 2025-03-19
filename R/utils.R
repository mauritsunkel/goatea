#' Get file extension
#'
#' @param x string filepath
#'
#' @returns string file extension
file_extension <- function (x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

#' throw error if R package is unavailable
#'
#' @param pkg R package name
#' @param msg function name / reference for user
#' @noRd
check_dependency = function(pkg, msg) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0(
      "An optional dependency for ",
      msg,
      " is not installed; R package '",
      pkg,
      "' is not available. For convenience, you may use the following command to install all dependencies (including optional) for the 'goat' R package; pak::pkg_install('ftwkoopmans/goat', dependencies = TRUE)"
    ), call. = FALSE)
  }
}


#' Wrap Shiny UI element with a loading spinner contained in html div tags
#'
#' @param id string: id of loader, used with show/hide in server side
#' @param ui_element wrapped Shiny UI element
#'
#' @export
#'
wrap_loader <- function(id, ui_element) {
  div(ui_element,
      div(
        id = id, 
        style = "display:none;", 
        icon("spinner", class = "fa-spin", style = "font-size: 20px; color: #008000;")
      ))}

#' Wrap Shiny UI element with a hoverable tooltip contained in html div tags
#'
#' @param text 
#' @param ui_element 
#'
#' @export
wrap_hovertip <- function(ui_element, hovertip) {
  tags$div(title = hovertip, ui_element)
}

#' Get gene annotation for mouse
#'
#' Can extend functino to get gene annotations for other organisms within annotables package.
#'
#' @param gene_symbols character vector of ensembl genes 109 symbols
#' @param organism default "Hs" (Homo Sapiens), otherwise: "Mm" (Mus Musculus)
#'
#' @export
get_gene_annotation <- function(gene_symbols, organism = "Hs") {
  if (organism == "Hs") {
    annodata <- annotables::grch38
  } else if (organism == "Mm") {
    annodata <- annotables::grcm38
  }
  return(plyr::mapvalues(
    x = gene_symbols,
    from = annodata$symbol,
    to = annodata$description,
    warn_missing = FALSE))
}

#' Get term names by searching with (partial) keywords
#'
#' @param patterns keywords to match (grepl) term names
#' @param terms character vector to be grepl searched
#' @param pos_neg return positive matches or negate matches
#' @param all_any need all or any patterns to match search terms
#'
#' @export
get_terms_by_keywords <- function(
    patterns, terms, pos_neg = 'pos', all_any = 'all') {
  patterns <- tolower(patterns)
  
  if (length(terms) == 0 ) {
    return (terms)
  } else if (length(terms) == 1) {
    if (pos_neg == 'pos') {
      res <- terms[get(all_any)(sapply(patterns, function(p) {grepl(p, tolower(terms))}))]
    } else if (pos_neg == 'neg') {
      res <- get(all_any)(sapply(patterns, function(p) {grepl(p, tolower(terms))}))
      if (all_any == "all") {
        res <- ! res
      }
    }
  } else {
    if (pos_neg == 'pos') {
      res <- terms[apply(sapply(patterns, function(p) {grepl(p, tolower(terms))}), 1, function(row) {get(all_any)(row)})]
    } else if (pos_neg == 'neg') {
      res <- terms[ ! apply(sapply(patterns, function(p) {grepl(p, tolower(terms))}), 1, function(row) {get(all_any)(row)})]
    }
  }
  
  return(unique(res))
}

#' Process Shiny areaInput string
#'
#' @param string_input string
#'
#' @returns string
process_string_input <- function(string_input) {
  if (length(string_input) == 1 && grepl('\n', string_input)) string_input <- strsplit(string_input, "\n")[[1]]
  string_input <- trimws(string_input)
  string_input <- gsub("[^a-zA-Z0-9]", "", string_input)
  string_input <- Filter(function(x) x != "", string_input)
  return(string_input)
}

#' Helper function to process and write combined genesets overview and genes overview output
#'
#' @param merged_enrichment row binded enrichment results dataframe/tibble
#' @param output_folder base output folder path to write to
#' @param top_n n top results per genelist per source based on genesets adjusted pvalue
#' 
#' @importFrom tidyr unnest
#' @importFrom purrr pmap_dbl
#' @importFrom dplyr group_by
#' @importFrom dplyr slice_min
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' 
#' @keywords internal
process_write_merged_enrichments <- function(merged_enrichment, output_folder, filename, top_n = NULL) {
  
  merged_enrichment_sources <- lapply(unique(merged_enrichment$source), function(source) {
    dir.create(file.path(output_folder, "searches", source), recursive = TRUE)
    
    merged_enrichment_source <- merged_enrichment[merged_enrichment$source == source, ] %>%
      select(genelist_ID, source, name, ngenes_input, ngenes, ngenes_signif, pvalue_adjust, zscore, symbol) %>%
      ## Select top 50 per genelist_ID based on pvalue_adjust before unnesting
      { if (!is.null(top_n)) group_by(., genelist_ID) %>%
          slice_min(pvalue_adjust, n = top_n, with_ties = FALSE) %>%
          ungroup() else . } %>%
      unnest(symbol) %>% ## long format by genes
      ## combine genesets with genes
      left_join(genes_overview, by = "symbol") %>%
      ## set best effectsize and pvalue columns based on respective values (excluding NA values)
      mutate(
        best_efsi = purrr::pmap_dbl(select(., ends_with("_efsi")),
                                    ~ { vals <- c(...);
                                    vals <- vals[!is.na(vals)];
                                    if (length(vals) > 0) vals[which.max(abs(vals))] else NA}),
        best_pval = purrr::pmap_dbl(select(., ends_with("_pval")),
                                    ~ { vals <- c(...);
                                    vals <- vals[!is.na(vals)];
                                    if (length(vals) > 0) vals[which.min(vals)] else NA})
      ) %>%
      ## order columns
      select(
        genelist_ID, source, name, ngenes_input, ngenes, ngenes_signif, pvalue_adjust, zscore, symbol,
        gene, gene_annotation, genelist_overlap, best_efsi, best_pval,
        ends_with("_efsi"),
        ends_with("_perc"),
        ends_with("_pval"),
        ends_with("geneSetRatio")
      )
    ## write results
    openxlsx::write.xlsx(merged_enrichment_source, file.path(output_folder, "searches", source, paste0(filename, ".xlsx")))
  })
}

#' Scale values between given min/max
#'
#' @param values numeric (vector) 
#' @param old_min numeric, default: min(values), else set as current expected minimum of values
#' @param old_max numeric, default: max(values), else set as current expected maximum of values
#' @param new_min numeric, default: 0, else set to wanted new minimum value
#' @param new_max numeric, default: 100, else set to wanted new maximum value
#'
#' @returns scaled numeric values
#' @export
scale_values_between <- function(values, old_min = min(values), old_max = max(values), new_min = 0, new_max = 100) {
  stopifnot(is.numeric(c(values, old_min, old_max, new_min, new_max)))
  if (old_min == old_max) return(rep(new_max, length(values)))
  ((values - old_min) / (old_max - old_min)) * (new_max - new_min) + new_min
}




#' Hex code colors to rgba format
#'
#' @param hexcolors character (vector), hexcode colors (e.g. #FFFFFF)
#' @param alpha numeric in range [0-1], default: NULL to use full opacity or given opacity (AA) in hexcolors (#RRGGBBAA)
#'
#' @returns colors in rgba format
#' 
#' @export
#'
#' @examples
#' colors <- colorify(5)
#' hexcolor2rgba(colors)
#' hexcolor2rgba(colors, alpha = .5)
#' colors <- gsub('FF$', 75, colors)
#' hexcolor2rgba(colors)
#' hexcolor2rgba(colors, alpha = .5)
hexcolor2rgba <- function(hexcolors, alpha = NULL) {
  stopifnot(
    is.character(hexcolors),
    is.null(alpha) | is.numeric(alpha) && alpha >= 0 && alpha <= 1
  )
  ## set rgb
  hex <- gsub('#', '', hexcolors) # strip #
  r <- as.numeric(paste0('0x', substr(hex, 1, 2))) # extract red
  g <- as.numeric(paste0('0x', substr(hex, 3, 4))) # extract green
  b <- as.numeric(paste0('0x', substr(hex, 5, 6))) # extract blue
  ## set alpha
  if (is.null(alpha)) {
    alpha <- 1
    if (nchar(hex)[1] == 8) alpha <- as.numeric(paste0('0x', substr(hex, 7, 8))) / 255
  }
  return(paste0('rgba(', r, ',', g, ',', b, ',', alpha*100, ')')) # convert to RGBA format
}
