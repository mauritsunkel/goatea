#' Get file extension
#' 
#' @export
#'
#' @param x string filepath
#'
#' @returns string file extension
#' 
#' @examples
#' file_extension(file.path(getwd(), 'filename.txt'))
file_extension <- function (x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

#' Wrap Shiny UI element with a loading spinner contained in html div tags
#'
#' @param id string: id of loader, used with show/hide in server side
#' @param ui_element wrapped Shiny UI element
#'
#' @export
#'
#' @importFrom htmltools div
#' @importFrom shiny icon
#' 
#' @returns html div element wrapped around given Shiny UI element 
#' 
#' @examples
#' wrap_loader('id_example', shiny::actionButton('id_example', 'example'))
wrap_loader <- function(id, ui_element) {
  div(ui_element,
      div(
        id = id, 
        style = "display:none;", 
        icon("spinner", class = "fa-spin", style = "font-size: 20px; color: #008000;")
      ))}

#' Wrap Shiny UI element with a hoverable tooltip contained in html div tags
#'
#' @param ui_element Shiny UI element to wrap with hovertext
#' @param hovertip text that will show as hover popup
#'
#' @importFrom htmltools tags
#'
#' @export
#' 
#' @returns tags$div element around given Shiny UI element 
#' 
#' @examples
#' wrap_hovertip(shiny::actionButton('id_example', 'example'), 'example')
wrap_hovertip <- function(ui_element, hovertip) {
  tags$div(title = hovertip, ui_element)
}

#' Process Shiny area input string
#' 
#' @param string_input siny string
#' @return processed string - no whitespace, enters, only letters and numbers
#' @export
#' @examples
#' process_string_input("test string \n")
process_string_input <- function(string_input) {
  if (length(string_input) == 1 && grepl('\n', string_input)) string_input <- strsplit(string_input, '\n')[[1]]
  string_input <- trimws(string_input)
  string_input <- gsub("[^a-zA-Z0-9]", "", string_input)
  string_input <- Filter(function(x) x != "", string_input)
  return(string_input)
}

#' Get term names by searching with (partial) keywords
#'
#' @param patterns keywords to match (grepl) term names
#' @param terms character vector to be grepl searched
#' @param pos_neg return positive matches or negate matches
#' @param all_any need all or any patterns to match search terms
#'
#' @export
#' 
#' @return character vector with matching terms by patterns
#' 
#' @examples
#' get_terms_by_keywords('circa', c('circadian rhythm', 'no match', 'circadian clock'))
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

#' Helper function to process and write combined genesets overview and genes overview output
#'
#' @param merged_enrichment row binded enrichment results dataframe/tibble
#' @param output_folder base output folder path to write to
#' @param filename string name of file
#' @param genes_overview table with row-wise gene info
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
#' @importFrom openxlsx write.xlsx
#' 
#' @return \value{None}
#' @noRd
#' 
#' @keywords internal
process_write_merged_enrichments <- function(merged_enrichment, output_folder, filename, genes_overview, top_n = NULL) {
  ## counters: no visible binding for global variable '.'
  . <- NULL
  merged_enrichment_sources <- lapply(unique(merged_enrichment$source), function(source) {
    dir.create(file.path(output_folder, "searches", source), recursive = TRUE)
    
    merged_enrichment_source <- merged_enrichment[merged_enrichment$source == source, ] %>%
      select(.data$genelist_ID, source, .data$name, .data$ngenes_input, .data$ngenes, .data$ngenes_signif, .data$pvalue_adjust, .data$zscore, .data$symbol) %>%
      ## Select top 50 per genelist_ID based on pvalue_adjust before unnesting
      { if (!is.null(top_n)) group_by(.data = ., .data$genelist_ID) %>%
          slice_min(.data$pvalue_adjust, n = top_n, with_ties = FALSE) %>%
          ungroup() else . } %>%
      unnest(.data$symbol) %>% ## long format by genes
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
        .data$genelist_ID, source, .data$name, .data$ngenes_input, .data$ngenes, .data$ngenes_signif, .data$pvalue_adjust, .data$zscore, .data$symbol,
        .data$gene, .data$gene_annotation, .data$genelist_overlap, .data$best_efsi, .data$best_pval,
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
#' 
#' @export
#' 
#' @examples
#' scale_values_between(c(1,3,1,4,1,6,1,6,5,7))
scale_values_between <- function(values, old_min = min(values), old_max = max(values), new_min = 0, new_max = 100) {
  stopifnot(is.numeric(c(values, old_min, old_max, new_min, new_max)))
  if (old_min == old_max) return(rep(new_max, length(values)))
  ((values - old_min) / (old_max - old_min)) * (new_max - new_min) + new_min
}

#' Hex code colors to rgba format
#'
#' @param hexcolors character (vector), hexcode colors (e.g. #FFFFFF)
#' @param alpha numeric in range \code{[0-1]}, default: NULL to use full opacity or given opacity (AA) in hexcolors (#RRGGBBAA)
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

#' Set base folder
#'
#' @param folder_path character, default NULL, else existing directory
#'
#' @description
#' sets/gets given folder path if provided
#' else checks in order: 
#' - path.expand("~") (tilde (~) expands to HOME folder path)
#' - Sys.getenv("R_USER") (set on R session start)
#' - Sys.getenv("USERPROFILE") (Windows specific) 
#'
#' @returns character folder path
#' 
#' @export
#'
#' @examples
#' get_base_folder()
get_base_folder <- function(folder_path = NULL) {
  if ( ! is.null(folder_path)) {
    if ( ! is.character(folder_path)) stop("if providing path, it must be a character")
    if (dir.exists(folder_path)) return(folder_path)
  }
  
  path_opts <- c(
    path.expand("~"),
    Sys.getenv("R_USER"),
    Sys.getenv("USERPROFILE")
  )
  for (p in path_opts) {
    if (p != "" & dir.exists(p)) return(p)
  }
  
  msg <- "no existing folder path given, and all base folder path options returned '' or do not exist, please supply an existing base folder path..."
  warning(msg)
  if ( ! is.null(shiny::getDefaultReactiveDomain())) shiny::showNotification(msg, type = 'warning')
}

#' Rename the gene overview
#'
#' @param names names to rename gene overview
#' @param genes_overview UI given genes overview dataframe (rv_genelists_overlap$gene_overview)
#'
#' @returns genes overview renamed
rename_gene_overview <- function(names, genes_overview) {
  df <- genes_overview
  
  sample_cols <- df %>%
    select(matches("(_efsi|_pval|_perc|_geneSetRatio)$")) %>%
    colnames()
  
  old_sample_names <- unique(gsub("(_efsi|_pval|_perc|_geneSetRatio)$", "", sample_cols))
  
  for (i in seq_along(old_sample_names)) {
    old_name <- old_sample_names[i]
    new_name <- names[i]
    df <- df %>%
      rename_with(
        .fn = function(x) gsub(paste0("^", old_name, "(?=_efsi|_pval|_perc|_geneSetRatio)"), new_name, x, perl = TRUE),
        .cols = matches(paste0("^", old_name, "(_efsi|_pval|_perc|_geneSetRatio)$"))
      )
    df$genelist_overlap <- gsub(paste0("\\b", old_name, "\\b"), new_name, df$genelist_overlap)
  }
  return(df)
}
