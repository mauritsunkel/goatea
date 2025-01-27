# TODO add doc
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
#' @param area_input Shiny areaInput string
#'
#' @returns string
process_area_input <- function(area_input) {
  if (length(area_input) == 1 && grepl('\n', area_input)) area_input <- strsplit(area_input, "\n")[[1]]
  area_input <- trimws(area_input)
  area_input <- gsub("[^a-zA-Z0-9]", "", area_input)
  return(area_input)
}


