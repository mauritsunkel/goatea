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
wrap_hovertip <- function(text, ui_element) {
  tags$div(title = text, ui_element)
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