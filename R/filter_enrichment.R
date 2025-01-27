#' Filter enrichment
#'
#' Search and filter and sort or summarize (compiled) enrichment output.
#'
#' @param df enrichment output dataframe
#' @param genes_input default: UI input/character vector of genes to select df terms for
#' @param genes_type default: 'any', else 'all', use to define to take only specific terms containing any or all associated genes
#' @param terms_query dfeault: UI input/character vector of keywords to match (grepl) term names
#' @param terms_query_all_any default: 'any', else 'all', defines if terms should match any or all of the query keywords given
#' @param terms_antiquery dfeault: UI input/character vector of keywords to NOT match (grepl) term names
#' @param terms_antiquery_all_any default: 'any', else 'all', defines if terms should NOT match any or all of the query keywords given
#' @param min_ngenes default: 0, set higher to filter terms with less n genes
#' @param min_ngenes_signif default: 0, set higher to filter terms with less n significant genes
#' @param min_abs_zscore default: 0, set higher to filter terms with less absolute zscore
#' @param max_pvalue_adjust default: 1, set lower to filter terms with lower adjusted p-value for multiple correction 
#'
#' @export
filter_enrichment <- function(
    df,
    genes_input = "",
    genes_any_all = c("any", "all"),
    terms_query = "",
    terms_query_all_any = c("any", "all"),
    terms_antiquery = "",
    terms_antiquery_all_any = c("any", "all"),
    min_ngenes = 0,
    min_ngenes_input = 0,
    min_ngenes_signif = 0,
    min_abs_zscore = 0,
    max_pvalue_adjust = 1
) {
  genes_any_all <- match.arg(genes_any_all)
  terms_query_all_any <- match.arg(terms_query_all_any)
  terms_antiquery_all_any <- match.arg(terms_antiquery_all_any)
  
  print(1)
  
  ## keep terms matching by genes
  if (genes_input != "") {
    genes_input <- process_area_input(genes_input)
    match_terms_by_genes <- if (genes_any_all == "any") {
      sapply(df$symbol, function(symbols) any(tolower(genes_input) %in% tolower(symbols)))
    } else if (genes_any_all == "all") {
      sapply(df$symbol, function(symbols) all(tolower(genes_input) %in% tolower(symbols)))
    }
    df <- df[match_terms_by_genes, ]
  }
  print(2)
  ## keep terms matching any/all queries and not any/all antiqueries
  if (terms_query != "") {
    print(2.1)
    terms_query <- process_area_input(terms_query)
    hits <- get_terms_by_keywords(
      patterns = terms_query,
      terms = df$name,
      pos_neg = 'pos',
      all_any = terms_query_all_any
    )
    print(2.2)
    if (terms_antiquery != "") {
      print(2.3)
      terms_antiquery <- process_area_input(terms_antiquery)
      hits <- get_terms_by_keywords(
        patterns = terms_antiquery,
        terms = hits,
        pos_neg = 'neg',
        all_any = terms_antiquery_all_any
      )
      print(2.4)
    }
    df <- df[df$name %in% hits,]
    print(2.5)
  }
  print(3)
  ## filter numericals
  df <- df[df$ngenes >= min_ngenes,]
  df <- df[df$ngenes_input >= min_ngenes_input,]
  df <- df[df$ngenes_signif >= min_ngenes_signif,]
  df <- df[abs(df$zscore) >= min_abs_zscore,]
  df <- df[df$pvalue_adjust <= max_pvalue_adjust,]
  print(4)
  return(df)
}
