#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom grDevices dev.off png pdf col2rgb colorRamp rgb2hsv hsv rgb 
#' @importFrom graphics pie rect text par
#' @importFrom utils write.csv2 read.table download.file tail head browseURL zip combn
#' @importFrom stats hclust dist cutree setNames reorder runif alias
#' @importFrom methods new is
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
## DEVNOTE use instead to not write .data$: #' @importFrom dplyr .data
