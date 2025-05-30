% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_splitdot.R
\name{plot_splitdot}
\alias{plot_splitdot}
\title{Plot splitdot plot}
\usage{
plot_splitdot(enrichment, topN = NA)
}
\arguments{
\item{enrichment}{GOAT enrichment result}

\item{topN}{default: NA to plot all, else integer to plot topN terms by adjusted pvalue}
}
\value{
ggplot2 object
}
\description{
Plot splitdot plot
}
\examples{
plot_splitdot(goatea::example_enrichment)
}
