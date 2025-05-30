% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{wrap_loader}
\alias{wrap_loader}
\title{Wrap Shiny UI element with a loading spinner contained in html div tags}
\usage{
wrap_loader(id, ui_element)
}
\arguments{
\item{id}{string: id of loader, used with show/hide in server side}

\item{ui_element}{wrapped Shiny UI element}
}
\value{
html div element wrapped around given Shiny UI element
}
\description{
Wrap Shiny UI element with a loading spinner contained in html div tags
}
\examples{
wrap_loader('id_example', shiny::actionButton('id_example', 'example'))
}
