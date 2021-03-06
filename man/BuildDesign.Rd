% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BuildDesign.R
\name{BuildDesign}
\alias{BuildDesign}
\title{Builds the design matrix}
\usage{
BuildDesign(tdiffa, numma, numme)
}
\arguments{
\item{tdiffa}{The difference in days as double which stems from the analysis period.}

\item{numma}{The transit number (start).}

\item{numme}{The transit number (end).}
}
\value{
Returns a matrix
}
\description{
Builds the xdesign.matrix by calling Funcs. For internal use.
}
