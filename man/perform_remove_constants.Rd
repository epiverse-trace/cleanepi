% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_constants.R
\name{perform_remove_constants}
\alias{perform_remove_constants}
\title{Perform constant data removal.}
\usage{
perform_remove_constants(data, cutoff)
}
\arguments{
\item{data}{The input data frame or linelist}

\item{cutoff}{The cut-off for empty rows and columns removal. If provided,
only rows and columns where the percent of missing data is greater than
this cut-off will removed.}
}
\value{
A list with the input dataset where all empty rows and columns as
well as constant columns have been removed.
}
\description{
This function is called at every iteration of the constant data removal until
no constant data is found.
}
\keyword{internal}
