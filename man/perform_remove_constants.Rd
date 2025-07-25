% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_constants.R
\name{perform_remove_constants}
\alias{perform_remove_constants}
\title{Remove constant data.}
\usage{
perform_remove_constants(data, cutoff)
}
\arguments{
\item{data}{The input \code{<data.frame>} or \code{<linelist>}}

\item{cutoff}{A \code{<numeric>} value specifying the cut-off for removing
constant data. The possible values vary between 0 (excluded) and 1
(included). The default is 1 i.e. remove rows and columns with 100\%
constant data.}
}
\value{
A \code{<list>} with the input dataset where all empty rows and
columns as well as constant columns have been removed.
}
\description{
This function is called at each iteration of the constant data removal
process until no constant data remains.
}
\keyword{internal}
