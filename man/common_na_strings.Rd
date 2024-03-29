% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-common_na_strings.R
\docType{data}
\name{common_na_strings}
\alias{common_na_strings}
\title{Common strings representing missing values}
\format{
A vector of 35 character strings.
}
\source{
This vector is a combination of \code{naniar::common_na_strings}
(\url{https://github.com/njtierney/naniar/}) and other strings found in the
literature.
}
\usage{
common_na_strings
}
\description{
This vector contains common values of NA (missing) and is intended for
use within \{cleanepi\} functions \code{\link[=replace_missing_values]{replace_missing_values()}}.
The current list of strings used can be found by printing out
\code{common_na_strings}. It serves as a helpful tool to explore your data
for possible missing values. However, I strongly caution against using
this to replace \code{NA} values without meticulously examining the
incidence for each case. Please note that \code{common_na_strings} utilizes
\verb{\\\\} around the "?", ".", and "*" characters to prevent their wildcard
}
\keyword{datasets}
