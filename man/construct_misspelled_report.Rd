% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionary_based_cleaning.R
\name{construct_misspelled_report}
\alias{construct_misspelled_report}
\title{Build the report for the detected misspelled values during dictionary-based
data cleaning operation}
\usage{
construct_misspelled_report(misspelled_options, data)
}
\arguments{
\item{misspelled_options}{A list with the detected misspelled values in the
columns of interest.}

\item{data}{A data frame}
}
\value{
A data frame the details about where in the input data the
misspelled values were found.
}
\description{
Build the report for the detected misspelled values during dictionary-based
data cleaning operation
}
\keyword{internal}
