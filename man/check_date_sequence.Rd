% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_date_sequence.R
\name{check_date_sequence}
\alias{check_date_sequence}
\title{Check whether the order of the sequence of date-events is valid}
\usage{
check_date_sequence(data, target_columns, remove = FALSE)
}
\arguments{
\item{data}{A data frame}

\item{target_columns}{A vector of event column names. Users should specify at
least 2 column names in the expected order.
For example: target_columns = c("date_symptoms_onset",
"date_hospitalization", "date_death"). When the input data is a \code{linelist}
object, this parameter can be set to \code{linelist_tags} if you wish to
the date sequence across tagged columns only.}

\item{remove}{A Boolean to specify if rows with incorrect order
should be filtered out or not. The default is FALSE}
}
\value{
Rows of the input data frame with incorrect date sequence
if \code{remove = FALSE}, the input data frame without those
rows if not.
}
\description{
Checks whether a date sequence in
a vector of  specified columns is in order or not.
}
\examples{
good_date_sequence <- check_date_sequence(
  data           = readRDS(system.file("extdata", "test_df.RDS",
                                       package = "cleanepi")),
  target_columns = c("date_first_pcr_positive_test", "date.of.admission"),
  remove         = FALSE
)
}
