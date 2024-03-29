% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_subject_ids.R
\name{correct_subject_ids}
\alias{correct_subject_ids}
\title{Correct the wrong subject IDs based on the user-provided values.}
\usage{
correct_subject_ids(data, target_columns, correction_table)
}
\arguments{
\item{data}{The input data frame or linelist}

\item{target_columns}{A vector of column names with the subject ids.}

\item{correction_table}{A data frame with the following two columns:
\enumerate{
\item \code{from}: a column with the wrong subject IDs,
\item \code{to}: a column with the values to be used to substitute the
incorrect ids.
}}
}
\value{
The input dataset where all subject ids comply with the expected
format.
}
\description{
After detecting incorrect subject IDs from the \code{check_subject_ids()}
function, use this function to provide the correct IDs and perform the
substitution.
}
\examples{
# detect the incorrect subject ids
dat <- check_subject_ids(
  data           = readRDS(system.file("extdata", "test_df.RDS",
                                       package = "cleanepi")),
  target_columns = "study_id",
  prefix         = "PS",
  suffix         = "P2",
  range          = c(1, 100),
  nchar          = 7
)

# generate the correction table
correction_table <- data.frame(
  from = c("P0005P2", "PB500P2", "PS004P2-1"),
  to   = c("PB005P2", "PB050P2", "PS004P2")
)

# perform the correction
dat <- correct_subject_ids(
  data             = dat,
  target_columns   = "study_id",
  correction_table = correction_table
)
}
