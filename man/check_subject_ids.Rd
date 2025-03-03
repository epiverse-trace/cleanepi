% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_subject_ids.R
\name{check_subject_ids}
\alias{check_subject_ids}
\title{Check whether the subject IDs comply with the expected format. When incorrect
IDs are found, the function sends a warning and the user can call the
\code{\link{correct_subject_ids}} function to correct them.}
\usage{
check_subject_ids(
  data,
  target_columns,
  prefix = NULL,
  suffix = NULL,
  range = NULL,
  nchar = NULL
)
}
\arguments{
\item{data}{The input \code{<data.frame>} or \code{<linelist>}}

\item{target_columns}{A \code{<vector>} of column names with the subject ids.}

\item{prefix}{A \code{<character>} with the expected prefix used in the
subject IDs}

\item{suffix}{A \code{<character>} with the expected suffix used in the
subject IDs}

\item{range}{A \code{<vector>} with the expected range of numbers in the
subject IDs}

\item{nchar}{An \code{<integer>} that represents the expected number of
characters in the subject ids.}
}
\value{
The input dataset with a warning if incorrect subject ids were found
}
\description{
Check whether the subject IDs comply with the expected format. When incorrect
IDs are found, the function sends a warning and the user can call the
\code{\link{correct_subject_ids}} function to correct them.
}
\examples{
dat <- check_subject_ids(
  data = readRDS(
    system.file("extdata", "test_df.RDS", package = "cleanepi")
  ),
  target_columns = "study_id",
  prefix = "PS",
  suffix = "P2",
  range = c(1, 100),
  nchar = 7
)
}
