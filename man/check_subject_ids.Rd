% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_subject_ids.R
\name{check_subject_ids}
\alias{check_subject_ids}
\title{Check whether the subject IDs complies with user-specified format}
\usage{
check_subject_ids(
  data = NULL,
  id.position = 1,
  format = NULL,
  check = TRUE,
  prefix = NULL,
  suffix = NULL,
  range = NULL
)
}
\arguments{
\item{data}{the data frame of interest}

\item{id.position}{the column position of the variable that unique identifies the subjects. This should only be specified when the column with the subject IDs is not the first column. default is 1.}

\item{format}{a template of the correct format for the subject IDs}

\item{check}{a boolean that specifies whether to clean the subject IDs or not}

\item{prefix}{the prefix used in the subject IDs}

\item{suffix}{the prefix used in the subject IDs}

\item{range}{a vector with the range of numbers in the sample IDs}
}
\value{
The will display messages about incorrect subject IDs, nothing if all IDs are correct.
}
\description{
Check whether the subject IDs complies with user-specified format
}
\examples{
check_subject_ids(data=data.table::fread(system.file("extdata","test.txt", package = "cleanepi")),
                  id.position=1,
                  format="PS000P2",
                  check=TRUE,
                  prefix="PS",
                  suffix="P2",
                  range=c(0,100)
                  )
}
