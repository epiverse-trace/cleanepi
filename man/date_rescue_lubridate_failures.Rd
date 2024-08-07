% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guess_dates.R
\name{date_rescue_lubridate_failures}
\alias{date_rescue_lubridate_failures}
\title{Find the dates that lubridate couldn't}
\usage{
date_rescue_lubridate_failures(date_a_frame, original_dates, mxl = TRUE)
}
\arguments{
\item{date_a_frame}{A data frame where each column contains a different
parsing of the same date vector}

\item{original_dates}{The vector of original dates.}

\item{mxl}{"modern excel" if TRUE, then it uses 1900 as the origin, otherwise
1904 is used as the origin.}
}
\value{
The input data frame where the values that do not match the proposed
formats have been converted into Date.
}
\description{
Find the dates that lubridate couldn't
}
\keyword{internal}
