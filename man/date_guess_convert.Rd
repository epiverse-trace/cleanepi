% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_standardization_helpers.R
\name{date_guess_convert}
\alias{date_guess_convert}
\title{Guess if a character vector contains Date values, and convert them to date}
\usage{
date_guess_convert(data, error_tolerance, timeframe, orders)
}
\arguments{
\item{data}{A \code{<data.frame>}}

\item{error_tolerance}{A \code{<numeric>} between 0 and 1 indicating the
proportion of entries which cannot be identified as dates to be tolerated;
if this proportion is exceeded, the original vector is returned, and a
message is issued; defaults to 0.4 (40 percent).}

\item{timeframe}{A \code{<vector>} of 2 values of type \code{<Date>}. If
provided, date values that do not fall within this timeframe will be set
to \code{NA}.}

\item{orders}{A \code{<list>} or \code{<vector>} of characters with the date
codes for fine-grained parsing of dates. This allows for parsing of mixed
dates. If a \code{<list>} is supplied, that \code{<list>} will be used for
successive tries in parsing. When this is not provided
(\code{orders = NULL}), the function will use the following order defined
in the guesser:

\if{html}{\out{<div class="sourceCode">}}\preformatted{list(
  quarter_partial_dates = c("Y", "Ym", "Yq"),
  world_digit_months = c("Yq", "ymd", "ydm", "dmy", "mdy", "myd", "dym",
                         "Ymd", "Ydm", "dmY", "mdY", "mYd", "dYm"),
  world_named_months = c("dby", "dyb", "bdy", "byd", "ybd", "ydb",
                         "dbY", "dYb", "bdY", "bYd", "Ybd", "Ydb"),
  us_format = c("Omdy", "YOmd")
)
}\if{html}{\out{</div>}}}
}
\value{
A \code{<list>} with the following two elements: the input data
frame where the character columns with date values have been converted
into \code{<Date>}, and a vector of column names where there are numeric
values that can also be of type Date.
}
\description{
Guess if a character vector contains Date values, and convert them to date
}
\keyword{internal}
