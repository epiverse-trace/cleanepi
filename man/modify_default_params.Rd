% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default_cleanepi_settings.R
\name{modify_default_params}
\alias{modify_default_params}
\title{Update \code{\link{clean_data}} default argument's values with the
user-provided values.}
\usage{
modify_default_params(defaults, params, strict = TRUE)
}
\arguments{
\item{defaults}{A \code{<list>} with the default arguments}

\item{params}{A \code{<list>} with the user-specified arguments}

\item{strict}{A \code{<logical>} that specified whether to trigger an error
or not when there is a difference between the list of default arguments
and list of the arguments provided by the user. Default is \code{TRUE}.}
}
\value{
The updated \code{<list>} of parameters that will be used to perform
the data cleaning.
}
\description{
Update \code{\link{clean_data}} default argument's values with the
user-provided values.
}
\keyword{internal}
