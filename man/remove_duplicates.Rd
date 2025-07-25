% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_and_remove_duplicates.R
\name{remove_duplicates}
\alias{remove_duplicates}
\title{Remove duplicates}
\usage{
remove_duplicates(data, target_columns = NULL)
}
\arguments{
\item{data}{The input \code{<data.frame>} or \code{<linelist>}.}

\item{target_columns}{A \code{<vector>} of column names to use when looking
for duplicates. When the input data is a \code{linelist} object, this
parameter can be set to \code{linelist_tags} if you wish to look for
duplicates on tagged columns only. Default is \code{NULL}.}
}
\value{
The input data \code{<data.frame>} or \code{<linelist>} without the
duplicated rows identified from all or the specified columns.
}
\description{
When removing duplicates, users can specify a set columns to consider with
the \code{target_columns} argument.
}
\details{
\strong{Caveat:} In many epidemiological datasets, multiple rows may share the
same value in one or more columns without being true duplicates.
For example, several individuals might have the same  symptom onset date
and admission date. Be cautious when using this function—especially when
applying it to a single target column—to avoid incorrect identification
or removal of valid entries.
}
\examples{
data <- readRDS(
  system.file("extdata", "test_linelist.RDS", package = "cleanepi")
)
no_dups <- remove_duplicates(
  data = data,
  target_columns = "linelist_tags"
)

# print the removed duplicates
print_report(no_dups, "removed_duplicates")

# print the detected duplicates
print_report(no_dups, "found_duplicates")
}
