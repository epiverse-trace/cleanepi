% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_report.R
\name{print_report}
\alias{print_report}
\title{Generate report from data cleaning operations}
\usage{
print_report(
  data,
  what = NULL,
  print = FALSE,
  report_title = "{cleanepi} data cleaning report",
  output_file_name = NULL,
  format = "html"
)
}
\arguments{
\item{data}{A \code{<data.frame>} or \code{<linelist>} object returned from
the \code{\link{clean_data}} or the main functions of each data cleaning
module.}

\item{what}{A \code{<character>} with the name of the specific data cleaning
report which would be displayed. The possible values are:
\describe{
\item{\code{incorrect_date_sequence}}{To display rows with the incorrect date
sequences}
\item{\code{colnames}}{To display the column names before and after
cleaning}
\item{\code{converted_into_numeric}}{To display the names of the columns that
that have been converted into numeric}
\item{\code{date_standardization}}{To display rows in the cleaned data with
date values that are outside of the specified time frame, and rows
with date values that comply with multiple formats}
\item{\code{misspelled_values}}{To display the detected misspelled values}
\item{\code{removed_duplicates}}{To display the duplicated rows that have
been removed}
\item{\code{found_duplicates}}{To display the duplicated rows}
\item{\code{constant_data}}{To display the constant data i.e. constant
columns, empty rows and columns}
\item{\code{missing_values_replaced_at}}{To display the names of the columns
where the missing value strings have been replaced with NA}
\item{\code{incorrect_subject_id}}{To display the missing, duplicated and
invalid subject subject IDs}
}}

\item{print}{A \code{<logical>} that specifies whether to to open the report
in your browser in the form of a HTML file or no. Default is \code{FALSE}.}

\item{report_title}{A \code{<character>} with the title that will appear on
the report}

\item{output_file_name}{A \code{<character>} used to specify the name of the
report file, excluding any file extension. If no file name is supplied,
one will be automatically generated with the format
\code{cleanepi_report_YYMMDD_HHMMSS}.}

\item{format}{A \code{<character>} with the file format of the report.
Currently only \code{"html"} is supported.}
}
\value{
A \code{<character>} containing the name and path of the saved
report
}
\description{
Generate report from data cleaning operations
}
\examples{
\donttest{
data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
test_dictionary <- readRDS(
  system.file("extdata", "test_dictionary.RDS", package = "cleanepi")
)

# scan through the data
scan_res <- scan_data(data)

# Perform data cleaning
cleaned_data <- data \%>\%
 standardize_column_names(keep = NULL, rename = c("DOB" = "dateOfBirth")) \%>\%
 replace_missing_values(target_columns = NULL, na_strings = "-99") \%>\%
 remove_constants(cutoff = 1.0) \%>\%
 remove_duplicates(target_columns = NULL) \%>\%
 standardize_dates(
   target_columns = NULL,
   error_tolerance = 0.4,
   format = NULL,
   timeframe = as.Date(c("1973-05-29", "2023-05-29"))
 ) \%>\%
 check_subject_ids(
   target_columns = "study_id",
   prefix = "PS",
   suffix = "P2",
   range = c(1L, 100L),
   nchar = 7L
 ) \%>\%
 convert_to_numeric(target_columns = "sex", lang = "en") \%>\%
 clean_using_dictionary(dictionary = test_dictionary)

# add the data scanning result to the report
cleaned_data <- add_to_report(
  x = cleaned_data,
  key = "scanning_result",
  value = scan_res
)

# save a report in the current directory using the previously-created objects
print_report(
  data = cleaned_data,
  report_title = "{cleanepi} data cleaning report",
  output_file_name = NULL,
  format = "html",
  print = TRUE
)
}

}
