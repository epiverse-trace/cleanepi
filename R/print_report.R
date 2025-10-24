#' Generate report from data cleaning operations
#'
#' @param data A \code{<data.frame>} or \code{<linelist>} object returned from
#'    the \code{\link{clean_data}} or the main functions of each data cleaning
#'    module.
#' @param report_title A \code{<character>} with the title that will appear on
#'    the report
#' @param output_file_name A \code{<character>} used to specify the name of the
#'    report file, excluding any file extension. If no file name is supplied,
#'    one will be automatically generated with the format
#'    \code{cleanepi_report_YYMMDD_HHMMSS}.
#' @param format A \code{<character>} with the file format of the report.
#'    Currently only \code{"html"} is supported.
#' @param print A \code{<logical>} that specifies whether to to open the report
#'    in your browser in the form of a HTML file or no. Default is \code{FALSE}.
#' @param what A \code{<character>} with the name of the specific data cleaning
#'    report which would be displayed. The possible values are:
#'    \describe{
#'      \item{`incorrect_date_sequence`}{To display rows with the incorrect date
#'          sequences}
#'      \item{`colnames`}{To display the column names before and after
#'          cleaning}
#'      \item{`converted_into_numeric`}{To display the names of the columns that
#'          that have been converted into numeric}
#'      \item{`date_standardization`}{To display rows in the cleaned data with
#'          date values that are outside of the specified time frame, and rows
#'          with date values that comply with multiple formats}
#'      \item{`misspelled_values`}{To display the detected misspelled values}
#'      \item{`removed_duplicates`}{To display the duplicated rows that have
#'          been removed}
#'      \item{`found_duplicates`}{To display the duplicated rows}
#'      \item{`constant_data`}{To display the constant data i.e. constant
#'          columns, empty rows and columns}
#'      \item{`missing_values_replaced_at`}{To display the names of the columns
#'          where the missing value strings have been replaced with NA}
#'      \item{`incorrect_subject_id`}{To display the missing, duplicated and
#'          invalid subject subject IDs}
#'      \item{`scanning_result`}{To display the output of the scan_data()
#'          function}
#'    }
#'
#' @returns A \code{<character>} containing the name and path of the saved
#'    report
#' @examples
#' \dontrun{
#'   data <- readRDS(
#'     system.file("extdata", "test_df.RDS",package = "cleanepi")
#'   )
#'   test_dictionary <- readRDS(
#'     system.file("extdata", "test_dictionary.RDS", package = "cleanepi")
#'   )
#'
#'   # scan through the data
#'   scan_res <- scan_data(data)
#'
#'   # Perform data cleaning
#'   cleaned_data <- data %>%
#'    standardize_column_names(
#'      keep = NULL,
#'      rename = c("DOB" = "dateOfBirth")
#'    ) %>%
#'    replace_missing_values(target_columns = NULL, na_strings = "-99") %>%
#'    remove_constants(cutoff = 1.0) %>%
#'    remove_duplicates(target_columns = NULL) %>%
#'    standardize_dates(
#'      target_columns = NULL,
#'      error_tolerance = 0.4,
#'      format = NULL,
#'      timeframe = as.Date(c("1973-05-29", "2023-05-29"))
#'    ) %>%
#'    check_subject_ids(
#'      target_columns = "study_id",
#'      prefix = "PS",
#'      suffix = "P2",
#'      range = c(1L, 100L),
#'      nchar = 7L
#'    ) %>%
#'    convert_to_numeric(target_columns = "sex", lang = "en") %>%
#'    clean_using_dictionary(dictionary = test_dictionary)
#'
#'   # add the data scanning result to the report
#'   cleaned_data <- add_to_report(
#'     x = cleaned_data,
#'     key = "scanning_result",
#'     value = scan_res
#'   )
#'
#'   # save the report in the R temporary directory
#'   print_report(
#'     data = cleaned_data,
#'     report_title = "{cleanepi} data cleaning report",
#'     output_file_name = NULL,
#'     format = "html",
#'     print = FALSE
#'   )
#' }
#'
#' @export
#' @importFrom utils browseURL
print_report <- function(data,
                         what = NULL,
                         print = FALSE,
                         report_title = "{cleanepi} data cleaning report",
                         output_file_name = NULL,
                         format = "html") {

  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_character(report_title, null.ok = FALSE,
                              any.missing = FALSE, len = 1L)
  checkmate::assert_character(output_file_name, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_choice(format, choices = "html", null.ok = FALSE)
  checkmate::assert_logical(print, any.missing = FALSE, len = 1,
                            null.ok = FALSE)
  checkmate::assert_choice(
    what, null.ok = TRUE,
    choices = c("incorrect_date_sequence", "colnames", "converted_into_numeric",
                "date_standardization", "misspelled_values",
                "removed_duplicates", "found_duplicates", "constant_data",
                "missing_values_replaced_at", "incorrect_subject_id",
                "scanning_result")
  )

  if (!requireNamespace("reactable", quietly = TRUE)) {
    cli::cli_abort(c(
      x = tr_("The {.pkg reactable} package is required for printing the report.")  # nolint: line_length_linter
    ))
  }

  # extract report, check whether any cleaning operation has been performed, and
  # allow for only HTML output format for the report.
  report <- attr(data, "report")
  if (is.null(report)) {
    cli::cli_abort(c(
      tr_("No report associated with the input data."),
      x = tr_("At least one data cleaning operation must be applied to the data before calling {.fn print_report}."), # nolint: line_length_linter
      i = tr_("The list of functions in {.pkg cleanepi} can be found at: {.url https://epiverse-trace.github.io/cleanepi/reference/index.html}.") # nolint: line_length_linter
    ), call = NULL)
  }
  if (format != "html") {
    cli::cli_abort(c(
      tr_("Incorrect value provided for {.emph format} argument!"),
      i = tr_("Only {.val html} format is currently supported.")
    ), call = NULL)
  }

  # set the report from scan_data() function to NA if no data scanning was
  # performed. This is because the function returns NA if no character column
  # was found in the input data
  if (!("scanning_result" %in% names(report))) {
    report[["scanning_result"]] <- NA
  }

  # only display the report from the specified cleaning operation in the
  # `operation` argument.
  if (!is.null(what)) {
    return(attr(data, "report")[[what]])
  }

  # generate output file and directory
  timestamp_string <- format(Sys.time(), "_%Y-%m-%d%_at_%H%M%S")
  if (is.null(output_file_name)) {
    output_file_name <- paste0("cleanepi_report_", timestamp_string, ".html")
  }

  # this ensures to add the logo to the report
  report[["report_title"]] <- report_title

  # render the Rmd file to generate the report
  temp_dir <- tempdir()
  file_and_path <- file.path(temp_dir, output_file_name)
  cli::cli_alert_info(
    tr_("Generating html report in {.file {temp_dir}}.")
  )

  # deduplicate the report
  report <- report[!duplicated(report)]

  # unnest date standardisation report
  report <- unnest_report(
    report,
    what = "date_standardization",
    "multi_format_dates", "out_of_range_dates"
  )

  # unnest duplicates finding report
  report <- unnest_report(
    report = report,
    what = "found_duplicates",
    "duplicated_rows", "duplicates_checked_from"
  )

  # unnest subject IDs checks report
  report <- unnest_report(
    report,
    what = "incorrect_subject_id",
    "idx_missing_ids", "duplicated_ids", "invalid_subject_ids"
  )

  # render the report
  rmarkdown::render(
    input = system.file(
      "rmarkdown", "templates", "printing-rmd", "skeleton", "skeleton.Rmd",
      package = "cleanepi",
      mustWork = TRUE
    ),
    output_file = file_and_path,
    params = report,
    quiet = TRUE
  )

  # print report if specified
  if (print) {
    utils::browseURL(file_and_path)
  }
  return(file_and_path)
}

#' Unnest an element of the data cleaning report
#'
#' @param report An object of type \code{<list>}
#' @inheritParams print_report
#' @param ... Any other extra argument
#'
#' @return The input object where the specified element has been unnested and
#'    removed.
#' @keywords internal
unnest_report <- function(report, what, ...) {
  checkmate::assert_list(report, min.len = 1)
  # get the extra argument
  extra_args <- list(...)

  if (what %in% names(report)) {
    target <- report[[what]]
    for (arg in extra_args) {
      if (arg %in% names(target)) {
        report[[arg]] <- target[[arg]]
      }
    }
    report[[what]] <- NULL
  }

  return(report)
}
