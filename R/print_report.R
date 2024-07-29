#' Generate report from data cleaning operations
#'
#' @param data A `data.frame` or `linelist` object returned from the
#'    [clean_data()] or the main functions of each data cleaning module.
#' @param report_title The title to appear on the report
#' @param output_file_name A string specifying the name of the report file,
#'    excluding any file extension. If no file name is supplied, one will be
#'    automatically generated with the format `cleanepi_report_YYMMDD_HHMMSS`.
#' @param format The file format of the report. Currently only `"html"`
#'    is supported.
#' @param print A logical that specifies whether to print the generated HTML
#'    file or no.
#'
#' @returns A string containing the name and path of the saved report
#' @examples
#' \donttest{
#' data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
#' test_dictionary <- readRDS(system.file("extdata", "test_dictionary.RDS",
#'                                        package = "cleanepi"))
#'
#' # scan through the data
#' scan_res <- scan_data(data)
#'
#' # Perform data cleaning
#' cleaned_data <- data %>%
#'  standardize_column_names(keep = NULL, rename = c("DOB" = "dateOfBirth")) %>%
#'  replace_missing_values(target_columns = NULL, na_strings = "-99") %>%
#'  remove_constants(cutoff = 1.0) %>%
#'  remove_duplicates(target_columns = NULL) %>%
#'  standardize_dates(target_columns  = NULL,
#'                    error_tolerance = 0.4,
#'                    format          = NULL,
#'                    timeframe   = as.Date(c("1973-05-29", "2023-05-29"))) %>%
#'  check_subject_ids(target_columns = "study_id",
#'                    prefix         = "PS",
#'                    suffix         = "P2",
#'                    range          = c(1L, 100L),
#'                    nchar          = 7L) %>%
#'  convert_to_numeric(target_columns = "sex", lang = "en") %>%
#'  clean_using_dictionary(dictionary = test_dictionary)
#'
#' # add the data scanning result to the report
#' cleaned_data <- add_to_report(x     = cleaned_data,
#'                               key   = "scanning_result",
#'                               value = scan_res)
#'
#' # save a report in the current directory using the previously-created objects
#' print_report(
#'   data             = cleaned_data,
#'   report_title     = "{cleanepi} data cleaning report",
#'   output_file_name = NULL,
#'   format           = "html",
#'   print            = TRUE
#' )
#' }
#'
#' @export
#' @importFrom utils browseURL
print_report <- function(data,
                         report_title     = "{cleanepi} data cleaning report",
                         output_file_name = NULL,
                         format           = "html",
                         print            = TRUE) {

  # extract report and check whether any cleaning operation has been performed
  report             <- attr(data, "report")
  stopifnot("No report associated with the input data." = !is.null(report))
  stopifnot("Invalid format: Only 'html' format is currently supported." =
              format == "html")

  # generate output file and directory
  timestamp_string   <- format(Sys.time(), "_%Y-%m-%d%_at_%H%M%S")
  if (is.null(output_file_name)) {
    output_file_name <- paste0("cleanepi_report_", timestamp_string, ".html")
  }

  # this ensures to add the logo to the report
  report[["report_title"]] <- report_title
  man_path                 <- file.path("man", "figures")
  report[["logo"]]         <- system.file(man_path, "logo.svg",
                                          package = "cleanepi")

  file_and_path <- file.path(tempdir(), output_file_name)
  message("Generating html report in ", tempdir())
  rmarkdown::render(
    input       = system.file("rmarkdown", "templates", "printing-rmd",
                              "skeleton", "skeleton.Rmd",
                              package  = "cleanepi",
                              mustWork = TRUE),
    output_file = file_and_path,
    params      = report,
    quiet       = TRUE
  )

  # print report if specified
  if (print) {
    utils::browseURL(file_and_path)
  }
  return(file_and_path)
}
