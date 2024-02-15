#' Generate report from data cleaning operations
#'
#' @param data A `data.frame` or `linelist` object returned from the
#'    [clean_data()] or the main functions of each data cleaning module.
#' @param report_title The title to appear on the report
#' @param output_directory A string specifying directory in which to save the
#'    report. Default is current directory.
#' @param output_filename A string specifying filename for the report, excluding
#'    any file extension. If no filename is supplied, one will be automatically
#'    generated with the format `cleanepi_report_YYMMDD_HHMMSS`.
#' @param format The file format of the report. Currently only `"html"`
#'    is supported.
#' @param print A logical that specifies whether to print the generated HTML
#'    file or no.
#'
#' @return A string containing the name and path of the saved report
#' @examples
#' \donttest{
#' replace_missing_values = list(target_columns = NULL, na_strings = "-99")
#'
#' remove_duplicates = list(target_columns   = NULL,
#'                          rm_empty_rows    = TRUE,
#'                          rm_empty_cols    = TRUE,
#'                          rm_constant_cols = TRUE)
#'
#' standardize_date = list(target_columns  = NULL,
#'                         error_tolerance = 0.5,
#'                         format          = NULL,
#'                         timeframe       = as.Date(c("1973-05-29",
#'                                                     "2023-05-29")))
#'
#' standardize_subject_ids = list(id_col_name = "study_id",
#'                                format      = NULL,
#'                                prefix      = "PS",
#'                                suffix      = "P2",
#'                                range       = c(1, 100))
#'
#' # scan through the data
#' scan_res <- scan_data(data = readRDS(system.file("extdata", "test_df.RDS",
#'                                                  package = "cleanepi")))
#'
#' # Perform data cleaning
#' cleaned_data <- clean_data(
#'   data   = readRDS(system.file("extdata", "test_df.RDS",
#'                                package = "cleanepi")),
#'   params = list(
#'     keep                    = NULL,
#'     replace_missing_values  = replace_missing_values,
#'     remove_duplicates       = remove_duplicates,
#'     standardize_date        = standardize_date,
#'     standardize_subject_ids = standardize_subject_ids,
#'     to_numeric              = "sex",
#'     dictionary              = NULL
#'   )
#' )
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
#'   output_directory = ".",
#'   output_filename  = "template_data_report",
#'   format           = "html",
#'   print            = TRUE
#' )
#' \dontshow{file.remove("./template_data_report.html")}
#' }
#'
#' @export
#' @importFrom utils browseURL
print_report <- function(data,
                         report_title     = "{cleanepi} data cleaning report",
                         output_directory = ".",
                         output_filename  = NULL,
                         format           = "html",
                         print            = TRUE) {

  # tmp_input  <- file.path(.libPaths(), "cleanepi", # nolint: undesirable_function_linter
  #                         "rmarkdown", "templates", "test_printing-rmd",
  #                         "skeleton", "skeleton.Rmd")

  # extract report and check whether any cleaning operation has been performed
  report            <- attr(data, "report")
  stopifnot("No report associated with the input data." = !is.null(report))

  # generate output file and directory
  timestamp_string  <- format(Sys.time(), "%Y%m%d%_%H%M%S")
  if (is.null(output_filename)) {
    output_filename <- paste0("cleanepi_report_", timestamp_string)
  }
  file_and_path     <- file.path(output_directory,
                                 paste0(output_filename, ".html"))

  # temporarily copy Rmd file from package library into save_directory so that
  # intermediate files also get created there.
  # NOTE: explicitly setting intermediates_dir in rmarkdown::render() to
  # save_directory or tempdir() causes duplicate chunk label errors when package
  # is run from inside an rmd/qmd
  temp_dirname <- file.path(output_directory,
                            paste0("cleanepi_temp_", timestamp_string))
  dir.create(temp_dirname)
  file.copy(
    # from      = system.file("rmarkdown", "templates", "test_printing-rmd",
    #                         "skeleton", "skeleton.Rmd",
    #                         package  = "cleanepi", #utils::packageName(),
    #                         mustWork = TRUE),
    from      = system.file("rmarkdown", "templates", "test_printing-rmd",
                            "skeleton", "report_htmldoc.Rmd",
                            package  = "cleanepi", #utils::packageName(),
                            mustWork = TRUE),
    to        = temp_dirname,
    overwrite = TRUE
  )

  if (format == "html") {
    message("Generating html report...")
    rmarkdown::render(
      input       = file.path(temp_dirname, "skeleton.Rmd"),
      output_file = paste0(output_filename, ".html"),
      output_dir  = output_directory,
      params      = list(report = report, report_title = report_title),
      quiet       = TRUE
    )
  } else {
    stop(paste("Invalid format: ", format,
               ". Only html format is currently supported."))
  }
  message("Report saved to: ", file_and_path)

  # remove temporary directory created earlier
  unlink(temp_dirname, recursive = TRUE)

  # print report if specified
  if (print) {
    utils::browseURL(file_and_path)
  }

  return(file_and_path)
}
