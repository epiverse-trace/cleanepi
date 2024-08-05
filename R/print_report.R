print_report <- function(data,
                         report_title     = "{cleanepi} data cleaning report",
                         output_directory = ".",
                         output_filename  = NULL,
                         format           = "html",
                         print            = TRUE) {

  # extract report and check whether any cleaning operation has been performed
  report            <- attr(data, "report")
  stopifnot("No report associated with the input data." = !is.null(report))

  # generate output file and directory
  timestamp_string  <- format(Sys.time(), "_%Y-%m-%d%_at_%H-%M-%S")
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
  # consider using 'utils::packageName()' to specify the package name
  file.copy(
    from      = system.file("rmarkdown", "templates", "test_printing-rmd",
                            "skeleton", "report_htmldoc.Rmd",
                            package  = "cleanepi",
                            mustWork = TRUE),
    to        = temp_dirname,
    overwrite = TRUE
  )
  report[["report_title"]] <- report_title
  man_path                 <- file.path("man", "figures")
  report[["logo"]]         <- system.file(man_path, "logo.svg",
                                          package = "cleanepi")

  if (format == "html") {
    message("Generating html report...")
    rmarkdown::render(
      input       = file.path(temp_dirname, "report_htmldoc.Rmd"),
      output_file = paste0(output_filename, ".html"),
      output_dir  = output_directory,
      params      = report,
      quiet       = TRUE
    )
  } else {
    stop("Invalid format: ", format, "\n",
         "Only html format is currently supported.")
  }
  message("Report saved to:\n", file_and_path)

  # remove temporary directory created earlier
  unlink(temp_dirname, recursive = TRUE)

  # print report if specified
  if (print) {
    utils::browseURL(file_and_path)
  }

  return(file_and_path)
}
