
#' Print the report from the data cleaning operations
#'
#' @param report the report object
#' @export
#'
print_report <- function(report) {
  checkmate::assert_list(report, min.len = 1L, null.ok = FALSE)
  tmp_input  <- file.path(.libPaths(), "cleanepi",
                          "rmarkdown", "templates", "test_printing-rmd",
                          "skeleton", "skeleton.Rmd")
  output_dir <- getwd()
  tmp_output <- file.path(output_dir,
                          "test_printing.html")
  rmarkdown::render(input         = tmp_input,
                    params        = report,
                    output_dir    = output_dir,
                    output_file   = tmp_output,
                    output_format = NULL)
  browseURL(tmp_output)
}