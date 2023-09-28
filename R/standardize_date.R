#' Standardize date variables
#'
#' @param data the input data frame
#' @param date_column_name the name of the date column of interest.
#'    default: 'Date', or 'DATE', or 'date'
#' @param format the format of the date values in the date column
#' @param check_timeframe a logical to check whether dates fall within timeframe
#' @param timeframe the expected first and last date. See the `clean_data()`
#'    helper for more details
#' @param report the object that will contains details about the result from the
#'    date columns standardization
#' @param error_tolerance a number between 0 and 1 indicating the proportion of
#'     entries which cannot be identified as dates to be tolerated. See the
#'     `clean_data()` helper for more details
#'
#' @returns a `list` of 2 elements: the input dataset where the date columns
#'    have been standardized and a report object of type `list` that contains
#'    the details about the columns that have been modified.
#'
#' @export
#' @examples
#' dat <- standardize_date(
#' data = readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")),
#' date_column_name = "date_first_pcr_positive_test",
#' format = NULL,
#' timeframe = NULL,
#' check_timeframe = FALSE,
#' report = list(),
#' error_tolerance = 0.5
#' )
standardize_date <- function(data, date_column_name = NULL, format = NULL,
                             timeframe = NULL, check_timeframe = FALSE,
                             report = list(), error_tolerance = 0.5) {
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  checkmate::assert_character(date_column_name, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_character(format, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_logical(check_timeframe, any.missing = FALSE,
                            null.ok = FALSE, len = 1L)
  checkmate::assert_date(timeframe, any.missing = FALSE,
                         null.ok = TRUE, len = 2L, unique = TRUE)
  checkmate::assert_numeric(error_tolerance, lower = 0L, upper = 1L,
                            max.len = 2L,
                            any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_list(report, any.missing = FALSE, null.ok = FALSE)

  if (!is.null(date_column_name)) {
    # check input data format
    date_column_name <- check_column_existence(data, date_column_name)

    # standardize it
    report[["standardize_date"]] <- glue::glue_collapse(date_column_name,
                                                        sep = ", ")
    for (cols in date_column_name) {
      sep <- unique(as.character(unlist(lapply(data[[cols]],
                                               detect_date_separator))))
      if (is.null(format)) {
        data <- convert_to_date(data, cols, sep, error_tolerance)
      }
    }
  } else {
    tmp_res <- date_guess_convert(data, error_tolerance = error_tolerance,
                                  timeframe, check_timeframe, report)
    data    <- tmp_res[[1L]]
    report  <- tmp_res[[2L]]
  }

  list(
    data   = data,
    report = report
  )
}
