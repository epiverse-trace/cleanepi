#' Standardize date variables
#'
#' @param data A data frame
#' @param target_columns A vector or a comma-separated list of targeted date
#'    column names.
#' @param format A format of the date values in the date columns
#' @param timeframe A vector of 2 values of type date. If provided, date values
#'    that do not fall within this timeframe will be set to `NA`.
#' @param error_tolerance A numerical value between 0 and 1, indicating the
#'    proportion of entries which cannot be identified as dates to be tolerated.
#'    See the `clean_data()` helper for more details.
#'
#' @returns The input dataset where the date columns have been standardized
#' @export
#'
#' @examples
#' dat <- standardize_dates(
#'   data            = readRDS(system.file("extdata", "test_df.RDS",
#'                                         package = "cleanepi")),
#'   target_columns  = "date_first_pcr_positive_test",
#'   format          = NULL,
#'   timeframe       = NULL,
#'   error_tolerance = 0.5
#' )
standardize_dates <- function(data,
                              target_columns  = NULL,
                              format          = NULL,
                              timeframe       = NULL,
                              error_tolerance = 0.5) {

  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  checkmate::assert_character(target_columns, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_character(format, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_date(timeframe, any.missing = FALSE,
                         null.ok = TRUE, len = 2L, unique = TRUE)
  checkmate::assert_numeric(error_tolerance, lower = 0L, upper = 1L,
                            max.len = 2L,
                            any.missing = FALSE, null.ok = TRUE)

  if (!is.null(target_columns)) {
    # check input data format
    target_columns <- date_check_column_existence(data, target_columns)
    for (cols in target_columns) {
      sep         <- unique(as.character(unlist(lapply(data[[cols]],
                                                       date_detect_separator))))
      # Guess the date format if it is not provided.
      # This returns NULL if the format is not resolved.
      if (length(sep) > 0L && is.null(format)) {
        result     <- date_get_format(data, cols, sep)
        format     <- result[["format"]]
        others     <- result[["others"]]
      }
      data         <- date_convert(data, cols, error_tolerance,
                                   format, timeframe, others)
    }
  } else {
    data           <- date_guess_convert(data,
                                         error_tolerance = error_tolerance,
                                         timeframe)
  }

  return(data)
}
