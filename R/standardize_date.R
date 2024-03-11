#' Standardize date variables
#'
#' @param data A data frame or linelist
#' @param target_columns A vector or a comma-separated list of the target date
#'    column names. When the input data is a `linelist` object, this parameter
#'    can be set to `linelist_tags` if you wish to standardize the date columns
#'    across tagged columns only.
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
    if (is.character(target_columns)) {
      target_columns <- unlist(strsplit(target_columns, ",", fixed = TRUE))
      target_columns <- trimws(target_columns)
    }
    target_columns <- get_target_column_names(data, target_columns, cols = NULL)

    if (!is.null(format)) {
      # we assume that when the format is provided, all values in that column
      # have the same format. Date standardization will be performed considering
      # the entire column, not individual values.
      format <- date_match_format_and_column(target_columns, format)
      for (i in seq_along(target_columns)) {
        data[[target_columns[i]]] <- as.Date(data[[target_columns[i]]],
                                             format = format[i])
        # check for outliers and set them to NA
        data <- date_convert(data, target_columns[i], error_tolerance,
                             timeframe = timeframe)
      }
    } else {
      for (cols in target_columns) {
        sep <- unique(as.character(unlist(lapply(data[[cols]],
                                                 date_detect_separator))))
        # Guess the date format if it is not provided.
        # This returns NULL if the format is not resolved.
        if (length(sep) > 0L) {
          format <- date_get_format(data, cols, sep)
        }

        # convert to ISO date
        if (!is.null(format)) {
          data[[cols]] <- as.Date(data[[cols]], format = format)
        } else {
          data <- date_convert(data, cols, error_tolerance, timeframe)
        }

      }
    }
  } else {
    data     <- date_guess_convert(data,
                                   error_tolerance = error_tolerance,
                                   timeframe)
  }

  return(data)
}
