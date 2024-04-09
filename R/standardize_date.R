#' Standardize date variables
#'
#' When the format of the values in a column is not provided, we strongly
#' recommend checking a few converted dates manually. This function tries to
#' extract dates from a `character` vector or a `factor`.
#'
#' @param data A data frame or linelist
#' @param target_columns A vector of the target date column names. When the
#'    input data is a `linelist` object, this parameter can be set to
#'    `linelist_tags` if you wish to standardize the date columns across tagged
#'    columns only.
#' @param format A format of the date values in the date columns
#' @param timeframe A vector of 2 values of type date. If provided, date values
#'    that do not fall within this timeframe will be set to `NA`.
#' @param error_tolerance A number between 0 and 1 indicating the proportion of
#'    entries which cannot be identified as dates to be tolerated; if this
#'    proportion is exceeded, the original vector is returned, and a message is
#'    issued; defaults to 0.4 (40 percent).
#' @param orders The date codes for fine-grained parsing of dates. This allows
#'    for parsing of mixed dates. If a list is supplied, that list will be used
#'    for successive tries in parsing. This is passed on to
#'    [lubridate::parse_date_time()]. Default orders are:
#'
#' ```
#' list(
#'   world_named_months = c("Ybd", "dby"),
#'   world_digit_months = c("dmy", "Ymd"),
#'   US_formats         = c("Omdy", "YOmd")
#' )
#' ```
#' @param modern_excel When parsing dates from excel, some dates are stored as
#'    integers. Modern versions of Excel represent dates as the number of days
#'    since 1900-01-01, but pre-2011 Excel for OSX have the origin set at
#'    1904-01-01. If this parameter is `TRUE` (default), then this assumes that
#'    all numeric values represent dates from either a Windows version of Excel
#'    or a 2011 or later version of Excel for OSX. Set this parameter to `FALSE`
#'    if the data came from an OSX version of Excel before 2011.
#'
#' @returns The input dataset where the date columns have been standardized
#' @export
#'
#' @details Converting ambiguous character strings to dates is difficult for
#'     many reasons:
#'
#' - dates may not use the standard Ymd format
#'
#' - within the same variable, dates may follow different formats
#'
#' - dates may be mixed with things that are not dates
#'
#' - the behavior of `as.Date` in the presence of non-date is hard to predict,
#'   sometimes returning `NA`, sometimes issuing an error.
#'
#' This function tries to address all the above issues. Dates with the following
#' format should be automatically detected, irrespective of separators
#' (e.g. "-", " ", "/") and surrounding text:
#'
#' - "19 09 2018"
#' - "2018 09 19"
#' - "19 Sep 2018"
#' - "2018 Sep 19"
#' - "Sep 19 2018"
#'
#' \subsection{How it works}{
#'
#' This function relies heavily on [lubridate::parse_date_time()], which is an
#' extremely flexible date parser that works well for consistent date formats,
#' but can quickly become unwieldy and may produce spurious results.
#' `standardize_dates()` will use a list of formats in the `orders` argument to
#' run `parse_date_time()` with each format vector separately and take the first
#' correctly parsed date from all the trials.
#'
#' With the default orders shown above, the dates 03 Jan 2018, 07/03/1982, and
#' 08/20/85 are correctly interpreted as 2018-01-03, 1982-03-07, and 1985-08-20.
#' The examples section will show how you can manipulate the `orders` to be
#' customized for your situation.
#' }
#'
#' @examples
#' dat <- standardize_dates(
#'   data            = readRDS(system.file("extdata", "test_df.RDS",
#'                                         package = "cleanepi")),
#'   target_columns  = "date_first_pcr_positive_test",
#'   format          = NULL,
#'   timeframe       = NULL,
#'   error_tolerance = 0.4,
#'   orders          = list(world_named_months = c("Ybd", "dby"),
#'                          world_digit_months = c("dmy", "Ymd"),
#'                          US_formats         = c("Omdy", "YOmd")),
#'   modern_excel    = TRUE
#' )
standardize_dates <- function(data,
                              target_columns  = NULL,
                              format          = NULL,
                              timeframe       = NULL,
                              error_tolerance = 0.5,
                              orders          = list(
                                world_named_months = c("Ybd", "dby"),
                                world_digit_months = c("dmy", "Ymd"),
                                US_formats         = c("Omdy", "YOmd")
                              ),
                              modern_excel    = TRUE) {

  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  checkmate::assert_character(target_columns, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_character(format, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_date(timeframe, any.missing = FALSE,
                         null.ok = TRUE, len = 2L, unique = TRUE)
  checkmate::assert_numeric(error_tolerance, lower = 0L, upper = 1L,
                            max.len = 2L,
                            any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_list(orders, min.len = 1L, null.ok = FALSE)
  checkmate::assert_logical(modern_excel, len = 1L, null.ok = FALSE,
                            any.missing = FALSE)

  if (!is.null(target_columns)) {
    # get the correct names in case some have been modified - see the
    # `retrieve_column_names()` function for more details
    target_columns <- retrieve_column_names(data, target_columns)
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
                             timeframe = timeframe, orders = orders,
                             modern_excel = modern_excel)
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
        }
        data <- date_convert(data, cols, error_tolerance, timeframe,
                             orders = orders, modern_excel = modern_excel)
      }
    }
  } else {
    data     <- date_guess_convert(data,
                                   error_tolerance = error_tolerance,
                                   timeframe       = timeframe,
                                   orders          = orders,
                                   modern_excel    = modern_excel)
  }

  return(data)
}
