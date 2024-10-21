#' Standardize date variables
#'
#' When the format of the values in a column and/or the target columns are not
#' defined, we strongly recommend checking a few converted dates manually to
#' make sure that the dates extracted from a `character` vector or a `factor`
#' are correct.\cr
#'
#' Check for the presence of date values that could have multiple formats
#' from the \code{$multi_format_dates} element of the \code{report}.\cr
#'
#' @param data A data frame or linelist
#' @param target_columns A vector of the target date column names. When the
#'    input data is a \code{linelist} object, this parameter can be set to
#'    \code{linelist_tags} if you wish to standardize the date columns across
#'    tagged columns only. Default is \code{NULL}.
#' @param format A vector of the expected formats in the date values from the
#'    date columns. Default is \code{NULL}.
#' @param timeframe A vector of 2 values of type date. If provided, date values
#'    that do not fall within this timeframe will be set to `NA`.
#' @param error_tolerance A number between 0 and 1 indicating the proportion of
#'    entries which cannot be identified as dates to be tolerated; if this
#'    proportion is exceeded, the original vector is returned, and a message is
#'    issued; defaults to 0.4 (40 percent).
#' @param orders A list or character vector with the date codes for fine-grained
#'    parsing of dates. This allows for parsing of mixed dates. If a list is
#'    supplied, that list will be used for successive tries in parsing. When
#'    this is not provided (\code{orders = NULL}), the function will use the
#'    following order defined in the guesser:
#'
#' ```
#' list(
#'   quarter_partial_dates = c("Y", "Ym", "Yq"),
#'   world_digit_months = c("Yq", "ymd", "ydm", "dmy", "mdy", "myd", "dym",
#'                          "Ymd", "Ydm", "dmY", "mdY", "mYd", "dYm"),
#'   world_named_months = c("dby", "dyb", "bdy", "byd", "ybd", "ydb",
#'                          "dbY", "dYb", "bdY", "bYd", "Ybd", "Ydb"),
#'   us_format = c("Omdy", "YOmd")
#' )
#' ```
#'
#' @returns The input dataset where the date columns have been standardized. The
#'    date values that are out of the specified timeframe will be reported in
#'    the report. Similarly, date values that comply with multiple formats will
#'    also be featured in the report object.
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
#' x <- c("03 Jan 2018", "07/03/1982", "08/20/85")
#' # The below will coerce values where the month is written in letters only
#' # into Date.
#' as.Date(lubridate::parse_date_time(x, orders = c("Ybd", "dby")))
#'
#' # coerce values where the month is written in letters or numbers into Date.
#' as.Date(lubridate::parse_date_time(x, orders = c("dmy", "Ymd")))
#'
#' # How to use standardize_dates()
#' dat <- standardize_dates(
#'   data = readRDS(
#'     system.file("extdata", "test_df.RDS", package = "cleanepi")
#'   ),
#'   target_columns = "date_first_pcr_positive_test",
#'   format = NULL,
#'   timeframe = NULL,
#'   error_tolerance = 0.4,
#'   orders = list(
#'     world_named_months = c("Ybd", "dby"),
#'     world_digit_months = c("dmy", "Ymd"),
#'     US_format = c("Omdy", "YOmd")
#'   )
#' )
standardize_dates <- function(data,
                              target_columns  = NULL,
                              format          = NULL,
                              timeframe       = NULL,
                              error_tolerance = 0.5,
                              orders          = NULL) {

  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  checkmate::assert_character(target_columns, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_character(format, null.ok = TRUE, any.missing = FALSE)
  checkmate::assert_date(timeframe, any.missing = FALSE,
                         null.ok = TRUE, len = 2L, unique = TRUE)
  checkmate::assert_numeric(error_tolerance, lower = 0L, upper = 1L,
                            max.len = 2L,
                            any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_list(orders, min.len = 1L, null.ok = TRUE)

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
        data[[target_columns[i]]] <- as.Date(
          data[[target_columns[i]]],
          format = format[i]
        )
        # check for outliers and set them to NA
        data <- date_convert(
          data,
          cols = target_columns[i],
          error_tolerance,
          timeframe = timeframe,
          orders = orders
        )
      }
    } else {
      for (cols in target_columns) {
        # convert to ISO8601 date using the inferred format
        data <- date_convert(data, cols, error_tolerance, timeframe,
                             orders = orders)
      }
    }
  } else {
    data <- date_guess_convert(
      data,
      error_tolerance = error_tolerance,
      timeframe = timeframe,
      orders = orders
    )
  }

  return(data)
}
