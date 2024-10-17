#' Calculate time span between dates
#'
#' @param data The input data frame or linelist
#' @param target_column A string used to specify the name of the date column of
#'    interest. The values in this column should be of type 'Date' in ISO8601
#'    format ("2024-01-31").
#' @param end_date An end date. It can be either a character that is the name of
#'    another column of type 'Date' from the input data or a vector of Dates or
#'    a single Date value. This should also be in the ISO8601 format
#'    ("2024-01-31"). Default is today's date \code{Sys.Date()}.
#' @param span_unit A string that specifies the units in which the time span
#'    between the dates will be returned. The possible units are: 'years',
#'    'months', 'weeks' or 'days'.
#' @param span_column_name A string for the name of the new column to be used to
#'    store the calculated time span in the input data frame.
#' @param span_remainder_unit A string for the unit in which the remainder of
#'    the time span should be calculated. May be one of "months", "weeks",
#'    and "days". Remainders requested in the same unit as the age will return
#'    values of 0. Default is \code{NULL} for decimal time span.
#'
#' @returns The input data frame with one or two additional columns:
#' \enumerate{
#'   \item "span" or any other name chosen by the user. This will contain the
#'      calculated time span in the desired units.
#'   \item "*_remainder": a column with the number of the remaining
#'         days or weeks or months depending on the value of the
#'         'span_remainder_unit' parameter. Here '*' represents the value of the
#'         'span_column_name' argument.
#'   }
#' @export
#'
#' @examples
#' # In the below example, this function is used to calculate patient's age from
#' # their dates of birth
#'
#' # import the data, replace missing values with NA and convert date into ISO
#' # format
#' data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
#' data <- data %>%
#'   replace_missing_values(target_columns = "dateOfBirth",
#'                          na_strings = "-99") %>%
#'   standardize_dates(target_columns = "dateOfBirth",
#'                     error_tolerance = 0.0)
#'
#' # calculate the age in 'years' and return the remainder in 'months'
#' age <- timespan(
#'   data = data,
#'   target_column = "dateOfBirth",
#'   end_date = Sys.Date(),
#'   span_unit = "years",
#'   span_column_name = "age_in_years",
#'   span_remainder_unit = "months"
#' )
timespan <- function(data,
                     target_column = NULL,
                     end_date = Sys.Date(),
                     span_unit = c("years", "months", "weeks", "days"),
                     span_column_name = "span",
                     span_remainder_unit = NULL) {
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_choice(target_column,
                           choices = colnames(data),
                           null.ok = TRUE)
  if (!inherits(end_date, "Date")) {
    checkmate::check_character(end_date, min.len = 1L, null.ok = FALSE)
  }
  checkmate::assert_character(span_column_name, len = 1L, null.ok = FALSE,
                              any.missing = FALSE)
  span_unit           <- match.arg(span_unit)
  checkmate::assert_choice(span_remainder_unit,
                           choices = c("months", "weeks", "days"),
                           null.ok = TRUE)

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_column <- retrieve_column_names(data, target_column)
  target_column <- get_target_column_names(data, target_column, cols = NULL)

  # end_date can be a column of the input data or
  # a vector of Date values with the same length as number of row in data or
  # a Date value
  if (is.character(end_date)) {
    stopifnot("The columns specified in 'end_date' argument should be of type
              Date in ISO8601 format." = end_date %in% colnames(data),
              inherits(data[[end_date]], "Date"))
    end_date <- data[[end_date]]
  }

  # switch divisor based on requested unit
  # NOTE: default divisor unit is 'years'
  divisor_age <- switch(
    span_unit,
    years  = lubridate::years(1L),
    months = months(1L), # from base, lubridate provides method returning period
    weeks  = lubridate::weeks(1L),
    days   = lubridate::days(1)
  )

  # calculate the time difference, convert to numeric or period, and get the
  # quotient and remainder
  time_diff <- lubridate::as.period(end_date - data[[target_column]])
  if (is.null(span_remainder_unit)) {
    data[, span_column_name] <- lubridate::time_length(time_diff,
                                                       unit = span_unit)
  } else {
    data[, span_column_name] <- time_diff %/% divisor_age

    # switch divisor for remainder based on requested unit
    divisor_remainder        <- switch(
      span_remainder_unit,
      months = months(1L), # from base as above
      weeks  = lubridate::weeks(1L),
      days   = lubridate::days(1)
    )
    data[, sprintf("remainder_%s", span_remainder_unit)] <-
      (time_diff %% divisor_age) %/% divisor_remainder
  }

  # when the time span is requested in days, remove remainder
  if (span_unit == "days") {
    data[, sprintf("remainder_%s", span_remainder_unit)] <- NULL
  }

  return(data)
}
