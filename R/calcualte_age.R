#' Calculate age from a specified date column
#'
#' @param data A data frame with at least one date column.
#' @param target_column A string specifying the name of the date column of
#'    interest.
#' @param end_date An end date. This should be in the format
#'    `%Y-%m-%d` ("2024-12-31" for 31st of December 2024). Otherwise, use the
#'    `as.Date()` function and specify the format of the end date. The default
#'    is today's date `Sys.Date()`.
#' @param age_in A string that specifies whether to return the age in 'years',
#'    'months', 'weeks' or 'days'. The default is in 'years'.
#' @param age_column_name A string for the name of the new column to be used to
#'    store the calculated age in the input data.frame.
#' @param age_remainder_unit A string for the unit in which the remainder of the
#'    age should be calculated. May be one of "digits", "months", "weeks", and
#'    "days". Remainders requested in the same unit as the age will return
#'    values of 0. Default is "digits" i.e. decimal age.
#'
#' @return The input data frame with 2 additional columns:
#' \enumerate{
#'   \item "age_in_years",  "age_in_months",  "age_in_weeks", or
#'         "age_in_days" depending on the value of the 'age_in' parameter.
#'   \item "remainder_*": A column with the number of the remaining
#'         days (remainder_days) or weeks (remainder_weeks) or months
#'         (remainder_months) depending on the value of the 'age_remainder_unit'
#'         parameter.
#'   }
#' @export
#'
#' @examples
#' data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
#'
#' # The 'dateOfBirth' column contains missing values recoded as "-99". let's
#' # replace them with NA
#' data <- replace_missing_values(data           = data,
#'                                target_columns = "dateOfBirth",
#'                                na_strings     = "-99")
#'
#' # the calculate_age() function expects the date values to be given in ISO.
#' # Let's convert the 'dateOfBirth' column into ISO date
#' data <- standardize_dates(data            = data,
#'                           target_columns  = "dateOfBirth",
#'                           error_tolerance = 0.0)
#'
#' # calculate the age 'years' and return the remainder in 'months'
#' age <- calculate_age(
#'   data               = data,
#'   target_column      = "dateOfBirth",
#'   end_date           = Sys.Date(),
#'   age_in             = "years",
#'   age_remainder_unit = "months"
#' )
#'
#' # The operations above can be written in a pipe-formatted way:
#' data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
#' age  <- data |>
#'   replace_missing_values(target_columns = "dateOfBirth",
#'                          na_strings     = "-99") |>
#'   standardize_dates(target_columns  = "dateOfBirth",
#'                     error_tolerance = 0.0) |>
#'   calculate_age(target_column      = "dateOfBirth",
#'                 end_date           = Sys.Date(),
#'                 age_in             = "years",
#'                 age_remainder_unit = "months")
#'
calculate_age <- function(data,
                          target_column      = NULL,
                          end_date           = Sys.Date(),
                          age_in             = "years",
                          age_column_name    = sprintf("age_in_%s", age_in),
                          age_remainder_unit = "digits") {
  checkmate::assert_data_frame(data, null.ok = FALSE)

  # check age_in, multiple choices not allowed by default
  # changed to give visibility of choices in function call in docs
  checkmate::assert_choice(age_in,
                           choices = c("years", "months", "weeks", "days"),
                           null.ok = FALSE)

  checkmate::assert_choice(target_column,
    choices = colnames(data),
    null.ok = TRUE
  )
  end_date <- checkmate::assert_date(
    end_date,
    any.missing = FALSE, len = 1L, null.ok = TRUE
  )
  checkmate::assert_string(age_column_name)

  # check age remainder and match to options
  # NOTE: "year" is not an option, but a remainder in "weeks" could be specified
  # for `age_in = "weeks"` - expect user will make common sense decisions
  checkmate::assert_choice(age_remainder_unit,
                           choices = c("digits", "months", "weeks", "days"),
                           null.ok = FALSE)

  # get age remainder column name
  age_remainder_colname <- sprintf("remainder_%s", age_remainder_unit)

  # switch divisor based on requested unit
  # NOTE: no default case defined, add a default case?
  divisor_age <- switch(age_in,
    years  = lubridate::years(1L),
    months = base::months(1L), # lubridate provides method returning period
    weeks  = lubridate::weeks(1L),
    days   = lubridate::days(1) # not really necessary(difftime will be in days)
  )

  # switch divisor for remainder based on requested unit
  divisor_remainder <- switch(age_remainder_unit,
    months = base::months(1L), # from base as above
    weeks  = lubridate::weeks(1L),
    days   = lubridate::days(1)
  )

  if (age_remainder_unit == "digits") {
    # decimal age is accounting for leap years where a year should be 365.25
    # days long, and 0.25 days is 6 hours
    decimal_age <- switch(age_in,
      years   = lubridate::interval(start = data[[target_column]], end = end_date) / (lubridate::days(365L) + lubridate::hours(6L)), # nolint: line_length_linters
      months = lubridate::interval(start = data[[target_column]], end = end_date) / months(1L), # nolint: line_length_linters
      weeks  = lubridate::interval(start = data[[target_column]], end = end_date) / lubridate::weeks(1L), # nolint: line_length_linters
      days   = lubridate::interval(start = data[[target_column]], end = end_date) / lubridate::days(1L) # nolint: line_length_linters
    )
    data[, age_column_name] <- round(decimal_age, digits = 2L)
  } else {
    # calculate the time difference, convert to a period, and get the quotient
    # and remainder
    time_diff         <- lubridate::as.period(end_date - data[[target_column]])
    data[, age_column_name]       <- time_diff %/% divisor_age
    data[, age_remainder_colname] <- (time_diff %% divisor_age) %/%
      divisor_remainder
  }

  # when the age is requested in days, remove remainder
  if (age_in == "days") {
    data[, age_remainder_colname] <- NULL
  }

  return(data)
}
