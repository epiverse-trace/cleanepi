#' Calculate age from a specified date column
#'
#' @param data A data frame with at least one date column.
#' @param target_column A string specifying the name of the date column of
#'    interest.
#' @param end_date An end date, the default is today's date.
#' @param age_in A string that specifies whether to return the age in 'years',
#'    'months', 'weeks' or 'days'. The default is in 'years'.
#' @param age_column_name A string for the name of the new column to be used to
#'    store the calculated age in the input data.frame.
#' @param age_remainder_unit A string for the unit in which the remainder of the
#'    age should be calculated. May be one of "months", "weeks", and "days".
#'    Remainders requested in the same unit as the age will return values of 0.
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
#' # calculate the age 'years' and return the remainder in 'months'
#' age <- calculate_age(
#'   data               = readRDS(system.file("extdata", "test_df.RDS",
#'                                            package = "cleanepi")),
#'   target_column      = "dateOfBirth",
#'   end_date           = Sys.Date(),
#'   age_in             = "years",
#'   age_remainder_unit = "months"
#' )
#'
#' # calculate the age in 'months' and return the remainder in 'days'
#' age <- calculate_age(
#'   data               = readRDS(system.file("extdata", "test_df.RDS",
#'                                            package = "cleanepi")),
#'   target_column      = "dateOfBirth",
#'   end_date           = Sys.Date(),
#'   age_in             = "months",
#'   age_remainder_unit = "days"
#' )
calculate_age <- function(data,
                          target_column      = NULL,
                          end_date           = Sys.Date(),
                          age_in             = c("years", "months",
                                                 "weeks", "days"),
                          age_column_name    = sprintf("age_in_%s", age_in),
                          age_remainder_unit = c("months", "weeks", "days")) {
  checkmate::assert_data_frame(data, null.ok = FALSE)

  # check age_in, multiple choices not allowed by default
  # changed to give visibility of choices in function call in docs
  age_in <- match.arg(age_in)

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
  age_remainder_unit <- match.arg(age_remainder_unit)
  # get age remainder column name
  age_remainder_colname <- sprintf("remainder_%s", age_remainder_unit)

  # standardize the input data if required
  # NOTE: define `else` case or check that target column is a `Date`
  if (!lubridate::is.Date(data[[target_column]])) {
    # the error_tolerance = 0.0 because target_column is explicit
    data <- standardize_dates(data,
                              target_column,
                              format          = NULL,
                              timeframe       = NULL,
                              error_tolerance = 0.0)
  }

  # switch divisor based on requested unit
  # NOTE: no default case defined, add a default case?
  divisor_age <- switch(age_in,
    years  = lubridate::years(1L),
    months = months(1L), # from base, lubridate provides method returning period
    weeks  = lubridate::weeks(1L),
    days   = lubridate::days(1) # not really necessary(difftime will be in days)
  )

  # switch divisor for remainder based on requested unit
  divisor_remainder <- switch(age_remainder_unit,
    months = months(1L), # from base as above
    weeks  = lubridate::weeks(1L),
    days   = lubridate::days(1)
  )

  # calculate the time difference, convert to a period, and get the quotient
  # and remainder
  time_diff            <- lubridate::as.period(end_date - data[[target_column]])
  data[, age_column_name]       <- time_diff %/% divisor_age
  data[, age_remainder_colname] <- (time_diff %% divisor_age) %/%
    divisor_remainder

  # when the age is requested in days, remove remainder
  if (age_in == "days") {
    data[, age_remainder_colname] <- NULL
  }

  return(data)
}
