#' Calculate time span between dates
#'
#' @param data The input data frame or linelist
#' @param target_column A string used to specify the name of the date column of
#'    interest. The values in this columns should be of type 'Date' in ISO
#'    format ("2024-01-31").
#' @param end_date An end date. It can be either a character that is the name of
#'    another column of type 'Date' from the input data or a vector of Dates or
#'    a single Date value. This should also be in the ISO format ("2024-01-31").
#'    Default is today's date `Sys.Date()`.
#' @param span_unit A string that specifies the units in which the spanning time
#'    between the dates will be returned. The possible units are: 'years',
#'    'months', 'weeks' or 'days'.
#' @param span_column_name A string for the name of the new column to be used to
#'    store the calculated spanning time in the input data frame.
#' @param span_remainder_unit A string for the unit in which the remainder of
#'    the spanning time should be calculated. May be one of "months", "weeks",
#'    and "days". Remainders requested in the same unit as the age will return
#'    values of 0. Default is NULL for decimal spanning time.
#'
#' @return The input data frame with one or two additional columns:
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
#' data <- data |>
#'   replace_missing_values(target_columns = "dateOfBirth",
#'                          na_strings     = "-99") |>
#'   standardize_dates(target_columns  = "dateOfBirth",
#'                     error_tolerance = 0.0)
#'
#' # calculate the age in 'years' and return the remainder in 'months'
#' age <- span(
#'   data                = data,
#'   target_column       = "dateOfBirth",
#'   end_date            = Sys.Date(),
#'   span_unit           = "years",
#'   span_column_name    = "age_in_years",
#'   span_remainder_unit = "months"
#' )
span <- function(data,
                 target_column       = NULL,
                 end_date            = Sys.Date(),
                 span_unit           = c("years", "months", "weeks", "days"),
                 span_column_name    = "span",
                 span_remainder_unit = c(NULL, "months", "weeks", "days")) {
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_choice(target_column,
                           choices = colnames(data),
                           null.ok = TRUE)
  if (!checkmate::check_date(end_date, min.len = 1L, null.ok = FALSE)) {
    checkmate::assert_character(end_date, len = 1L, null.ok = FALSE)
  }
  checkmate::assert_character(span_column_name, len = 1L, null.ok = FALSE,
                              any.missing = FALSE)
  span_unit           <- match.arg(span_unit)
  span_remainder_unit <- match.arg(span_remainder_unit)

  # end_date can be a column of the input data or
  # a vector of Date values with the same length as number of row in data or
  # a Date value
  if (is.character(end_date) && end_date %in% colnames(data)) {
    span_result <- abs(unclass(data[[target_column]]) -
                         unclass(data[[end_date]]))
  } else {
    span_result <- abs(unclass(data[[target_column]]) - unclass(end_date))
  }
  units         <- c(365.25, 30.0, 7.0, 1.0)
  names(units)  <- c("years", "months", "weeks", "days")
  if (!is.null(span_remainder_unit)) {
    data[, span_column_name] <- floor(span_result / units[span_unit])
    data[, sprintf("%s_remainder", span_column_name)] <- round(
      (span_result %% units[span_unit]) / units[span_remainder_unit],
      digits = 2L)
  } else {
    data[, span_column_name] <- round(span_result / units[span_unit],
                                      digits = 2L)
  }

  return(data)
}
