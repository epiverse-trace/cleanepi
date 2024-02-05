#' Calculate age from a specified date column
#'
#' @param data A data frame with one date column
#' @param target_column A string specifying the name of the date column of interest
#' @param end_date An end date, the default is today's date
#' @param age_in a string that specifies whether to return the age in 'years',
#'     'months',  'weeks', or 'days'. The default is in 'years'.
#' @param ... Other extra arguments needed to perform this operation. They
#'    include:
#'    \enumerate{
#'      \item "na_strings": A string that represents the missing values in the
#'            date column of interest. This is only needed when the date column
#'            contains missing values.
#'   }
#'
#' @return A data frame with the following 1 or 2 extra columns compared to the
#'    input data frame:
#' \enumerate{
#'   \item "age_in_years",  "age_in_months",  "age_in_weeks", or
#'         "age_in_days" depending on the value of the 'age_in' parameter.
#'   \item "remainder_days": A column with the number of remaining days after
#'         the age is converted in weeks or months.
#'   }
#' @export
#'
#' @examples
#' age <- calculate_age(
#'   data          = readRDS(system.file("extdata", "test_df.RDS",
#'                                       package = "cleanepi")),
#'   target_column = "dateOfBirth",
#'   end_date      = Sys.Date(),
#'   age_in        = "months",
#'   na_strings    = "-99"
#' )
calculate_age <- function(data,
                          target_column = NULL,
                          end_date      = Sys.Date(),
                          age_in        = "years",
                          ...) {
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_character(target_column, null.ok = TRUE,
                              any.missing = FALSE, len = 1L)
  checkmate::assert_character(age_in, null.ok = FALSE, any.missing = FALSE,
                              len = 1L)
  checkmate::assert_date(end_date, any.missing = FALSE, len = 1L,
                         null.ok = TRUE)

  tmp_age       <- remainder_days <- NULL
  extra_args    <- list(...)

  # check if date column exists in the data
  target_column <- date_check_column_existence(data, target_column)

  # replace missing data characters with NA
  if (length(extra_args) > 0L && "na_strings" %in% names(extra_args)) {
    na_strings  <- extra_args[["na_strings"]]
    data        <- replace_missing_values(data, target_column,
                                          na_strings = na_strings)
  }


  # standardize the input data if required
  if (!lubridate::is.Date(data[[target_column]])) {
    data        <- standardize_dates(data, target_column, format = NULL,
                                     timeframe = NULL, error_tolerance = 0.5)
  }

  # calculate age
  if (!(age_in %in% c("years", "months", "weeks", "days"))) {
    stop("Incorrect value for 'age_in' parameter.\n",
         "Please specify whether the age should be returned in 'years', or",
         "'months', or in 'weeks', or in 'days'.")
  }
  end_date <- as.Date(end_date)

  # calculate the age
  res <- switch(
    age_in,
    years = data %>%
      dplyr::mutate(age_years = round((data[[target_column]] %--% end_date)
                                      %/% lubridate::years(1L))),
    months = data %>%
      dplyr::mutate(tmp_age = lubridate::as.period(end_date -
                                                     data[[target_column]])) %>% # nolint: line_length_linter.
      dplyr::mutate(age_months = tmp_age %/% months(1L), # nolint
                    remainder_days = (tmp_age %% months(1L)) %/% # nolint
                      lubridate::days(1L)) %>%
      dplyr::select(-tmp_age),
    days = data %>%
      dplyr::mutate(tmp_age = lubridate::as.period(end_date -
                                                     data[[target_column]])) %>% # nolint: line_length_linter.
      dplyr::mutate(age_days = tmp_age %/% lubridate::days(1L)) %>%
      dplyr::select(-tmp_age),
    weeks = data %>%
      dplyr::mutate(tmp_age = lubridate::as.period(end_date -
                                                     data[[target_column]])) %>% # nolint: line_length_linter.
      dplyr::mutate(age_weeks = tmp_age %/% lubridate::weeks(1L),
                    remainder_days = (tmp_age %% lubridate::weeks(1L))
                    %/% lubridate::days(1L)) %>%
      dplyr::select(-tmp_age)
  )
  if (age_in %in% c("months", "weeks") && all(res[["remainder_days"]] == 0L)) {
    res <- res %>% dplyr::select(-remainder_days)
  }

  return(res)
}
