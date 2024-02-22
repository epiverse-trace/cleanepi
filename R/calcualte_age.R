#' Calculate age from a specified date column
#'
#' @param data A data frame with at least one date column.
#' @param target_column A string specifying the name of the date column of
#'    interest.
#' @param end_date An end date, the default is today's date.
#' @param age_in A string that specifies whether to return the age in 'years',
#'    'months', 'weeks' or 'days'. The default is in 'years'.
#' @param na_strings A string that represents the missing values in the
#'    date column of interest. This is only needed when the date column
#'    contains missing values.
#'
#' @return The input data frame with 2 additional columns:
#' \enumerate{
#'   \item "age_in_years",  "age_in_months",  "age_in_weeks", or
#'         "age_in_days" depending on the value of the 'age_in' parameter.
#'   \item "remainder_days": A column with the number of remaining days after
#'         the age is calculated in years, weeks or months.
#'   }
#' @export
#'
#' @examples
#' age <- calculate_age(
#'   data = readRDS(system.file("extdata", "test_df.RDS",
#'     package = "cleanepi"
#'   )),
#'   target_column = "dateOfBirth",
#'   end_date = Sys.Date(),
#'   age_in = "months",
#'   na_strings = "-99"
#' )
calculate_age <- function(data,
                          target_column = NULL,
                          end_date      = Sys.Date(),
                          age_in        = "years",
                          na_strings    = cleanepi::common_na_strings) {
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_vector(na_strings, null.ok = FALSE,
                           any.missing = FALSE, min.len = 1L)
  checkmate::assert_choice(age_in,
                           choices = c("years", "months", "weeks", "days"),
                           null.ok = FALSE)
  checkmate::assert_choice(target_column, choices = colnames(data),
                           null.ok = TRUE)
  end_date <- checkmate::assert_date(as.Date(end_date),
                                     any.missing = FALSE, len = 1L,
                                     null.ok = TRUE)
  tmp_age <- remainder_days <- NULL

  # replace missing data characters with NA
  data <- replace_missing_values(data, target_column,
                                 na_strings = na_strings)

  # standardize the input data if required
  if (!lubridate::is.Date(data[[target_column]])) {
    data <- standardize_dates(data, target_column,
                              format          = NULL,
                              timeframe       = NULL,
                              error_tolerance = 0.0) # because target_column is explicit
  }

  # calculate the age
  res <- switch(age_in,
    years = data %>%
      dplyr::mutate(age_years = round((data[[target_column]] %--% end_date)
      %/% lubridate::years(1L))),
    months = data %>%
      dplyr::mutate(tmp_age = lubridate::as.period(end_date -
        data[[target_column]])) %>%
      dplyr::mutate(
        age_months = tmp_age %/% months(1L, abbreviate = FALSE),
        remainder_days = (tmp_age %% months(1L, abbreviate = FALSE)) %/%
          lubridate::days(1L)
      ) %>%
      dplyr::select(-tmp_age),
    days = data %>%
      dplyr::mutate(tmp_age = lubridate::as.period(end_date -
        data[[target_column]])) %>%
      dplyr::mutate(
        age_days = tmp_age %/% lubridate::days(1L)
      ) %>%
      dplyr::select(-tmp_age),
    weeks = data %>%
      dplyr::mutate(tmp_age = lubridate::as.period(end_date -
        data[[target_column]])) %>%
      dplyr::mutate(
        age_weeks = tmp_age %/% lubridate::weeks(1L),
        remainder_days = (tmp_age %% lubridate::weeks(1L))
        %/% lubridate::days(1L)
      ) %>%
      dplyr::select(-tmp_age)
  )
  if (age_in %in% c("months", "weeks") && all(res[["remainder_days"]] == 0L)) {
    res <- res %>% dplyr::select(-remainder_days)
  }

  return(res)
}
