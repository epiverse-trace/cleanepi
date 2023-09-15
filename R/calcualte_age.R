#' function to calculate age from date of birth
#' @param data a data frame with the date column to standardise
#' @param date_column_name the name of the date column of interest.
#' default: 'Date', or, 'DATE', or 'date'
#' @param end_date the end date. default: today's date
#' @param age_in a character string to specify whether to calculate
#'  the age in 'years', or 'months', or 'days', or 'weeks'.
#' default is: 'years'
#' @returns a data frame with 1 or 2 extra columns compared to the input data
#' frame
#' \enumerate{
#'   \item "age_years", or "age_months", or "age_weeks", or "age_days",
#' depending on the value of the 'age_in' parameter
#'   \item "remainder_days": a column with the number of remaining days
#'  after the age is converted in weeks or months
#'   }
#' @export
#' @examples
#' age <- calculate_age(
#' data = readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")),
#' date_column_name = "dateOfBirth",
#' end_date = Sys.Date(),
#' age_in = "months"
#' )
calculate_age <- function(data, date_column_name = NULL, end_date = Sys.Date(),
                          age_in = "years") {
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_character(date_column_name, null.ok = TRUE,
                              any.missing = FALSE, len = 1L)
  checkmate::assert_character(age_in, null.ok = FALSE, any.missing = FALSE,
                              len = 1L)
  checkmate::assert_date(end_date, any.missing = FALSE, len = 1L,
                         null.ok = TRUE)

  tmp_age <- remainder_days <- NULL

  # check if date column exists in the data
  date_column_name <- check_column_existence(data, date_column_name)

  # replace missing data characters with NA
  data <- replace_missing_char(data, date_column_name)

  # standardize the input data if required
  if (!lubridate::is.Date(data[[date_column_name]])) {
    data <- standardize_date(data, date_column_name, format = NULL,
                             timeframe = NULL, check_timeframe = FALSE,
                             error_tolerance = 0.5)[[1L]]
  }

  # calculate age
  if (!(age_in %in% c("years", "months", "weeks", "days"))) {
    stop("Incorrect value for 'age_in' parameter.\n",
         "Please specify whether the age should be returned in 'year', or",
         "'month', or in 'weeks', or in 'days'.")
  }
  end_date <- as.Date(end_date)

  # calculate the age
  res <- switch(
    age_in,
    years = data %>%
      dplyr::mutate(age_years = round((data[[date_column_name]] %--% end_date)
                                      %/% lubridate::years(1L))),
    months = data %>%
      dplyr::mutate(tmp_age = lubridate::as.period(end_date -
                                                     data[[date_column_name]])) %>% # nolint: line_length_linter.
      dplyr::mutate(age_months = tmp_age %/% months(1L),
                    remainder_days = (tmp_age %% months(1L)) %/%
                      lubridate::days(1L)) %>%
      dplyr::select(-tmp_age),
    days = data %>%
      dplyr::mutate(tmp_age = lubridate::as.period(end_date -
                                                     data[[date_column_name]])) %>% # nolint: line_length_linter.
      dplyr::mutate(age_days = tmp_age %/% lubridate::days(1L)) %>%
      dplyr::select(-tmp_age),
    weeks = data %>%
      dplyr::mutate(tmp_age = lubridate::as.period(end_date -
                                                     data[[date_column_name]])) %>% # nolint: line_length_linter.
      dplyr::mutate(age_weeks = tmp_age %/% lubridate::weeks(1L),
                    remainder_days = (tmp_age %% lubridate::weeks(1L))
                    %/% lubridate::days(1L)) %>%
      dplyr::select(-tmp_age)
  )
  if (age_in %in% c("months", "weeks") && all(res[["remainder_days"]] == 0L)) {
    res <- res %>% dplyr::select(-remainder_days)
  }
  res
}
