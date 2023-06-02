#' Guess if a character vector contains Date values, and convert them into Date
#'
#' @param data the input data frame
#' @param error_tolerance a number between 0 and 1 indicating the proportion of
#'     entries which cannot be identified as dates to be tolerated. See the
#'     `clean_data()` helper for more details
#' @param timeframe the expected first and last date. See the `clean_data()`
#'    helper for more details
#' @param check_timeframe a logical to determine whether to check if the dates
#'    fall under the given time frame of not
#' @param report the object that will contains the report from this operation
#'
date_guess_convert <- function(data, error_tolerance = error_tolerance,
                               timeframe, check_timeframe, report) {
  # guess and convert for column of type character, factor and POSIX
  col_types <- vapply(data, function(x) class(x)[1], FUN.VALUE = "character")
  are_POSIX      <- which(grepl("^POSIX", col_types, fixed = TRUE))
  are_characters <- which(col_types == "character")
  are_factors    <- which(col_types == "factor")

  # convert POSIX to date
  for (i in are_POSIX) {
    data[[i]] <- as.Date(data[[i]])
  }

  # convert characters and factors to date
  if (is.null(timeframe)) {
    first_date <- NULL
    last_date <- Sys.Date()
  } else {
    first_date <- timeframe[1]
    last_date <- timeframe[2]
  }

  if (!("standardize_date" %in% names(report))) {
    report[["standardize_date"]] <- list()
  }
  for (i in c(are_characters, are_factors)) {
    tmp_data <- guess_dates(data[[i]], error_tolerance = error_tolerance,
                            first_date, last_date,
                            orders = list(world_named_months = c("Ybd", "dby"),
                                          world_digit_months = c("dmy", "Ymd"),
                                          US_formats = c("Omdy", "YOmd")),
                            check_timeframe
                            )
    data[[i]] <- tmp_data[[1]]
    if (!is.null(tmp_data[[2]])) {
      variable_name <- paste0(names(data)[i], "_NOT_IN_TIMEFRAME")
      report[["standardize_date"]][[variable_name]] <- tmp_data[[2]]
    }

  }

  list(data, report)
}
