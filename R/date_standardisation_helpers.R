#' Reformat the date
#'
#' @param x the string of interest
#' @param format the date format
#'
as_Date <- function(x, format = c("ymd", "ydm", "dmy", "mdy", "myd", "dym",
                                  "Ymd", "Ydm", "dmY", "mdY", "mYd", "dYm")) {
  fmt <- lubridate::guess_formats(x, format)
  fmt <- unique(fmt)
  y <- as.Date(x, format = fmt[1])
  for (i in seq_along(fmt)[-1]) {
    na <- is.na(y)
    if (!any(na)) break
    y[na] <- as.Date(x[na], format = fmt[i])
  }
  y
}


#' Check date time frame
#'
#' @param first_date a Date object specifying the first valid date. Defaults to
#'   fifty years before the `last_date`
#' @param last_date last_date a Date object specifying the last valid date.
#'   Defaults to the current date.
#'
#' @return a list with the first and last date
#'
check_first_and_last_date <- function(first_date, last_date) {

  # make sure that they are single character strings in ISO 8601 format.
  iso_8601 <- "[0-9]{4}-(0|1(?=[0-2]))[0-9]-([0-2]|3(?=[0-1]))[0-9]"

  first_date_is_charcater <- is.character(first_date)
  first_date_has_len_1 <- length(first_date) == 1
  first_date_has_iso_8601 <- grepl(iso_8601, first_date, perl = TRUE)
  verdict <- first_date_is_charcater & first_date_has_len_1 &
    first_date_has_iso_8601
  if (verdict) {
    first_date <- as.Date(first_date, "%Y-%m-%d")
  }

  last_date_is_charcater <- is.character(last_date)
  last_date_has_len_1 <- length(last_date) == 1
  last_date_has_iso_8601 <- grepl(iso_8601, last_date, perl = TRUE)
  verdict <- last_date_is_charcater & last_date_has_len_1 &
    last_date_has_iso_8601
  if (verdict) {
    last_date <- as.Date(last_date, "%Y-%m-%d")
  }

  # Set the first date to 50 years before the last date if it's not set
  if (is.null(first_date) && inherits(last_date, "Date")) {
    first_date <- min(seq.Date(last_date, length.out = 2, by = "-50 years"))
  }

  if (!inherits(first_date, "Date") || !inherits(last_date, "Date")) {
    stop("first_date and last_date must be Date objects or characters in
         yyyy-mm-dd format.")
  }
  list(first_date, last_date)
}

#' Convert characters to dates
#'
#' @param data the input data frame
#' @param cols the date column name(s)
#' @param sep the separator in the date values
#'
#' @return the input data frame where the specified columns have been converted
#'    into Date.
#'
#' @keywords internal
convert_to_date <- function(data, cols, sep) {
  format <- get_format(data, cols, sep)
  if (!is.null(format)) {
    data[[cols]] <- as.Date(data[[cols]], format = format)
  } else {
    data[[cols]] <- guess_dates(data[[cols]],
                                error_tolerance = 0.5,
                                check_timeframe = FALSE)
  }
  data
}

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
  are_dates      <- which(col_types == "Date")

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
  for (i in c(are_characters, are_factors, are_dates, are_POSIX)) {
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

#' Detect complex format
#'
#' @param x the string of interest
detect_complex_format <- function(x) {
  f1 <- f2 <- NULL
  tmp_sep <- unique(unlist(lapply(x, detect_date_separator)))
  if (is.null(tmp_sep)) {
    f1 <- detect_simple_format(x)
    if (is.null(f1)) {
      f1 <- detect_day_or_month(x)
    }
  } else if (!is.na(tmp_sep) && length(tmp_sep) == 1) {
    p1 <- as.character(unlist(lapply(x, get_part1, tmp_sep)))
    p2 <- as.character(unlist(lapply(x, get_part2, tmp_sep)))
    f1 <- detect_simple_format(p1)
    f2 <- detect_simple_format(p2)
    if (is.null(f1)) {
      f1 <- detect_day_or_month(p1)
    }
    if (is.null(f2)) {
      f2 <- detect_day_or_month(p2)
    }
  } else {
    # stop("Unrecognised date format.\nPlease specify the date format using the
    #      'format' argument.")
    return(NULL)
  }
  format <- make_format(f1, f2, tmp_sep)
  format
}

#' Detect the date format with only 1 separator
#'
#' @param x the string of interest
detect_date_format <- function(x) {
  # check the format in x
  idx <- which(is.na(x))
  if (length(idx) > 0) {
    x <- x[-idx]
  }
  if (all(numbers_only(x))) {
    f1 <- detect_simple_format(x)
  } else {
    f1 <- detect_complex_format(x)
  }
  f1
}

#' Detect the special character that is the separator in the date values
#'
#' @param x the string of interest
#' @returns the detected separator
detect_date_separator <- function(x) {
  sep <- NULL
  if (!is.na(x)) {
    special_characters <- c("-", "/", ",", " ")
    sep <- NULL
    for (spec_char in special_characters) {
      if (stringr::str_detect(x, spec_char)) {
        sep <- c(sep, spec_char)
      }
    }
  }
  sep
}

#' Detect whether it's day or month
#'
#' @param x the string of interest
detect_day_or_month <- function(x) {
  f1 <- NULL
  full_days <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                 "Friday", "Saturday", "Sunday")
  abreviated_days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  all_full_months <- c(month.name, toupper(month.name), tolower(month.name))
  all_abb_months <- c(month.abb, toupper(month.abb), tolower(month.abb))
  all_full_days <- c(full_days, toupper(full_days), tolower(full_days))
  all_abb_days <- c(abreviated_days, toupper(abreviated_days),
                    tolower(abreviated_days))
  in_full_month <- x %in% all_full_months
  in_abb_month <- x %in% all_abb_months
  in_full_day <- x %in% all_full_days
  in_abb_day <- x %in% all_abb_days
  if (all(in_full_month)) {
    f1 <- "%B"
  } else if (all(in_abb_month)) {
    f1 <- "%b"
  }
  if (all(in_full_day)) {
    f1 <- "%A"
  } else if (all(in_abb_day)) {
    f1 <- "%a"
  }
  f1
}

#' function to get simple format
#'
#' @param x the string of interest
detect_simple_format <- function(x) {
  f1 <- NULL
  if (is.null(x)) f1 <- NULL
  if (all(nchar(x) == 4)) {
    f1 <- "%Y" # year with century i.e 4 digits year
  } else if (any(nchar(x) == 4) && any(nchar(x) == 2)) {
    stop("Detected different lengths in first digits of date column.\n
         Please use same number of digits or specify the date format with
         the 'format' argument.")
  } else if (all(nchar(x) == 2)) {
    tmp <- as.numeric(x)
    if (all(tmp <= 12)) {
      f1 <- "%m"
    } else if (all(tmp >= 1) && all(tmp <= 31)) {
      f1 <- "%d"
    } else {
      f1 <- "%y"
    }
  }
  f1
}

#' detect date format from date column
#'
#' @param data the input data frame
#' @param date_column_name the date column of interest
#' @param sep the separator in the date string
#'
#' @return a string with the detected date format
get_format <- function(data, date_column_name, sep) {
  format <- NULL
  data[[date_column_name]] <- as.character(data[[date_column_name]])
  part1 <- as.character(unlist(lapply(data[[date_column_name]], get_part1,
                                      sep[1])))
  part2 <- as.character(unlist(lapply(data[[date_column_name]], get_part2,
                                      sep[1])))
  part3 <- as.character(unlist(lapply(data[[date_column_name]], get_part3,
                                      sep[1])))
  f1 <- ifelse(all(is.na(part1)), NA, detect_date_format(part1))
  f2 <- ifelse(all(is.na(part2)), NA, detect_date_format(part2))
  f3 <- ifelse(all(is.na(part3)), NA, detect_date_format(part3))
  idx <- which(is.na(c(f1, f2, f3)))
  if (length(idx) == 0) {
    format <- paste0(format, f1, sep[1], f2, sep[1], f3)
  } else if (idx == 3) {
    format <- paste0(format, f1, sep[1], f2)
  } else if (idx == c(2, 3)) {
    format <- paste0(format, f1)
  } else {
    # stop("Unrecognised date format.\nPlease specify the date format using the
    #        'format' argument.")
    return(NULL)
  }
  format
}

#' Build the auto-detected format
#'
#' Put together the different date format characters that were identified in
#' the target date column.
#'
#' @param f1 the first part of the date values
#' @param f2 the second part of the date values
#' @param tmp_sep the character string that separate the first and second parts
#'    of the date values
#'
#' @return a character string that represent the inferred format of the date
#'    values.
#'
#' @keywords internal
make_format <- function(f1, f2, tmp_sep) {
  if (all(is.null(f1) & is.null(f2))) {
    stop("Unrecognised date format.\nPlease specify the date format using
         the 'format' argument.")
  } else if (all(!is.null(f1) & !is.null(f2))) {
    format <- paste0(f1, tmp_sep, f2)
  } else if (!is.null(f1) && is.null(f2)) {
    format <- f1
  } else if (!is.null(f2) && is.null(f1)) {
    format <- f2
  }
  format
}


#' Process date variable
#'
#' @param x an object of date class
#' @param first_date a Date object specifying the first valid date. Defaults to
#'   fifty years before the `last_date`
#' @param last_date last_date a Date object specifying the last valid date.
#'   Defaults to the current date.
#' @param check_timeframe a logical specifying whether to check if all values
#'    fall under the specified timeframe
#'
#' @return the modified input object
#'
process_dates <- function(x, first_date, last_date, check_timeframe) {
  # If the input is a date already: no guessing needed!
  if (inherits(x, c("Date", "POSIXt", "aweek"))) {
    x <- as.Date(x)
    if (check_timeframe) {
      x[x < first_date | x > last_date] <- as.Date(NA_character_)
    }
    return(x)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (!is.character(x)) {
    stop("guess dates will only work for characters and factors")
  }

  return(x)
}
