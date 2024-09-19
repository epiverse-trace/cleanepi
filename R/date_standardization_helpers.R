#' Check date time frame
#'
#' @param first_date A Date object specifying the first valid date.
#'    The arbitrary default value is fifty years before the `last_date`.
#' @param last_date A Date object specifying the last valid date.
#'    The defaults value is the current date.
#'
#' @returns A list with the first and last dates
#' @keywords internal
#'
date_check_timeframe <- function(first_date, last_date) {

  # make sure that they are single character strings in ISO 8601 format.
  iso_8601 <- "[0-9]{4}-(0|1(?=[0-2]))[0-9]-([0-2]|3(?=[0-1]))[0-9]"

  first_date_is_character <- is.character(first_date)
  first_date_has_len_1    <- length(first_date) == 1L
  first_date_has_iso_8601 <- grepl(iso_8601, first_date, perl = TRUE)
  verdict <- first_date_is_character & first_date_has_len_1 &
    first_date_has_iso_8601
  if (verdict) {
    first_date <- as.Date(first_date, "%Y-%m-%d")
  }

  last_date_is_character <- is.character(last_date)
  last_date_has_len_1    <- length(last_date) == 1L
  last_date_has_iso_8601 <- grepl(iso_8601, last_date, perl = TRUE)
  verdict <- last_date_is_character & last_date_has_len_1 &
    last_date_has_iso_8601
  if (verdict) {
    last_date <- as.Date(last_date, "%Y-%m-%d")
  }

  # Set the first date to 50 years before the last date if it's not set
  if (is.null(first_date) && inherits(last_date, "Date")) {
    first_date <- min(seq.Date(last_date, length.out = 2L, by = "-50 years"))
  }

  if (!inherits(first_date, "Date") || !inherits(last_date, "Date")) {
    stop("first_date and last_date must be Date objects or characters in",
         "yyyy-mm-dd format.")
  }
  return(list(first_date, last_date))
}

#' Trim dates outside of the defined boundaries
#'
#' @param new_dates A vector of the new date values
#' @param dmin A minimum date
#' @param dmax A maximum date
#' @param original_dates A vector of original dates (to be collected
#'    for errors)
#' @param cols The name of the date column of interest
#' @param original_dates A vector of the original date values
#'
#' @returns A list of 2 elements: the update input vector where date values that
#'    are out of the boundaries are replaced by `NA`, and a vector of the out of
#'    boundaries values.
#' @keywords internal
#'
date_trim_outliers <- function(new_dates, dmin, dmax, cols, original_dates) {

  # filter out the dates that are below the threshold
  outsiders <- new_dates < dmin | new_dates > dmax
  outsiders[is.na(outsiders)] <- FALSE

  # record the bad dates in the report
  out_of_boundaries <- NULL
  if (any(outsiders)) {
    idx <- which(outsiders)
    out_of_boundaries <- data.frame(idx            = idx,
                                    column         = rep(cols, length(idx)),
                                    original_value = original_dates[idx])
  }
  # mark the bad dates as NA
  new_dates[outsiders] <- as.Date(NA_character_)

  return(list(
    new_dates = new_dates,
    outsiders = out_of_boundaries
  ))
}

#' Convert characters to dates
#'
#' @inheritParams standardize_dates
#' @param cols  date column name(s)
#'
#' @returns A data frame where the specified columns have been converted
#'    into Date.
#'
#' @keywords internal
#'
date_convert <- function(data, cols, error_tolerance,
                         timeframe = NULL, orders, modern_excel) {
  # Guess the date using lubridate (for actual dates and numbers) and the
  # guesser we developed
  new_dates <- data[[cols]]
  if (!inherits(data[[cols]], "Date")) {
    date_guess_res <- date_guess(
      new_dates,
      orders = orders,
      modern_excel = modern_excel,
      column_name = cols
    )
    new_dates <- date_guess_res[["res"]]
    multi_format_dates <- date_guess_res[["multi_format"]]
    # report the multi formatted dates if they were detected
    if (nrow(multi_format_dates) > 0L) {
      data <- add_to_report(
        x = data,
        key = "multi_format_dates",
        value = multi_format_dates
      )
    }
  }

  # Trim outliers i.e. date values that are out of the range of the provided
  # timeframe
  outsiders        <- NULL
  if (!is.null(timeframe)) {
    res            <- date_convert_and_update(data, timeframe, new_dates, cols,
                                              error_tolerance)
    data           <- res[["data"]]
    outsiders      <- res[["outsiders"]]
  } else {
    data[[cols]]   <- new_dates
  }

  # report the out of range dates
  if (!is.null(outsiders)) {
    data         <- add_to_report(x     = data,
                                  key   = "out_of_range_dates",
                                  value = outsiders)
  }

  return(data)
}


#' Convert and update the date values
#'
#' @inheritParams standardize_dates
#' @param new_dates A vector of the converted date values
#' @param cols The names of the date column been converted
#'
#' @returns A list of 2 data frames: the updated input data (if some columns
#'    were converted to Date) and a data frame of date values that are not
#'    within the specified timeframe.
#' @keywords internal
#'
date_convert_and_update <- function(data, timeframe, new_dates, cols,
                                    error_tolerance = 0.5) {
  timeframe <- date_check_timeframe(timeframe[[1L]], timeframe[[2L]])
  new_dates <- date_trim_outliers(new_dates,
                                  timeframe[[1L]], timeframe[[2L]],
                                  cols, data[[cols]])
  na_before       <- sum(is.na(data[[cols]]))
  na_after        <- sum(is.na(new_dates[["new_dates"]]))
  prop_successful <- (nrow(data) - na_after) / (nrow(data) - na_before)

  # shape result depending on whether conversion was successful
  if (prop_successful > (1L - error_tolerance)) {
    data[[cols]]   <- new_dates[["new_dates"]]
  }

  outsiders   <- new_dates[["outsiders"]]
  if (all(is.na(new_dates[["new_dates"]]))) {
    outsiders <- NULL
  }
  report      <- attr(data, "report")
  if ("out_of_range_dates" %in% names(report)) {
    outsiders <- rbind(report[["out_of_range_dates"]], outsiders)
  }

  return(list(data = data, outsiders = outsiders))
}

#' Guess if a character vector contains Date values, and convert them to date
#'
#' @param data A data frame
#' @inheritParams standardize_dates
#'
#' @returns The input data frame where the character columns with date values
#'    have been converted into Date.
#' @keywords internal
#'
date_guess_convert <- function(data, error_tolerance, timeframe,
                               orders, modern_excel) {
  # guess and convert for column of type character, factor and POSIX
  are_posix <- which(vapply(data, inherits, logical(1), "POSIXt"))
  are_characters <- which(vapply(data, inherits, logical(1), "character"))
  are_factors <- which(vapply(data, inherits, logical(1), "factor"))
  are_dates <- which(vapply(data, inherits, logical(1), "Date"))

  # convert POSIX to date
  for (i in are_posix) {
    data[[i]] <- as.Date(data[[i]])
  }

  # convert characters and factors to date when applicable
  of_interest <- c(are_characters, are_factors, are_dates, are_posix)
  multi_format_dates <- NULL
  for (i in names(of_interest)) {
    date_guess_res    <- date_guess(data[[i]], orders = orders,
                                    modern_excel = modern_excel,
                                    column_name = i)
    new_dates         <- date_guess_res[["res"]]
    multi_format      <- date_guess_res[["multi_format"]]
    multi_format_dates <- c(multi_format_dates, list(multi_format))

    if (!all(is.na(new_dates)) && is.null(timeframe)) {
      data[[i]]       <- new_dates
    }
    if (!is.null(timeframe)) {
      res             <- date_convert_and_update(data, timeframe, new_dates, i,
                                                 error_tolerance)
      data            <- res[["data"]]
      data            <- add_to_report(x     = data,
                                       key   = "out_of_range_dates",
                                       value = res[["outsiders"]])
    }
  }
  multi_format_dates <- dplyr::bind_rows(multi_format_dates)

  # report the multi formatted dates
  if (!is.null(multi_format_dates)) {
    data            <- add_to_report(
      x     = data,
      key   = "multi_format_dates",
      value = multi_format_dates
    )
  }

  return(data)
}

#' Detect complex date format
#'
#' @param x A string of interest
#'
#' @returns A string with the inferred format.
#' @keywords internal
#'
date_detect_complex_format <- function(x) {
  f1 <- f2 <- NULL
  tmp_sep <- unique(unlist(lapply(x, date_detect_separator)))
  if (is.null(tmp_sep)) {
    f1 <- date_detect_simple_format(x)
    if (is.null(f1)) {
      f1 <- date_detect_day_or_month(x)
    }
  } else if (!is.na(tmp_sep) && length(tmp_sep) == 1L) {
    p1 <- as.character(unlist(lapply(x, date_get_part1, tmp_sep)))
    p2 <- as.character(unlist(lapply(x, date_get_part2, tmp_sep)))
    f1 <- date_detect_simple_format(p1)
    f2 <- date_detect_simple_format(p2)
    if (is.null(f1)) {
      f1 <- date_detect_day_or_month(p1)
    }
    if (is.null(f2)) {
      f2 <- date_detect_day_or_month(p2)
    }
  }

  if (all(is.null(c(f1, f2)))) {
    return(NULL)
  }
  format <- paste(c(f1, f2), collapse = tmp_sep)
  return(format)
}

#' Detect a date format with only 1 separator
#'
#' @param x A string of interest
#'
#' @returns A string with the identified format.
#' @keywords internal
#'
date_detect_format <- function(x) {
  # check the format in x
  idx <- which(is.na(x))
  if (length(idx) > 0L) {
    x <- x[-idx]
  }
  if (all(numbers_only(x))) {
    f1 <- date_detect_simple_format(x)
  } else {
    f1 <- date_detect_complex_format(x)
  }
  return(f1)
}

#' Check if date column exists in the given dataset
#'
#' @param data The input data frame
#' @param date_column_names A vector with the name of the columns to check
#'
#' @returns The input vector if all column names are part of the input data, an
#'    error is issued otherwise.
#' @keywords internal
#'
date_check_column_existence <- function(data, date_column_names) {
  # convert to vector if comma-separated list of names is provided
  if (is.character(date_column_names)) {
    date_column_names <- unlist(strsplit(date_column_names, ",", fixed = TRUE))
    date_column_names <- trimws(date_column_names)
  }

  # check whether the provided column name belong to the data
  if (!all(date_column_names %in% names(data))) {
    idx <- which(!(date_column_names %in% names(data)))
    stop("Can't find columns: ", toString(date_column_names[idx]))
  }
  return(date_column_names)
}

#' Detect the special character that is the separator in the date values
#'
#' @param x A string of interest
#' @returns A detected separator
#'
#' @returns A vector of the identified special characters.
#' @keywords internal
#'
date_detect_separator <- function(x) {
  sep                <- NULL
  special_characters <- c("-", "/", ",", " ")
  if (!is.na(x)) {
    sep <- NULL
    for (spec_char in special_characters) {
      if (grepl(pattern = spec_char, x)) {
        sep <- c(sep, spec_char)
      }
    }
  }
  return(sep)
}

#' Detect the appropriate abbreviation for day or month value
#'
#' @param x The input string
#'
#' @returns A string with abbreviation used to distinguish the written day or
#'    month
#' @keywords internal
#'
date_detect_day_or_month <- function(x) {
  f1 <- NULL
  full_days <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                 "Friday", "Saturday", "Sunday")
  abreviated_days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  all_full_months <- c(month.name, toupper(month.name), tolower(month.name))
  all_abb_months  <- c(month.abb, toupper(month.abb), tolower(month.abb))
  all_full_days   <- c(full_days, toupper(full_days), tolower(full_days))
  all_abb_days    <- c(abreviated_days, toupper(abreviated_days),
                       tolower(abreviated_days))
  in_full_month   <- x %in% all_full_months
  in_abb_month    <- x %in% all_abb_months
  in_full_day     <- x %in% all_full_days
  in_abb_day      <- x %in% all_abb_days
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
  return(f1)
}

#' Get format from a simple Date value
#'
#' @param x A string with the date value of interest
#'
#' @returns A string with the abbreviation that correspond to the Date value
#' @keywords internal
#'
date_detect_simple_format <- function(x) {
  if (all(is.na(suppressWarnings(as.numeric(x))))) {
    return(NULL)
  }
  f1 <- NULL
  if (is.null(x)) f1 <- NULL
  if (all(nchar(x) == 4L)) {
    f1 <- "%Y" # year with century i.e 4 digits year
  } else if (any(nchar(x) == 4L) && any(nchar(x) == 2L)) {
    stop("Detected different lengths in first digits of date column.\n",
         "Please use same number of digits or specify the date format with \n",
         "the 'format' argument.")
  } else if (all(nchar(x) == 2L)) {
    tmp <- as.numeric(x)
    if (all(tmp <= 12L)) {
      f1 <- "%m"
    } else if (all(tmp >= 1L) && all(tmp <= 31L)) {
      f1 <- "%d"
    } else {
      f1 <- "%y"
    }
  }
  return(f1)
}

#' Infer date format from a vector or characters
#'
#' @param x The input character vector
#'
#' @return A string with the inferred date format
#' @keywords internal
#'
date_get_format <- function(x) {
  # special characters, which usually represent the separator between different
  # parts of a date value, will be replaced by "-" given that the format is hard
  # to resolve in presence of multiple special characters and only the first
  # separator in required to resolve the format.

  # define the regular expression used to substitute all occurrences of special
  # characters with '-'
  separator <- "[[:punct:][:blank:]]+"
  x <- gsub(separator, "-", x)

  # only retain values containing '-'.
  idx_potential_dates <- grep("-", x, fixed = TRUE)
  if (length(idx_potential_dates) == 0) {
    return(NULL)
  }
  x <- unique(x[idx_potential_dates])

  # split by "-" to separate the values into pieces from which a date format
  # could be determined. Every piece will be subjected to the guesser that
  # was built and described below:
  tmp_list <- strsplit(x, "-", fixed = TRUE)

  # when there is a missing value (NA) or a shorter element in the list, repeat
  # complete it with NA to get the same length across all elements of the list
  lengths <- as.numeric(lapply(tmp_list, length))
  add_na <- function(y, n) return(c(y, rep(NA, n - length(y))))
  tmp_list <- lapply(tmp_list, add_na, max(lengths))

  # split all elements of the list based on "-" and store the different parts
  # in separate object. Each part is subjected to the date guesser:
  # 1. For numeric dates, it will check whether none of the values is > 12. If
  # some are > 12, then that part of the split corresponds to the days in the
  # date value. Otherwise, it is considered to correspond to the months in the
  # date value. When the values are four digit numbers, then it will consider
  # them as the year part of the date value.
  # 2. Similarly, the remaining 2 parts are all subjected to the guesser to
  # determine the 3 usual components of a date value.
  # 3. Character values in all the three parts will be checked against the full
  # and abbreviated month names to detect the corresponding month from the date
  # value.
  # 4. When there is some ambiguities or the guesser is not able to resolve the
  # format, it returns NA for that corresponding part.
  part1 <- part2 <- part3 <- rep(NA, length(x))
  if (any(lengths(tmp_list) >= 1L)) {
    part1 <- as.character(
      unlist(lapply(x, date_get_part1, "-"))
    )
  }
  if (any(lengths(tmp_list) >= 2L)) {
    part2 <- as.character(
      unlist(lapply(x, date_get_part2, "-"))
    )
  }
  if (any(lengths(tmp_list) >= 3L)) {
    part3 <- as.character(
      unlist(lapply(x, date_get_part3, "-"))
    )
  }

  # guess the date format from every part of the vector
  f1 <- if (all(is.na(part1))) NA else date_detect_format(part1)
  f2 <- if (all(is.na(part2))) NA else date_detect_format(part2)
  f3 <- if (all(is.na(part3))) NA else date_detect_format(part3)
  format <- date_make_format(f1, f2, f3)
  return(format)
}

#' Build the auto-detected format
#'
#' Put together the different date format characters that were identified from
#' the target date column.
#'
#' @param f1 The first part of the inferred format
#' @param f2 The second part of the inferred format
#' @param f3 The third part of the inferred format
#'
#' @return A character string that represent the inferred format from the date
#'    values. It returns NULL when the format was not resolved.
#'
#' @keywords internal
#'
date_make_format <- function(f1, f2, f3) {
  verdict <- is.null(f1) | is.null(f2) | is.null(f3) |
    length(is.na(c(f1, f2, f3))) >= 2L
  if (verdict) {
    return(NULL)
  }
  format   <- NULL
  idx      <- which(is.na(c(f1, f2, f3)))
  if (length(idx) == 0L) {
    format <- paste0(format, f1, "-", f2, "-", f3)
  } else if (idx == 3L) {
    format <- paste0(format, f1, "-", f2)
  } else {
    return(NULL)
  }
}


#' Process date variable
#'
#' @param x A object of class Date
#'
#' @return The converted input value into Date or character
#' @keywords internal
#'
date_process <- function(x) {
  # If the input is a date already: no guessing needed!
  if (inherits(x, c("Date", "POSIXt", "aweek"))) {
    x <- as.Date(x)
    return(x)
  }

  if (is.factor(x) || is.numeric(x)) {
    x <- as.character(x)
  }

  if (!is.character(x)) {
    stop("guess dates will only work for characters and factors")
  }

  return(x)
}

#' Check whether the number of provided formats matches the number of target
#' columns to be standardized.
#'
#' @param target_columns A vector of column names to be standardized
#' @param format A vector of formats to be used when standardizing the columns
#'
#' @return A vector of format
#' @keywords internal
#'
date_match_format_and_column <- function(target_columns, format) {
  if (length(target_columns) > 2L && length(format) == 2L) {
    stop("Need to specify one format if all target columns have the same",
         "format.\nProvide one format per target column, otherwise.")
  }
  if (length(target_columns) >= 1L && length(format) == 1L) {
    warning("Using ", format, " to standardize target columns...",
            call. = FALSE)
    format <- rep(format, length(target_columns))
  }
  return(format)
}
