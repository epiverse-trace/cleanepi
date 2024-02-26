#' Reformat a given date
#'
#' @param x A string of interest
#' @param format A date format
#' @keywords internal
#'
as_date <- function(x, format = c("ymd", "ydm", "dmy", "mdy", "myd", "dym",
                                  "Ymd", "Ydm", "dmY", "mdY", "mYd", "dYm")) {
  fmt <- lubridate::guess_formats(x, format)
  fmt <- unique(fmt)
  y   <- as.Date(x, format = fmt[[1L]])
  for (i in seq_along(fmt)[-1L]) {
    na    <- is.na(y)
    if (!any(na)) break
    y[na] <- as.Date(x[na], format = fmt[i])
  }
  return(y)
}


#' Check date time frame
#'
#' @param first_date A Date object specify.ing the first valid date.
#' The default value is fifty years before the `last_date`
#' @param last_date A  Date object specifying the last valid date.
#'    The defaults value is the current date.
#'
#' @return A list with the first and last dates
#' @keywords internal
#'
date_check_timeframe <- function(first_date, last_date) {

  # make sure that they are single character strings in ISO 8601 format.
  iso_8601 <- "[0-9]{4}-(0|1(?=[0-2]))[0-9]-([0-2]|3(?=[0-1]))[0-9]"

  first_date_is_charcater <- is.character(first_date)
  first_date_has_len_1    <- length(first_date) == 1L
  first_date_has_iso_8601 <- grepl(iso_8601, first_date, perl = TRUE)
  verdict <- first_date_is_charcater & first_date_has_len_1 &
    first_date_has_iso_8601
  if (verdict) {
    first_date <- as.Date(first_date, "%Y-%m-%d")
  }

  last_date_is_charcater <- is.character(last_date)
  last_date_has_len_1    <- length(last_date) == 1L
  last_date_has_iso_8601 <- grepl(iso_8601, last_date, perl = TRUE)
  verdict <- last_date_is_charcater & last_date_has_len_1 &
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
#' @return A data frame where the specified columns have been converted
#'    into Date.
#'
#' @keywords internal
#'
date_convert <- function(data, cols, error_tolerance,
                         timeframe = NULL) {
  # Guess the date using Thibault's parser
  new_dates <- date_guess(data[[cols]], error_tolerance = error_tolerance)

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

  # report this cleaning operation
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
#' @return A list of 2 data frames: the updated input data (if some columns were
#'    converted to Date) and a data frame of date values that are not within the
#'    specified timeframe.
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
  prop_successful <- (length(data[[cols]]) - na_after) / (length(data[[cols]]) - na_before) # nolint: line_length_linter

  # shape result depending on whether conversion was successful
  outsiders        <- NULL
  if (prop_successful > (1L - error_tolerance)) {
    data[[cols]]   <- new_dates[["new_dates"]]
    outsiders      <- rbind(outsiders, new_dates[["outsiders"]])
    report         <- attr(data, "report")
    if ("out_of_range_dates" %in% names(report)) {
      outsiders    <- rbind(report[["out_of_range_dates"]], outsiders)
    }
  }

  return(list(
    data           = data,
    outsiders      = outsiders
  ))
}

#' Guess if a character vector contains Date values, and convert them to date
#'
#' @param data A data frame
#' @param error_tolerance A numeric value between 0 and 1 that signifies the
#' proportion of entries that cannot be recognized as dates and are acceptable.
#'  For more detailed information, you can refer to the `clean_data()`
#'  helper function.
#' @param timeframe The expected first and last date. See the `clean_data()`
#'    helper for more details.
#'
#' @keywords internal
#'
date_guess_convert <- function(data, error_tolerance, timeframe) {
  # guess and convert for column of type character, factor and POSIX
  col_types      <- vapply(data, function(x) class(x)[[1L]],
                           FUN.VALUE = "character")
  are_posix      <- which(grepl("^POSIX", col_types, fixed = TRUE))
  are_characters <- which(col_types == "character")
  are_factors    <- which(col_types == "factor")
  are_dates      <- which(col_types == "Date")

  # convert POSIX to date
  for (i in are_posix) {
    data[[i]]   <- as.Date(data[[i]])
  }

  # convert characters and factors to date when applicable
  outsiders     <- NULL
  of_interest   <- c(are_characters, are_factors, are_dates, are_posix)
  for (i in names(of_interest)) {
    new_dates   <- date_guess(data[[i]], error_tolerance = error_tolerance)
    if (!all(is.na(new_dates)) && is.null(timeframe)) {
      data[[i]] <- new_dates
    }
    if (!is.null(timeframe)) {
      res       <- date_convert_and_update(data, timeframe, new_dates, i,
                                           error_tolerance)
      data      <- res[["data"]]
      outsiders <- res[["outsiders"]]
    }

    # report this cleaning operation
    if (!is.null(outsiders)) {
      data      <- add_to_report(x     = data,
                                 key   = "out_of_range_dates",
                                 value = outsiders)
    }
  }

  return(data)
}

#' Detect complex date format
#'
#' @param x A string of interest
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
  } else {
    return(NULL)
  }
  format <- date_make_format(f1, f2, tmp_sep)
  return(format)
}

#' Detect a date format with only 1 separator
#'
#' @param x A string of interest
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
#' @param data A dataframe
#' @param date_column_names The name of the columns to check
#'
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
    stop("Can't find columns: ", glue::glue_collapse(date_column_names[idx],
                                                     sep = ", "))
  }
  return(date_column_names)
}

#' Detect the special character that is the separator in the date values
#'
#' @param x A string of interest
#' @returns A detected separator
#' @keywords internal
#'
date_detect_separator <- function(x) {
  sep                <- NULL
  special_characters <- c("-", "/", ",", " ")
  if (!is.na(x)) {
    sep <- NULL
    for (spec_char in special_characters) {
      if (stringr::str_detect(x, spec_char)) {
        sep <- c(sep, spec_char)
      }
    }
  }
  return(sep)
}

#' Detect whether it's day or month
#'
#' @param x A string of interest
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

#' Get format from a simple date value
#'
#' @param x A string with the date value of interest
#' @keywords internal
#'
date_detect_simple_format <- function(x) {
  f1 <- NULL
  if (is.null(x)) f1 <- NULL
  if (all(nchar(x) == 4L)) {
    f1 <- "%Y" # year with century i.e 4 digits year
  } else if (any(nchar(x) == 4L) && any(nchar(x) == 2L)) {
    stop("Detected different lengths in first digits of date column.\n",
         "Please use same number of digits or specify the date format with",
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

#' Detect date format from a date column
#'
#' @param data A  data frame
#' @param date_column_name The name of the date columns of interest
#' @param sep A separator in the date string
#'
#' @return A string with the detected date format
#' @keywords internal
#'
date_get_format <- function(data, date_column_name, sep) {
  # we will first check if all values in the date column have that separator in
  # them. If there are some without the separator, we will set
  # other = index_of_values_without_the_separator and return it. This will be
  # used to inform the 'date_convert()' function that those values need to
  # handle differently.
  tmp_date_column <- data[[date_column_name]]
  tmp_date_column <- as.character(tmp_date_column)
  others          <- NULL

  # return 'NULL' if there are multiple formats in the date column
  if (!all(grepl(sep[[1L]], tmp_date_column))) {
    return(NULL)
    # idx_not_to_consider    <- which(!grepl(sep, tmp_date_column))
    # tmp_date_column        <- tmp_date_column[-idx_not_to_consider]
    # others                 <- idx_not_to_consider
  }

  # investigate the format among the date values that contain the separator
  tmp_list                 <- strsplit(tmp_date_column,
                                       sep[[1L]],
                                       fixed = TRUE)
  idx                      <- which(is.na(tmp_list))
  if (length(idx) > 0L) {
    tmp_list[[idx]]        <- rep(NA, max(lengths(tmp_list)))
  }
  part1 <- part2 <- part3 <- rep(NA, length(tmp_date_column))
  if (any(lengths(tmp_list) >= 1L)) {
    part1 <- as.character(unlist(lapply(tmp_date_column,
                                        date_get_part1,
                                        sep[[1L]])))
  }
  if (any(lengths(tmp_list) >= 2L)) {
    part2 <- as.character(unlist(lapply(tmp_date_column,
                                        date_get_part2,
                                        sep[[1L]])))
  }
  if (any(lengths(tmp_list) >= 3L)) {
    part3 <- as.character(unlist(lapply(tmp_date_column,
                                        date_get_part3,
                                        sep[[1L]])))
  }

  f1       <- ifelse(all(is.na(part1)), NA, date_detect_format(part1))
  f2       <- ifelse(all(is.na(part2)), NA, date_detect_format(part2))
  f3       <- ifelse(all(is.na(part3)), NA, date_detect_format(part3))
  idx      <- which(is.na(c(f1, f2, f3)))
  format   <- NULL
  if (length(idx) == 0L) {
    format <- paste0(format, f1, sep[[1L]], f2, sep[[1L]], f3)
  } else if (idx == 3L) {
    format <- paste0(format, f1, sep[[1L]], f2)
  } else if (idx == c(2L, 3L)) {
    format <- paste0(format, f1)
  } else {
    return(NULL)
  }
  return(format)
}

#' Build the auto-detected format
#'
#' Put together the different date format characters that were identified in
#' the target date column.
#'
#' @param f1 The first part of the date values
#' @param f2 The second part of the date values
#' @param tmp_sep The character string that separate the first and second parts
#'    of the date values.
#'
#' @return A character string that represent the inferred format of the date
#'    values.
#'
#' @keywords internal
#'
date_make_format <- function(f1, f2, tmp_sep) {
  if (is.null(f1) && is.null(f2)) {
    stop("Unrecognised date format.\n",
         "Please specify the date format using the 'format' argument.")
  }

  return(paste(c(f1, f2), collapse = tmp_sep))
}


#' Process date variable
#'
#' @param x A object of date class
#'
#' @return A modified input object
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
#' @param target_columns a vector of column names to be standardised
#' @param format a vector of formats to be used when standardising the columns
#'
#' @return a vector of format
#' @keywords internal
#'
date_match_format_and_column <- function(target_columns, format) {
  if (length(target_columns) > 2L && length(format) == 2L) {
    stop("Need to specify one format if all target columns have the same format.",
         "Provide one format per target column, otherwise.")
  }
  if (length(target_columns) >= 1L && length(format) == 1L) {
    warning("Using ", format, " to standardize all columns...", call. = FALSE)
    format <- rep(format, length(target_columns))
  }
  return(format)
}
