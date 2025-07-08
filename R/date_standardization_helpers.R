#' Check date time frame
#'
#' @param first_date A \code{<Date>} object specifying the first valid date.
#' Default is fifty years before the \code{last_date}.
#' This can also be a character in ISO8601 format, i.e., "2024-12-31".
#' @param last_date A \code{<Date>} object specifying the last valid date.
#' Default is the current date. This can also be a character in
#' ISO8601 format, i.e., "2024-12-31".
#'
#' @returns A \code{<list>} with the first and last dates
#' @keywords internal
#'
date_check_timeframe <- function(first_date, last_date) {

  # make sure that they are single character strings in ISO 8601 format.
  iso_8601 <- "[0-9]{4}-(0|1(?=[0-2]))[0-9]-([0-2]|3(?=[0-1]))[0-9]"

  # convert 'first_date' into Date in cases where it is provided as a
  # character.
  first_date_is_character <- is.character(first_date)
  first_date_has_len_1 <- length(first_date) == 1L
  first_date_has_iso_8601 <- grepl(iso_8601, first_date, perl = TRUE)
  verdict <- first_date_is_character & first_date_has_len_1 &
    first_date_has_iso_8601
  if (verdict) {
    first_date <- as.Date(first_date, "%Y-%m-%d")
  }

  # convert 'last_date' into Date in cases where it is provided as a
  # character.
  last_date_is_character <- is.character(last_date)
  last_date_has_len_1 <- length(last_date) == 1L
  last_date_has_iso_8601 <- grepl(iso_8601, last_date, perl = TRUE)
  verdict <- last_date_is_character & last_date_has_len_1 &
    last_date_has_iso_8601
  if (verdict) {
    last_date <- as.Date(last_date, "%Y-%m-%d")
  }

  # When 'first_date' is not provided, set it to 50 years before the last date
  if (is.null(first_date) && inherits(last_date, "Date")) {
    first_date <- last_date - lubridate::years(50)
  }

  # both 'first_date' and 'last_date' are expected to be of type Date
  if (!inherits(first_date, "Date") || !inherits(last_date, "Date")) {
    cli::cli_abort(c(
      tr_("Unexpected format in the function arguments."),
      i = tr_("{.emph first_date} and {.emph last_date} must be of type {.cls Date} or {.cls character} written in {.emph ISO8601} format ('2024-12-31' for December 31, 2024).") # nolint: line_length_linter
    ))
  }
  return(list(first_date, last_date))
}

#' Trim dates outside of the defined timeframe
#'
#' @param new_dates A \code{<vector>} of the new date values
#' @param dmin A \code{<Date>} value with the minimum date
#' @param dmax A \code{<Date>} value with the maximum date
#' @param original_dates A \code{<vector>} of original dates (to be collected
#'    for errors)
#' @param cols A \code{<character>} with the name of the date column of interest
#' @param original_dates A \code{<vector>} of the original date values
#'
#' @returns A \code{<list>} of 2 elements: the update input vector where date
#'    values that are out of the specified timeframe are replaced by \code{NA},
#'    and a vector of the out of timeframe values.
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
    out_of_boundaries <- data.frame(idx = idx,
                                    column = rep(cols, length(idx)),
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
#' @param cols A \code{<Date>} column name(s)
#'
#' @returns A \code{<list>} with the following two elements: a data frame where
#'    the specified columns have been converted into \code{<Date>} values, a
#'    boolean that tells whether numeric values that can also be of type
#'    \code{<Date>} are found in the specified columns.
#'
#' @keywords internal
#'
date_convert <- function(data, cols, error_tolerance,
                         timeframe = NULL, orders) {
  # Guess the date using lubridate (for actual dates and numbers) and the
  # guesser we developed
  has_ambiguous_values <- FALSE

  # declare a variable to store the reports from this operation
  date_standardisation <- NULL

  old_dates <- new_dates <- data[[cols]]
  if (!inherits(data[[cols]], "Date")) {
    date_guess_res <- date_guess(
      old_dates,
      orders = orders,
      column_name = cols
    )
    new_dates <- date_guess_res[["res"]]
    multi_format_dates <- date_guess_res[["multi_format"]]
    has_ambiguous_values <- date_guess_res[["found_ambiguous"]]

    # report the multi formatted dates if they were detected
    if (!is.null(multi_format_dates)) {
      date_standardisation[["multi_format_dates"]] <- multi_format_dates
    }
  }

  # Trim outliers i.e. date values that are out of the range of the provided
  # timeframe
  if (!is.null(timeframe)) {
    res <- date_check_outsiders(data, timeframe, new_dates, cols)
    new_dates <- res[["new_date"]]
    outsiders <- res[["outsiders"]]
    # report the out of range dates
    if (!is.null(outsiders)) {
      date_standardisation[["out_of_range_dates"]] <- outsiders
    }
  }

  # add to the report
  if (!is.null(date_standardisation)) {
    data <- add_to_report(
      x = data,
      key = "date_standardization",
      value = date_standardisation
    )
  }


  # Check whether to tolerate the amount of NA introduced during the process
  na_before <- sum(is.na(old_dates))
  na_after <- sum(is.na(new_dates))
  prop_successful <- (nrow(data) - na_after) / (nrow(data) - na_before)
  if (prop_successful >= (1L - error_tolerance)) {
    data[[cols]] <- new_dates
  } else {
    cli::cli_inform(c(
      "!" = tr_("{.field {cols}} cannot be converted into {.cls Date} due to:"),
      "*" = tr_("insuffisient {.cls Date} values"),
      "*" = tr_("or a high number of values that are outside the specified time frame.") # nolint: line_length_linter
    ))
  }

  return(list(
    data = data,
    has_ambiguous_values = has_ambiguous_values
  ))
}


#' Convert and update  date values
#'
#' @inheritParams standardize_dates
#' @param new_dates A \code{<vector>} of the converted date values
#' @param cols A \code{<character>} with the names of the date column to be
#'    converted
#'
#' @returns A \code{<list>} of 2 data frames: the updated input data (if some
#'    columns were converted to Date) and a data frame of date values that are
#'    not within the specified timeframe.
#' @keywords internal
#'
date_check_outsiders <- function(data, timeframe, new_dates, cols) {
  timeframe <- date_check_timeframe(timeframe[[1L]], timeframe[[2L]])
  if (!inherits(new_dates, "Date")) {
    return(list(
      new_date = new_dates,
      outsiders = NULL
    ))
  }
  new_dates <- date_trim_outliers(new_dates,
                                  timeframe[[1L]], timeframe[[2L]],
                                  cols, data[[cols]])

  outsiders <- new_dates[["outsiders"]]
  if (all(is.na(new_dates[["new_dates"]]))) {
    outsiders <- NULL
  }
  report <- attr(data, "report")
  if ("date_standardization" %in% names(report)) {
    outsiders <- rbind(
      report[["date_standardization"]][["out_of_range_dates"]],
      outsiders
    )
  }

  return(list(new_date = new_dates[["new_dates"]], outsiders = outsiders))
}

#' Guess if a character vector contains Date values, and convert them to date
#'
#' @param data A \code{<data.frame>}
#' @inheritParams standardize_dates
#'
#' @returns A \code{<list>} with the following two elements: the input data
#'    frame where the character columns with date values have been converted
#'    into \code{<Date>}, and a vector of column names where there are numeric
#'    values that can also be of type Date.
#' @keywords internal
#'
date_guess_convert <- function(data, error_tolerance, timeframe,
                               orders) {
  # detect columns of type character, factor, POSIXt and Date
  are_posix <- which(vapply(data, inherits, logical(1), "POSIXt"))
  are_characters <- which(vapply(data, inherits, logical(1), "character"))
  are_factors <- which(vapply(data, inherits, logical(1), "factor"))
  are_dates <- which(vapply(data, inherits, logical(1), "Date"))

  # POSIXt columns are directly converted into Date
  for (i in are_posix) {
    data[[i]] <- as.Date(data[[i]])
  }

  # Both 4 types are subjected to the date guesser
  of_interest <- c(are_characters, are_factors, are_dates, are_posix)
  multi_format_dates <- NULL

  # declare a variable to store the reports from this operation
  date_standardisation <- NULL

  # set the variable to store the ambiguous column
  ambiguous_cols <- NULL
  for (i in names(of_interest)) {
    # save the original vector. this will be used later to compare the number
    # of NA before and after guessing
    old_dates <- data[[i]]

    # perform date guessing
    date_guess_res <- date_guess(
      old_dates,
      orders = orders,
      column_name = i
    )
    new_dates <- date_guess_res[["res"]]
    multi_format <- date_guess_res[["multi_format"]]
    multi_format_dates <- c(multi_format_dates, list(multi_format))
    if (date_guess_res[["found_ambiguous"]]) {
      ambiguous_cols <- c(ambiguous_cols, i)
    }

    # check for dates that are outside of the defined timeframe
    if (!is.null(timeframe)) {
      res <- date_check_outsiders(data, timeframe, new_dates, i)
      new_dates <- res[["new_date"]]
      if (!is.null(res[["outsiders"]])) {
        data <- add_to_report(
          x = data,
          key = "out_of_range_dates",
          value = res[["outsiders"]]
        )
      }
    }

    # Check whether to tolerate the amount of NA introduced during the process
    na_before <- sum(is.na(old_dates))
    na_after <- sum(is.na(new_dates))
    prop_successful <- (nrow(data) - na_after) / (nrow(data) - na_before)
    if (prop_successful >= (1L - error_tolerance)) {
      data[[i]] <- new_dates
    }
  }

  # report the multi formatted dates
  if (!is.null(multi_format_dates)) {
    multi_format_dates <- dplyr::bind_rows(multi_format_dates)
    date_standardisation[["multi_format_dates"]] <- multi_format_dates
  }

  # add to the report
  data <- add_to_report(
    x = data,
    key = "date_standardization",
    value = date_standardisation
  )

  return(list(
    data = data,
    ambiguous_cols = ambiguous_cols
  ))
}

#' Detect complex date format
#'
#' @param x A \code{<character>} with the string of interest
#'
#' @returns A \code{<character>} with the inferred format.
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
  return(paste(c(f1, f2), collapse = tmp_sep))
}

#' Detect a date format with only 1 separator
#'
#' @param x A \code{<character>} with the string of interest
#'
#' @returns A \code{<character>} with the identified format.
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

#' Detect the special character that is the separator in the date values
#'
#' @param x A \code{<character>} with the string of interest
#' @returns A \code{<character>} with the detected separator
#'
#' @returns A \code{<vector>} of the identified special characters.
#' @keywords internal
#'
date_detect_separator <- function(x) {
  sep <- NULL
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
#' @param x A \code{<character>} with the input string
#'
#' @returns A \code{<character>} with the abbreviation used to designate the
#'    written day or month
#' @keywords internal
#'
date_detect_day_or_month <- function(x) {
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
  return(f1)
}

#' Get format from a simple Date value
#'
#' @param x A \code{<character>} with the date value of interest
#'
#' @returns A \code{<character>} with the abbreviation that correspond to the
#'    Date value
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
    cli::cli_abort(c(
      tr_("Expected values with the same format."),
      x = tr_("You've tried to convert values in different formats into {.cls Date}."), # nolint: line_length_linter
      i = tr_("Please specify the formats encountered in your column of interest via the {.emph format} argument.") # nolint: line_length_linter
    ))
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
#' @param x A \code{<vector>} of characters
#'
#' @return A \code{<character>} with the inferred date format
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
  list_elt_lengths <- as.numeric(lapply(tmp_list, length))
  add_na <- function(y, n) return(c(y, rep(NA, n - length(y))))
  tmp_list <- lapply(tmp_list, add_na, max(list_elt_lengths))

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
  return(date_make_format(f1, f2, f3))
}

#' Build the auto-detected format
#'
#' Put together the different date format characters that were identified from
#' the target date column.
#'
#' @param f1 A \code{<character>} with the first part of the inferred format
#' @param f2 A \code{<character>} with the second part of the inferred format
#' @param f3 A \code{<character>} with the third part of the inferred format
#'
#' @return A \code{<character>} that represents the inferred format from the
#'    provided elements. It returns \code{<NULL>} when the format was not
#'    resolved.
#'
#' @keywords internal
#'
date_make_format <- function(f1, f2, f3) {
  verdict <- is.null(f1) | is.null(f2) | is.null(f3) |
    length(is.na(c(f1, f2, f3))) >= 2L
  if (verdict) {
    return(NULL)
  }

  idx <- which(is.na(c(f1, f2, f3)))
  if (length(idx) == 0L) {
    return(paste0(f1, "-", f2, "-", f3))
  } else if (idx == 3L) {
    return(paste0(f1, "-", f2))
  } else {
    return(NULL)
  }
}


#' Process date variable
#'
#' @param x A \code{<Date>} object
#'
#' @return The converted input value into \code{<Date>} or \code{<character>}
#' @keywords internal
#'
date_process <- function(x) {
  # If the input is a date already: no guessing needed!
  if (inherits(x, c("Date", "POSIXt", "aweek"))) {
    return(as.Date(x))
  }

  if (is.factor(x) || is.numeric(x)) {
    x <- as.character(x)
  }

  if (!is.character(x)) {
    cli::cli_abort(c(
      tr_("Unexpected data type provided to {.fn date_guess} function."),
      i = tr_("You can convert the values into {.cls character} to enable format guessing."), # nolint: line_length_linter
      x = tr_("You've tried to guess the date format from values of type other than Date and character.") # nolint: line_length_linter
    ))
  }

  return(x)
}

#' Check whether the number of provided formats matches the number of target
#' columns to be standardized.
#'
#' @param target_columns A \code{<vector>} of column names to be standardized
#' @param format A \code{<vector>} of formats to be used when standardizing the
#'    columns
#'
#' @return A \code{<vector>} of characters with the validated formats
#' @keywords internal
#'
date_match_format_and_column <- function(target_columns, format) {
  if (length(target_columns) > 2L && length(format) == 2L) {
    cli::cli_abort(c(
      tr_("Unable to match formats to target columns."),
      x = tr_("The number of target columns does not match the number of specified formats."), # nolint: line_length_linter
      i = tr_("Only one format is needed if all target columns contain values of the same format. Otherwise, one format per target column must be provided.") # nolint: line_length_linter
    ))
  }
  if (length(target_columns) >= 1L && length(format) == 1L) {
    cli::cli_alert_info(
      tr_("The target {cli::qty(length(target_columns))} column{?s} will be standardized using the format: {.val {format}}.") # nolint: line_length_linter
    )
    format <- rep(format, length(target_columns))
  }
  return(format)
}
