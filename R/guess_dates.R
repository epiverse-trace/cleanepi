#' Try and guess dates from a characters
#'
#' Note that THIS FEATURE IS STILL EXPERIMENTAL: we strongly recommend checking
#' a few converted dates manually. This function tries to extract dates from a
#' `character` vector or a `factor`. It treats each entry independently, using
#' regular expressions to detect if a date is present, its format, and if
#' successful it converts that entry to a standard `Date` with the *Ymd* format
#' (e.g. `2018-01-21`). Entries which cannot be processed result in `NA`. An
#' error threshold can be used to define the maximum number of resulting `NA`
#' (i.e. entries without an identified date) that can be tolerated. If this
#' threshold is exceeded, the original vector is returned.
#'
#' @param x A `character` vector or a `factor`
#' @param quiet A logical indicating if messages should be displayed to the
#'     console (`TRUE`, default); set to `FALSE` to silence messages
#' @param column_name The target column name
#' @inheritParams standardize_dates
#'
#' @returns A list of following two elements: a vector of the newly reformatted
#'    dates and a data frame with the date values that were converted from more
#'    than one format. If all values comply with only one format, the later
#'    element will be NULL.
#'
#' @keywords internal
#'
date_guess <- function(x,
                       column_name,
                       quiet           = TRUE,
                       modern_excel    = TRUE,
                       orders          = NULL) {

  ## This function tries converting a single character string into a
  ## well-formatted date, but still returning a character. If it can't convert
  ## it, it returns NA.

  ## The conversion process uses `lubridate::parse_date_time()` under the hood,
  ## but attempts to avoid the specificity problems that lubridate introduces
  ## when you have several date formats you want to test. For example,
  ## lubridate will sometimes parse 04 Feb 1982 as 1982-04-19 because it thinks
  ## that "Feb" is a separator.
  ##
  ## To prevent this, `guess_dates()` takes a list of possible date formats and
  ## parses the entire vector of `x` using each element separately.
  ##
  ## We defined four default formats:
  ##
  ## $quarter_partial_dates (for quarter and partial date values)
  ## [1] "Y" "Ym" "Yq"
  ##
  ## $world_named_months (and other formats derived from them)
  ## [1] "Ybd" "dby"
  ##
  ## $world_digit_months (and other formats derived from them)
  ## [1] "dmy" "Ymd"
  ##
  ## $US_formats
  ## [1] "Omdy" "YOmd"
  ##

  # elements of the 'orders' list represent the date formats which will be used
  # to infer whether a value is of type date or not.
  # The order in which the elements are provided matters as every value will be
  # matched against the first element, then the second, and so on. When a match
  # is found with more than element, the format of the first match is returned,
  # and the value will be registered as a multi-formatted date value in the
  # report object.
  if (is.null(orders)) {
    orders <- list(
      quarter_partial_dates = c("Y", "Ym", "Yq"),
      world_digit_months = c("Yq", "ymd", "ydm", "dmy", "mdy", "myd", "dym",
                             "Ymd", "Ydm", "dmY", "mdY", "mYd", "dYm"),
      world_named_months = c("dby", "dyb", "bdy", "byd", "ybd", "ydb",
                             "dbY", "dYb", "bdY", "bYd", "Ybd", "Ydb"),
      us_format = c("Omdy", "YOmd")
    )
  }

  # Process lubridate order list
  if (!is.list(orders) && is.character(orders)) {
    orders <- list(orders)
  }

  if (!is.list(orders)) {
    stop("`order` must be either a character or a list of character vectors.")
  }

  # Process dates - here we check whether values in the vector are any of the
  # following types: "Date", "POSIXt", "aweek".
  # If the input is already a date: no guessing needed!
  if (inherits(x, c("Date", "POSIXt", "aweek"))) {
    x <- as.Date(x)
    return(x)
  }

  # In this section, we are preventing from the following odd case scenarios:
  # 1. lubridate replacement of links by the dates founds within them
  # 2. guessing date from non-ASCII character values
  # 3. other future odd scenarios
  # links and non-ASCII characters will be replaced by NA before the guessing
  # starts.
  detect_links <- function(y) {
    regex <- "^(https?://)?(www\\.)?([a-z0-9]([a-z0-9]|(\\-[a-z0-9]))*\\.)+[a-z]+$" # nolint: line_length_linter
    domain <- strsplit(gsub("^(https?://)?(www\\.)?", "", y),
                       "/", fixed = TRUE)[[c(1L, 1L)]]
    grepl(regex, domain)
  }

  are_non_ascii <- grepl("[^ -~]", x)
  are_links <- unlist(lapply(x, detect_links))
  are_odd_cases <- are_non_ascii | are_links
  x[are_odd_cases] <- NA

  # guess dates
  # create output data frame for dates
  res        <- list(rep(as.Date(NA_character_), length(x)))
  res        <- rep(res, length(orders))
  names(res) <- names(orders)

  # parsing the vector, looking for actual (no ambiguity about whether it is a
  # date or no) date values. Loop over each set of lubridate orders and find
  # the dates
  for (i in seq_along(orders)) {
    # guess at only the subset of dates
    res[[i]] <- suppressWarnings(
      as.Date(lubridate::parse_date_time(x, orders = orders[[i]]))
    )
  }

  # if lubridate fails to do the job for all or some values, then we should use
  # the parser defined in date_get_format
  x_rescued <- date_rescue_lubridate_failures(
    date_a_frame = data.frame(res),
    original_dates = x,
    mxl = modern_excel
  )

  # select the first correct date generated from the formats listed in the
  # 'orders' argument. If multiple formats are possible, pick the first one and
  # report the remaining as part of the 'multi_format_dates' element of the
  # report object.
  new_x <- date_choose_first_good(x_rescued, column_name = column_name)
  return(new_x)
}



#' Find the dates that lubridate couldn't
#'
#' @param date_a_frame A data frame where each column contains a different
#'    parsing of the same date vector
#' @param original_dates The vector of original dates.
#' @param mxl "modern excel" if TRUE, then it uses 1900 as the origin, otherwise
#'    1904 is used as the origin.
#'
#' @returns The input data frame where the values that do not match the proposed
#'    formats have been converted into Date.
#' @keywords internal
date_rescue_lubridate_failures <- function(date_a_frame, original_dates,
                                           mxl = TRUE) {
  # find places where all rows are missing i.e. values that lubridate could not
  # parse.
  nas <- is.na(date_a_frame)
  all_nas <- apply(nas, 1L, all)

  # convert to numeric to identify numeric values
  o_num <- suppressWarnings(as.integer(original_dates))
  numbers <- !is.na(o_num)

  # find non date and non numeric values (character) values. they will be
  # subjected to the date guesser for character values
  go_tibo <- which(all_nas & !numbers)

  # find non date but numeric. they will be subjected to excel conversion
  go_exel <- all_nas & numbers

  # Use the date guesser on character values
  if (length(go_tibo) && !all(is.na(original_dates[go_tibo]))) {
    date_a_frame[[1L]][go_tibo] <- date_i_guess_and_convert(
      original_dates[go_tibo]
    )
  }

  # Use the excel guesser on numeric values
  if (sum(go_exel)) {
    # using the date-time for 1970-01-01 UTC in POSIXct format as origin for
    # modern exel; "1904-01-01" otherwise.
    origin <- if (mxl) as.Date(lubridate::origin) else as.Date("1904-01-01")
    tmpxl <- lubridate::as_date(o_num[go_exel], origin = origin)
    date_a_frame[[1L]][go_exel] <- tmpxl
  }

  return(date_a_frame)
}

#' Extract date from a character vector
#'
#' This function tries converting a single character string into a
#' well-formatted date, but still returning a character. If it can't convert
#' it, it returns NA.
#'
#' @param x A character vector
#'
#' @returns If the format cannot be resolved, the function returns NA; if a
#'    matching format is found, it returns the converted values.
#' @keywords internal
date_i_guess_and_convert <- function(x) {
  x <- as.character(x)
  format <- date_get_format(x)
  if (is.null(format)) {
    return(NA_character_)
  }
  x <- as.character(as.Date(x, format = format))
  return(x)
}


#' Choose the first non-missing date from a data frame of dates
#'
#' @param date_a_frame A data frame where each column contains a different
#'    parsing of the same date vector
#' @param column_name The target column name
#'
#' @returns The chosen first date value. When there other possible values for a
#'    given date, this will be registered in the report object.
#' @keywords internal
#'
date_choose_first_good <- function(date_a_frame, column_name) {
  multi_format <- NULL
  n <- nrow(date_a_frame)
  res <- rep_len(lubridate::NA_Date_, length.out = n)
  for (i in seq_len(n)) {
    # get values that lubridate and the guesser converted successfully
    tmp        <- date_a_frame[i, , drop = TRUE]
    idx        <- which(!is.na(tmp))
    if (length(idx) > 0L) {
      res[i]   <- as.Date(tmp[idx][[1L]])
      # detect values that comply with multiple formats. Useful for report.
      tmp_date <- unique(tmp[idx])
      if (length(tmp_date) > 1L) {
        multi_format <- c(
          multi_format,
          list(cbind(field = column_name, idx = i,
                     date_a_frame[i, , drop = FALSE]))
        )
      }
    }
  }
  return(list(
    res = res,
    multi_format = dplyr::bind_rows(multi_format)
  ))
}
