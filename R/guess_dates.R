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
#'
#' @author Thibaut Jombart, Zhian N. Kamvar
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
                       orders          = list(
                         world_named_months = c("Ybd", "dby"),
                         world_digit_months = c("dmy", "Ymd"),
                         US_formats         = c("Omdy", "YOmd")
                        )) {

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
  ## There are three default formats:
  ##
  ## $world_named_months
  ## [1] "Ybd" "dby"
  ##
  ## $world_digit_months
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
      world_digit_months = c("ymd", "ydm", "dmy", "mdy", "myd", "dym",
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
    stop("orders must be a list of character vectors")
  }

  # Process dates - here we check whether values in the vector are any of the
  # following types: "Date", "POSIXt", "aweek".
  # If the input is a date already: no guessing needed!
  if (inherits(x, c("Date", "POSIXt", "aweek"))) {
    x <- as.Date(x)
    return(x)
  }

  # Guess dates
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
  res <- data.frame(res)





  ## if lubridate fails to do the job, then we should use thibaut's parser.
  x_rescued  <- date_rescue_lubridate_failures(data.frame(res),
                                               original_dates = x,
                                               mxl            = modern_excel)

  # Select the correct dates and test if we were successful --------------------
  new_x     <- date_choose_first_good(x_rescued, column_name = column_name)
  return(new_x)
}


#' Choose the first non-missing date from a data frame of dates
#'
#' @param date_a_frame A data frame where each column contains a different
#'   parsing of the same date vector
#' @param column_name The target column name
#'
#' @returns The chosen first date value
#' @keywords internal
#'
date_choose_first_good <- function(date_a_frame, column_name) {
  multi_format <- NULL
  n            <- nrow(date_a_frame)
  res          <- rep_len(lubridate::NA_Date_, length.out = n)
  for (i in seq_len(n)) {
    # get values that lubridate converted successfully
    tmp        <- date_a_frame[i, , drop = TRUE]
    idx        <- which(!is.na(tmp))
    if (length(idx) > 0L) {
      res[i]   <- as.Date(tmp[idx][[1L]])
      # detect values that comply with multiple formats. Useful for report.
      tmp_date <- unique(tmp[idx])
      if (length(tmp_date) > 1L) {
        multi_format <- c(multi_format,
                          list(cbind(field = column_name, idx = i,
                                     date_a_frame[i, , drop = FALSE])))
      }
    }
  }
  return(
    list(res          = res,
         multi_format = dplyr::bind_rows(multi_format))
  )
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
  # subjected to thibault's guesser
  go_tibo <- which(all_nas & !numbers)

  # find non date but numeric. they will be subjected to excel conversion
  go_exel <- all_nas & numbers

  # Use Thibaut's guesser
  tmpbo <- rep(as.Date(NA_character_), length(go_tibo))
  for (i in go_tibo) {
    tmpbo[go_tibo == i] <- date_i_extract_string(original_dates[i])
  }
  date_a_frame[[1L]][go_tibo] <- tmpbo

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


#' Extract date from a character string
#'
#' This function looks for a well-formatted date character string inside a
#' single character string, and returns the matching date using the `%Y-%m-%d`
#' format (e.g. `2018-01-23`).
#'
#' @author Thibaut Jombart
#' @returns Either `NA_character_` or a date, as a standardized character
#'    string.
#' @keywords internal
date_i_extract_string <- function(x) {

  ## This function tries converting a single character string into a
  ## well-formatted date, but still returning a character. If it can't convert
  ## it, it returns NA.

  date_info <- date_i_find_format(x)
  if (is.null(date_info)) {
    return(NA_character_)
  }

  return(as.character(as.Date(date_info[["date"]],
                              format = date_info[["format"]])))
}

#' Guess date format of a character string
#'
#' The motivation behind this is that `as.Date` does not handle correctly its
#' `format` argument, e.g. `as.Date("01-12-2001", format = "%Y-%m-%d")` returns
#' `1-12-20`. Tries to match a single character string against regular
#' expressions representing potential date formats. Returns the format as
#' something that can be processed by `as.Date` if a match is found, and `NULL`
#' otherwise.
#'
#' @author Thibaut Jombart
#'
#' @returns If no matching format can be found, the function returns NULL; if a
#'    matching format is found, the function returned the matched regular
#'    expression (clean date) and its format compatible with `as.Date`.
#' @keywords internal
date_i_find_format <- function(x) {
  x <- as.character(x[[1L]])

  ## define the regular expressions used to find dates

  num       <- "[[:digit:]]"
  letters   <- "[[:alpha:]]"
  separator <- "[[:punct:][:blank:]]+"
  x         <- gsub(separator, "-", x)


  ## These are the formats currently handled; not that any punctuation is
  ## coerced to a single "-" prior to conversion.

  formats <- list(
    ## 2010-01-23
    "%Y-%m-%d" = paste0(num, "{4}", "-",
                        num, "{2}", "-",
                        num, "{2}", collapse = ""),
    ##  23-01-2010
    "%d-%m-%Y" = paste0(num, "{2}", "-",
                        num, "{2}",  "-",
                        num, "{4}", collapse = ""),
    ## 23-Jan-2010
    "%d-%b-%Y" = paste0(num, "{2}", "-",
                        letters, "{3}",  "-",
                        num, "{4}", collapse = ""),
    ## 2010-Jan-23
    "%Y-%b-%d" = paste0(num, "{4}", "-",
                        letters, "{3}",  "-",
                        num, "{2}", collapse = "")
  )


  ## look for these expressions in 'x', return NULL if we don't find anything
  matching <- vapply(formats, function(pattern) {
    any(grepl(pattern, x))
  }, logical(1L))

  idx      <- which(matching)
  if (length(idx) == 0L) {
    return(NULL)
  } else {
    format <- names(which(matching))[[1L]] # only get the first matching format
  }


  ## If we do find the format, extract the clean date (it could be flanked by
  ## garbage), and return a named character vector of length 2, containing the
  ## as.Date compatible 'format', and the clean date itself, as a character.

  expression    <- formats[[format]]
  cleaning_expr <- paste0("^.*(", expression, ").*$")
  clean_date    <- gsub(cleaning_expr, "\\1", x)
  out           <- c(format = format, date = clean_date)
  return(out)
}
