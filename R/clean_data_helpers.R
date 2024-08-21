#' Scan through all character columns of a data frame to determine the
#' proportion of `missing`, `numeric`, `Date`, `character`, `logical`, values.
#'
#' @param data A data frame or linelist
#'
#' @returns A data frame if the input data contains columns of type character.
#'    It invisibly returns `NA` otherwise. The returned data frame will have the
#'    same number of rows as the number of character columns, and six
#'    columns representing their column names, proportion of missing, numeric,
#'    date, character, and logical values.
#'
#' @details
#' How does it work?
#' The character columns are identified first. When there is no character column
#' the function returns a message.
#' For every character column, we look for the presence of date values.
#' When Date values are found, the first count of dates is recorded. These Date
#' values will be in turn converted to numeric. If any numeric value is detected
#' among them, the first count of numeric values is recorded.
#' The remaining values are then converted to numeric. The second numeric count
#' will be recorded from this. The detected numeric values will also be
#' converted into Date to identify the ones which are potentially of type Date
#' (a numeric, which after conversion to Date, fall within the interval
#  [50 years back from today's date, today's date]). Those that turns out to be
#  Date values are counted in the second count of dates.
#  For this reason, the sum across rows in the output object could be greater
#  than 1.
#  In the absence of Date values, the entire column is converted into numeric to
#  record the numeric count.
#  The logical and character counts will subsequently be evaluated.
#'
#' @export
#'
#' @examples
#' # scan through a data frame of characters
#' scan_result <- scan_data(
#'   data = readRDS(system.file("extdata", "messy_data.RDS",
#'                              package = "cleanepi"))
#' )
#'
#' # scan through a data frame with two character columns
#' scan_result <- scan_data(
#'   data = readRDS(system.file("extdata", "test_linelist.RDS",
#'                              package = "cleanepi"))
#' )
#'
#' # scan through a data frame with no character columns
#' data(iris)
#' iris[["fct"]] <- as.factor(sample(c("gray", "orange"), nrow(iris),
#'                            replace = TRUE))
#' iris[["lgl"]]  <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)
#' iris[["date"]] <- as.Date(seq.Date(from = as.Date("2024-01-01"),
#'                                    to = as.Date("2024-08-30"),
#'                                    length.out = nrow(iris)))
#' iris[["posit_ct"]] <- as.POSIXct(iris[["date"]])
#' scan_result        <- scan_data(data = iris)
#'
scan_data <- function(data) {
  # scan through all columns of the data and the identify character columns
  types          <- vapply(data, typeof, character(1L))
  target_columns <- which(types == "character")

  # send an message if there is no character column found within the input data
  if (length(target_columns) == 0L) {
    message("No character column found in the provided data.")
    return(invisible(NA))
  }

  # unclass the data to prevent from warnings when dealing with linelist, and
  # scan through the character columns
  data <- as.data.frame(data)[, target_columns, drop = FALSE]
  scan_result <- vapply(seq_len(ncol(data)), function(col_index) {
    scan_in_character(data[[col_index]], names(data)[[col_index]])
  }, numeric(5L))
  scan_result        <- as.data.frame(t(scan_result))
  names(scan_result) <- c("missing", "numeric", "date", "character", "logical")
  scan_result        <- cbind(Field_names = names(data), scan_result)
  rownames(scan_result)     <- NULL
  return(scan_result)
}

#' Scan through a character column
#'
#' @param x The input character vector
#' @param x_name The name of the corresponding column
#'
#' @return A numeric vector with the proportion of the different types of data
#'    that were detected within the input vector.
#' @keywords internal
#'
scan_in_character <- function(x, x_name) {
  # There might be, within a character column, values of type:
  # character, numeric, date (date or date-time), NA, and logical
  # In this function, we check the presence of these different types within a
  # character column.

  # save the variable length
  # the character count is decreased by the number of occurrence a different
  # data type is found.
  initial_length <- character_count <- length(x)

  # get the count of missing data (NA)
  na_count <- sum(is.na(x))
  x      <- x[!is.na(x)]
  character_count <- character_count - na_count

  # We will check if there is any Date values within the variable by parsing the
  # values, looking for the ones that fit any of the predefined format.
  #     When there is one or more Date values, they will be converted into
  # numeric. The first numeric count is recorded at this point. The rest of the
  # values are converted into numeric, and the second count of numeric is
  # recorded. They will in turn be converted into date.
  # If any of these numeric values is a Date (a numeric, which
  # after conversion to Date, fall within the interval
  # [50 years back from today's date, today's date]), it will add to the second
  # date count.
  # That way the Date count is the count of date identified from the
  # parsing + the count of Dates within the numeric values. Similarly, the
  # numeric count is the count of numeric values within dates values and count
  # among the non-date values.
  #
  # NOTE: This is what justifies that the sum across rows in the output object
  # could be > 1.
  #
  #     When there is no Date values identified from the parsing, the variable
  # is converted into numeric. The final numeric count is the sum of all the
  # identified numeric values.
  #     The logical count is the number of TRUE and FALSE written in both lower
  # and upper cases within the variable.
  #     The remaining values will be considered of type character.

  # parsing the vector, looking for date values
  are_date <- suppressWarnings(
    as.Date(
      lubridate::parse_date_time(
        x,
        orders = c("ymd", "ydm", "dmy", "mdy", "myd", "dym", "Ymd", "Ydm",
                   "dmY", "mdY", "mYd", "dYm")
      )
    )
  )

  # when date values are identified, check if they are at the same time numeric
  # and get the count of ambiguous
  # convert the rest to numeric and check if they can also translate to data and
  # get the second count of ambiguous
  date_count <- ambiguous_count <- numeric_count <- 0L
  if (sum(!is.na(are_date)) > 0L) {
    # get the date count and the indices of the date values
    date_count <- sum(!is.na(are_date))
    are_date_idx <- which(!is.na(are_date))

    # convert the date values into numeric and check if some of them are also
    # numeric. If some are, get the first count of ambiguous and numeric values
    are_numeric_in_dates <- suppressWarnings(as.numeric(x[are_date_idx]))
    ambiguous_count <- sum(!is.na(are_numeric_in_dates))
    numeric_count <- ambiguous_count

    # getting out of this condition with non-date values
    character_count <- character_count - date_count
    x <- x[-are_date_idx]

    # convert the remaining values into numeric.
    # then check if any of them can be a date value
    are_numeric <- suppressWarnings(as.numeric(x))
    numeric_count <- numeric_count + sum(!is.na(are_numeric))
    are_numeric_idx <- which(!is.na(are_numeric))
    if (length(are_numeric_idx) > 0L) {
      numeric_values <- as.numeric(x[are_numeric_idx])
      x <- x[-are_numeric_idx]
      character_count <- character_count - length(are_numeric_idx)

      # convert the numeric values into date.
      # If some are date, get the second count of ambiguous and date values

      # Set the first date to 50 years before the current date
      oldest_date <- Sys.Date() - lubridate::years(50)

      # identify potential date values and increment date and ambiguous counts
      date_values <- lubridate::as_date(numeric_values)
      valid_dates <- date_values >= oldest_date & date_values <= Sys.Date()
      if (any(valid_dates)) {
        # get the second count of date and ambiguous values coming from date
        # within numeric
        date_count <- date_count + sum(valid_dates)
        ambiguous_count <- ambiguous_count + sum(valid_dates)
      }
    }

    # send a warning about the number of ambiguous values found on that column
    if (ambiguous_count > 0) {
      cli::cli_alert_warning(
        "Found {ambiguous_count} values that can be either numeric or date in",
        "column `{x_name}`"
      )
    }
  }

  # convert everything to numeric and get the numeric count
  are_numeric <- suppressWarnings(as.numeric(x))
  numeric_count <- numeric_count + sum(!is.na(are_numeric))
  character_count <- character_count - sum(!is.na(are_numeric))

  # get logical count
  logicals <- toupper(x) == "TRUE" | toupper(x) == "FALSE"
  logical_count <- sum(logicals)
  character_count <- character_count - logical_count

  # transform into proportions
  props <- round(
    c(na_count, numeric_count, date_count, character_count, logical_count) /
      initial_length, 4L
  )

  return(props)
}
