#' Scan through a data frame and return the proportion of `missing`, `numeric`,
#' `Date`, `character`, `logical` values.
#'
#' The function checks for the existence of character columns in the data. When
#' found, it reports back the proportion of the data types mentioned above in
#' those columns. See the details section to know more about how it works.
#'
#' @param data A data frame or linelist
#'
#' @returns A data frame if the input data contains columns of type character.
#'    It invisibly returns \code{NA} otherwise. The returned data frame will
#'    have the same number of rows as the number of character columns, and six
#'    columns representing their column names, proportion of missing, numeric,
#'    date, character, and logical values.
#'
#' @details
#' How does it work?
#' The \code{character} columns are identified first. When there is no
#' character column the function returns a message.
#' For every character column, we count:
#' 1. the number of missing data \code{NA}
#' 2. the number of numeric values. A process of detecting valid dates among the
#' numeric values is then initiated using \code{lubridate::as_date()} and
#' \code{date_guess()} functions. If found, a warning is triggered to alert on
#' the presence and ambiguous (numeric values that are potentially date) values.
#' NOTE: A date is considered valid in this case if it falls within the interval
#' of today's date and 50 years back from today.
#' 3. detect the Date values from the non-numeric using the \code{date_guess()}
#' function. The date count is the sum of dates identified from numeric and
#' non-numeric values. Because of the overlap between numeric and date, the sum
#' across the rows in the scanning result might be greater than 1.
#' 4. count the logical values.
#' The remaining values will be those of type characters.
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
  types <- vapply(data, typeof, character(1L))
  target_columns <- which(types == "character")

  # send an message if there is no character column found within the input data
  if (length(target_columns) == 0L) {
<<<<<<< HEAD
    cli::cli_alert_info("No character column found from the input data.")
=======
    cli::cli_alert_info("No character column found in the provided data.")
>>>>>>> f0a5d17 (account for possible dates within numeric values)
    return(invisible(NA))
  }

  # unclass the data to prevent from warnings when dealing with linelist, and
  # scan through the character columns
  data <- as.data.frame(data)[, target_columns, drop = FALSE]
  scan_result <- vapply(seq_len(ncol(data)), function(col_index) {
    scan_in_character(data[[col_index]], names(data)[[col_index]])
  }, numeric(5L))
  scan_result <- as.data.frame(t(scan_result))
  names(scan_result) <- c("missing", "numeric", "date", "character", "logical")
  scan_result <- cbind(Field_names = names(data), scan_result)
  rownames(scan_result) <- NULL
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
  # There might be, in addition to missing values, within a character column,
  # values of type: character, numeric, date (date or date-time), and logical
  # In this function, we check the presence of these different types within a
  # character column.

  # save the variable length
  # the character count is decreased by the number of occurrence a different
  # data type is found.
  initial_length <- character_count <- length(x)

  # get the count of missing data (NA)
  na_count <- sum(is.na(x))
  x <- x[!is.na(x)]
  character_count <- character_count - na_count

  # convert to numeric to determine the numeric count
  # store the index of numeric values for comparison with index of date values
  # values that are both numeric and date will be considered as ambiguous and
  # will trigger a warning
  are_numeric <- suppressWarnings(as.numeric(x))
  numeric_count <- sum(!is.na(are_numeric))
  character_count <- character_count - numeric_count

  # Some numeric values might actually be of type Date. This is the case for
  # data imported into R from MS Excel. We use:
  # 1. lubridate parser on these values to convert them into Date using the
  # date-time for 1900-01-01 UTC in POSIXct format as origin for modern
  # MS Excel.
  # 2. the date guesser implemented for date standardisation.
  # Then detect the valid date values. A date is considered valid in this case
  # if it falls within the interval of today's date and 50 years back from
  # today's.
  # Values identified as date will add to the date count as well.
  from_direct_conversion <- lubridate::as_date(
    are_numeric[!is.na(are_numeric)],
    origin = as.Date("1900-01-01")
  )
  from_guesser <- date_guess(x[!is.na(are_numeric)], x_name)[["res"]]
  if (!inherits(from_guesser, "Date")) {
    from_guesser <- rep(lubridate::NA_Date_, length(from_guesser))
  }
  oldest_date <- Sys.Date() - lubridate::years(50)
  conversion_valid_date <- from_direct_conversion >= oldest_date &
    from_direct_conversion <= Sys.Date()
  guesser_valid_date <- from_guesser >= oldest_date & from_guesser <= Sys.Date()
  ambiguous_count <- sum(
    conversion_valid_date | guesser_valid_date,
    na.rm = TRUE
  )
  # send a warning about the presence of ambiguous values found on that column
  if (ambiguous_count > 0) {
    cli::cli_alert_warning(
      "Found {ambiguous_count} numeric values that can also be of type Date \\\
       in column `{x_name}`."
    )
  }
  x <- x[is.na(are_numeric)]

  # We will check if there is any Date values within the remaining character
  # values by parsing them via the date_guess() used in standardize_dates().
  pure_date_count <- 0
  are_date <- date_guess(x, x_name)[["res"]]
  if (inherits(are_date, "Date")) {
    pure_date_count <- sum(!is.na(are_date))
    x <- x[is.na(are_date)]
    character_count <- character_count - pure_date_count
  }
  # the date count is count of actual date and date inferred from numeric
  date_count <- pure_date_count + ambiguous_count

  # get logical count
  # The logical count is the number of TRUE and FALSE written in both lower
  # and upper cases within the variable.
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
