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
  data               <- as.data.frame(data)[, target_columns]
  scan_result        <- vapply(data, scan_in_character, numeric(5L))
  scan_result        <- as.data.frame(t(scan_result))
  names(scan_result) <- c("missing", "numeric", "date", "character", "logical")
  scan_result        <- cbind(Field_names = rownames(scan_result), scan_result)
  rownames(scan_result)     <- NULL
  return(scan_result)
}

#' Scan through a character column
#'
#' @param x The input character vector
#'
#' @return A numeric vector with the proportion of the different types of data
#'    that were detected within the input vector.
#' @keywords internal
#'
scan_in_character <- function(x) {
  # There might be, within a character column, values of type:
  # character, numeric, date (date or date-time), NA, and logical
  # In this function, we check the presence of these different types within a
  # character column.

  # save the variable length
  initial_length <- length(x)

  # get the count of missing data (NA)
  na_count <- sum(is.na(x))
  x      <- x[!is.na(x)]

  # We will check if there is any Date values within the variable by parsing the
  # values, looking for the ones that fit any of the predefined format.
  #     When there is one or more Date values, we will convert the remaining
  # values into numeric and determine if any of them is a Date (a numeric, which
  # after conversion to Date, fall within the interval
  # [50 years back from today's date, today's date]). That way the Date count is
  # the count of date identified from the parsing + the count of Dates within
  # the numeric values.
  #     When there is no Date values identified from the parsing, the variable
  # is converted into numeric. The numeric count is the sum of numeric values.
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

  # getting the date and numeric count as describe above
  date_count <- numeric_count <- 0L
  if (sum(!is.na(are_date)) > 0L) {
    # Setting the first date to 50 years before the current date
    oldest_date <- seq.Date(Sys.Date(), length.out = 2L, by = "-50 years")[[2L]]

    # get the date count
    date_count <- date_count + sum(!is.na(are_date))

    # convert to numeric and check for the presence of Date among the numeric
    non_date <- x[is.na(are_date)]
    are_numeric <- suppressWarnings(as.numeric(non_date))
    if (sum(!is.na(are_numeric)) > 0L) {
      y <- lubridate::as_date(
        are_numeric[!is.na(are_numeric)],
        origin = oldest_date
      )
      # second count of date values coming from date within numeric
      date_count <- date_count + sum(!is.na(y))
      numeric_count <- sum(is.na(y))
    }
  } else {
    are_numeric <- suppressWarnings(as.numeric(x))
    numeric_count <- sum(!is.na(are_numeric))
  }

  # get logical count
  logicals <- toupper(x) == "TRUE" | toupper(x) == "FALSE"
  logical_count <- sum(logicals)

  # get the character count
  character_count <- initial_length -
    (na_count + logical_count + numeric_count + date_count)

  # transform into proportions
  props <- round(
    c(na_count, numeric_count, date_count, character_count, logical_count) /
      initial_length, 4L
  )

  return(props)
}
