
#' Calculate the percentage of missing and other data type values in a vector
#' containing different data types such as numeric, Date, character,
#' logical, date-time, factor.
#'
#' @param x A vector of ones or a combination of various data types.
#' @param type A character with the the vector type.
#'
#' @returns A vector of 5 elements representing the percentage of missing,
#' numeric, date, character, and logical values found in the input vector.
#'
#' @keywords internal
#'
scan_columns <- function(x, type) {
  res <- switch(type,
                double = scan_in_double(x),
                integer = scan_in_integer(x),
                logical = scan_in_logical(x),
                character = scan_in_character(x))
  return(res)
}

#' Scan a data frame to determine the percentage of `missing`, `numeric`,
#'    `Date`, `character`, `logical`, `date-time`, and `factor` values in every
#'    column.
#'
#' @param data A data frame or linelist
#'
#' @returns A data frame or linelist with the same number of rows as the number
#'    of columns of the input data, and 8 column representing the field names,
#'    the percentage of missing, numeric, date, character, logical, date-time,
#'    and factor values in each column.
#'
#' @export
#'
#' @examples
#' scan_result <- scan_data(
#'   data = readRDS(system.file("extdata", "messy_data.RDS",
#'                              package = "cleanepi"))
#' )
#'
#' @details
#' For columns of type character, the detected numeric values could actually be
#' of type Date or date-time. This is because R coerces some Date values into
#' numeric when the date is imported from an MS Excel file.
#'
scan_data <- function(data) {
  types       <- vapply(data, typeof, character(1L))
  scan_result <- vapply(seq_len(ncol(data)), function(col_index) {
    scan_columns(data[[col_index]], types[[col_index]])
  }, numeric(7L))
  scan_result        <- as.data.frame(t(scan_result))
  names(scan_result) <- c("missing", "numeric", "date", "character",
                          "logical", "date-time", "factor")
  scan_result        <- cbind(Field_names = names(data), scan_result)
  return(scan_result)
}

#' Scan through a double column
#'
#' @param x The input vector
#'
#' @return A numeric vector with the proportion of the different types of data
#'    that were detected within the input vector.
#' @keywords internal
#'
scan_in_double <- function(x) {
  are_factor <- are_date <- are_date_time <- are_character <- are_numeric <-
    are_logical <- are_na <- 0.0
  # save the variable length
  n_rows <- length(x)

  # get the proportion of NA
  are_na <- round((sum(is.na(x)) / n_rows), 6L)
  x      <- x[!is.na(x)]

  # doubles are either numeric (attributes = NULL), or Date (has a 'class'
  # attributes = Date), or date-time (has a 'class' attributes = POSIXt)
  if ("class" %in% names(attributes(x))) {
    if ("Date" %in% attributes(x)[["class"]]) {
      are_date <- round((length(x) / n_rows), 6L)
    } else if ("POSIXt" %in% attributes(x)[["class"]]) {
      are_date_time <- round((length(x) / n_rows), 6L)
    }
  } else {
    are_numeric <- round((length(x) / n_rows), 6L)
  }
  return(
    c(are_na, are_numeric, are_date, are_character, are_logical, are_date_time,
      are_factor)
  )
}

#' Scan through an integer column
#'
#' @param x The input vector
#'
#' @return A numeric vector with the proportion of the different types of data
#'    that were detected within the input vector.
#' @keywords internal
#'
scan_in_integer <- function(x) {
  are_factor <- are_date <- are_date_time <- are_character <- are_numeric <-
    are_logical <- are_na <- 0.0
  # save the variable length
  n_rows <- length(x)

  # get the proportion of NA
  are_na <- round((sum(is.na(x)) / n_rows), 6L)
  x      <- x[!is.na(x)]

  # integers are either numeric (attributes = NULL), or factors (has a 'class'
  # and 'levels' attributes)
  if (is.null(attributes(x))) {
    are_numeric <- round((length(x) / n_rows), 6L)
  } else if (identical(names(attributes(x)), c("levels", "class"))) {
    are_factor <- round((length(x) / n_rows), 6L)
  }

  return(
    c(are_na, are_numeric, are_date, are_character, are_logical, are_date_time,
      are_factor)
  )
}

#' Scan through a logical column
#'
#' @param x The input vector
#'
#' @return A numeric vector with the proportion of the different types of data
#'    that were detected within the input vector.
#' @keywords internal
#'
scan_in_logical <- function(x) {
  are_factor <- are_date <- are_date_time <- are_character <- are_numeric <-
    are_logical <- are_na <- 0.0

  # logical are simply logical. We will only determine the %NA and %logical
  # save the variable length
  n_rows <- length(x)

  # get the proportion of NA
  are_na <- round((sum(is.na(x)) / n_rows), 6L)
  x      <- x[!is.na(x)]

  # get the proportion of logical
  are_logical <- round((length(x) / n_rows), 6L)
  return(
    c(are_na, are_numeric, are_date, are_character, are_logical, are_date_time,
      are_factor)
  )
}

#' Scan through a character column
#'
#' @param x The input vector
#'
#' @return A numeric vector with the proportion of the different types of data
#'    that were detected within the input vector.
#' @keywords internal
#'
scan_in_character <- function(x) {
  # There might be, within a character column, values of type:
  # character, numeric, date, date-time, NA, and logical
  # In this function, we check the presence of these different types within a
  # character column.
  # Note that numeric values can actually be of 'Date' or 'date-time' type.
  # Given that any numeric can be converted into Date, we will not check for
  # Date or date-time values within the numeric.

  are_factor <- are_date <- are_date_time <- are_character <- are_numeric <-
    are_logical <- are_na <- 0.0

  # save the variable length
  n_rows <- length(x)

  # get the proportion of NA
  are_na <- round((sum(is.na(x)) / n_rows), 6L)
  x      <- x[!is.na(x)]

  # get double values and evaluate the proportion numeric values
  doubles <- x[!is.na(suppressWarnings(as.double(x)))]
  if (length(doubles) > 0L) {
    are_numeric <- round((length(doubles) / n_rows), 6L)
  }

  # get character values and check for the presence of Date and date-time
  characters <- x[is.na(suppressWarnings(as.double(x)))]
  if (length(characters) > 0L &&
      !is.null(lubridate::guess_formats(characters,
                                        c("ymd", "ydm", "dmy", "mdy", "myd",
                                          "dym", "Ymd", "Ydm", "dmY", "mdY",
                                          "mYd", "dYm")))) {
    # get the proportion of date values
    tmp    <- suppressWarnings(
      as.Date(
        lubridate::parse_date_time(
          characters,
          orders = c("ymd", "ydm", "dmy", "mdy", "myd", "dym", "Ymd", "Ydm",
                     "dmY", "mdY", "mYd", "dYm")
        )
      )
    )
    are_date   <- round((sum(!is.na(tmp)) / n_rows), 6L)
    x          <- x[is.na(tmp)]
    characters <- characters[is.na(tmp)]
  }

  # get the proportion of logical values
  logicals <- toupper(characters) == "TRUE" | toupper(characters) == "FALSE"
  are_logical <- round((sum(logicals) / n_rows), 6L)

  # get the proportion of character values
  are_character <- round((1.0 - (are_na + are_numeric +
                                   are_date + are_logical)), 6L)

  # return the output
  return(
    c(are_na, are_numeric, are_date, are_character, are_logical, are_date_time,
      are_factor)
  )
}
