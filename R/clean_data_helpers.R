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
#' When a numeric value is found in a character column which also contains Date
#' values, the numeric ones which are potentially of type Date (a numeric, which
#  after conversion to Date, fall within the interval
#  [50 years back from today's date, today's date]) will add to the date count
#' as well as to the numeric count. For this reason, the sum across rows in the
#' output object could be greater than 1.
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
  #     When there is one or more Date values, the remaining values are
  # converted into numeric. The first numeric count is recorded at this point.
  # If any of these numeric values is a Date (a numeric, which
  # after conversion to Date, fall within the interval
  # [50 years back from today's date, today's date]), it will added to the date
  # count. That way the Date count is the count of date identified from the
  # parsing + the count of Dates within the numeric values.
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

  # getting the date and numeric count as describe above
  date_count <- numeric_count <- 0L
  if (sum(!is.na(are_date)) > 0L) {
    # Setting the first date to 50 years before the current date
    oldest_date <- seq.Date(Sys.Date(), length.out = 2L, by = "-50 years")[[2L]]

    # get the date count
    date_count <- date_count + sum(!is.na(are_date))
    character_count <- character_count - date_count

    # convert to numeric and check for the presence of Date among the numeric
    non_date <- x[is.na(are_date)]
    are_numeric <- suppressWarnings(as.numeric(non_date))
    character_count <- character_count - sum(!is.na(are_numeric))
    numeric_count <- numeric_count + sum(!is.na(are_numeric))
    if (sum(!is.na(are_numeric)) > 0L) {
      y <- lubridate::as_date(
        are_numeric[!is.na(are_numeric)],
        origin = oldest_date
      )
      # send a warning to inform about the presence of numeric values that could
      # potentially be Date
      if (sum(!is.na(y)) > 0L) {
        # second count of date values coming from date within numeric
        date_count <- date_count + sum(!is.na(y))
        warning(
          sprintf(
            "'%s' contains %d numeric values that are potentially of type Date."
            , x_name,
            sum(!is.na(y))
          ),
          call. = FALSE
        )
      }
    }
  } else {
    are_numeric <- suppressWarnings(as.numeric(x))
    numeric_count <- numeric_count + sum(!is.na(are_numeric))
    character_count <- character_count - numeric_count
  }

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
