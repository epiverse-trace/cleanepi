
#' Calculate the percentage of missing and other data type values from a vector
#' of factor or logical values.
#'
#' @param x A vector of ones or a combination of various data types.
#'
#' @returns A vector of 5 elements representing the percentage of missing,
#' numeric, date, character, and logical values found in the input vector.
#'
#' @keywords internal
#'
scan_lgl_and_fct_columns <- function(x) {
  ## for logical and factor columns, we will use their `levels` to determine
  ## the proportion of the different types
  are_numeric <- are_date <- are_logical <- are_character <- 0L

  # save the variable length
  n_rows <- length(x)

  # get the proportion of NA
  are_na <- round((sum(is.na(x)) / n_rows), 6L)
  x      <- x[!is.na(x)]

  # get the proportion of logical values
  if (is.logical(x)) {
    are_logical <- round((sum(x) + sum(!x)) / n_rows, 6L)
    return(c(are_na, are_numeric, are_date, are_character, are_logical))
  }

  # get the proportion of numeric values
  l <- levels(x)
  numeric_levels <- l[!is.na(suppressWarnings(as.numeric(l)))]
  if (length(numeric_levels) > 0L) {
    are_numeric <- round((sum(x %in% numeric_levels) / n_rows), 6L)
  }

  date_levels <- l[!is.na(suppressWarnings(
    as.Date(
      lubridate::parse_date_time(
        l,
        orders = c("ymd", "ydm", "dmy", "mdy", "myd", "dym", "Ymd", "Ydm",
                   "dmY", "mdY", "mYd", "dYm")
      )
    )
  ))]
  if (length(date_levels) > 0L) {
    are_date <- round((sum(x %in% date_levels) / n_rows), 6L)
  }

  are_character <- round((1.0 - (are_na + are_numeric +
                                   are_date + are_logical)), 6L)

  # return the output
  return(c(are_na, are_numeric, are_date, are_character, are_logical))
}


#' Calculate the percentage of missing and other data type values in a vector
#' containing different data types such as numeric, Date, character, and
#' logical.
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
  ## The processing path is determine with the logical `verdict`
  type    <- unlist(strsplit(type, ","))
  verdict <- type %in% c("logical", "factor")

  ## logical and factor are processed differently from the other.
  ## Logical vectors are expected to contain `TRUE` or `FALSE` or `NA`. For
  ## columns of this type, we will check the %NA, %TRUE and %FALSE. The others
  ## will be set at 0.
  ## For factor columns, the processing is based on their levels. Levels will be
  ## checked for numeric, date, logical, NA, and character
  if (any(verdict)) {
    return(scan_lgl_and_fct_columns(x))
  }

  # save the variable length
  n_rows <- length(x)

  # get the proportion of NA
  are_na <- round((sum(is.na(x)) / n_rows), 6L)
  x      <- x[!is.na(x)]

  # get the proportion of date values
  are_date <- 0L
  if (any(type %in% c("POSIXct", "POSIXt"))) {
    tmp      <- as.Date(x)
    are_date <- round((sum(!is.na(tmp)) / n_rows), 6L)
    x        <- x[is.na(tmp)]
  }
  if (!is.null(lubridate::guess_formats(x, c("ymd", "ydm", "dmy", "mdy", "myd",
                                             "dym", "Ymd", "Ydm", "dmY", "mdY",
                                             "mYd", "dYm")))) {
    tmp    <- suppressWarnings(
      as.Date(
        lubridate::parse_date_time(
          x, orders = c("ymd", "ydm", "dmy", "mdy", "myd", "dym", "Ymd", "Ydm",
                        "dmY", "mdY", "mYd", "dYm")
        )
      )
    )
    are_date <- round((sum(!is.na(tmp)) / n_rows), 6L)
    x        <- x[is.na(tmp)]
  }

  # get the proportion of numeric values
  are_numeric <- are_logical <- 0L
  if (length(x) > 0L) {
    tmp         <- suppressWarnings(as.numeric(x))
    are_numeric <- round((sum(!is.na(tmp)) / n_rows), 6L)
  }

  # get the proportion of character values
  are_character   <- 0L
  if (!all(verdict)) {
    are_character <- round((1.0 - (are_na + are_numeric +
                                     are_date + are_logical)), 6L)
  }

  # return the output
  return(c(are_na, are_numeric, are_date, are_character, are_logical))
}

#' Get class of a vector
#'
#' @param x The input vector
#'
#' @return A character with the class of the input vector
#' @keywords internal
#'
get_class <- function(x) {
  paste(class(x), collapse = ",")
}

#' Scan a data frame to determine the percentage of `missing`, `numeric`,
#'    `Date`, `character`,  and `logical` values in every column.
#'
#' @param data A data frame or linelist
#'
#' @returns A  data frame or linelist with the same columns as the input data
#'  and 5 rows representing the percentage of missing, numeric, date, character,
#'   and logical values in each column.
#'
#' @export
#'
#' @examples
#' scan_result <- scan_data(
#'   data = readRDS(system.file("extdata", "messy_data.RDS",
#'                              package = "cleanepi"))
#' )
scan_data <- function(data) {
  # when scanning through the data, logical and factor columns will be treated
  # differently from the others. It means only the percent of missing and Date
  # values will be evaluated for these columns.
  # The percent of numeric and character value will be set automatically to 0 as
  # to prevent from the effects of the conversion to numeric and character.
  types       <- vapply(data, get_class, character(1L))
  scan_result <- vapply(seq_len(ncol(data)), function(col_index) {
    scan_columns(data[[col_index]], types[[col_index]])
  }, numeric(5L))
  scan_result        <- as.data.frame(t(scan_result))
  names(scan_result) <- c("missing", "numeric", "date", "character",
                             "logical")
  scan_result        <- cbind(Field_names = names(data), scan_result)
  return(scan_result)
}
