#' Detects whether a string contains only numbers or not.
#'
#' @param x A string of numbers
#'
#' @returns \code{TRUE} if the string only contains numbers, \code{FALSE}
#'    otherwise
#' @keywords internal
numbers_only <- function(x) {
  return(!grepl("\\D", x))
}

#' Split a string based on a pattern and return the first element of the
#' resulting vector.
#'
#' @param x A string of interest
#' @param sep A separator in the string of interest
#'
#' @returns A string with the first element of the vector returned by the
#'    \code{strsplit()} function.
#' @keywords internal
date_get_part1 <- function(x, sep) {
  if (is.na(x)) {
    return(NA)
  }
  return(unlist(strsplit(x, sep, fixed = TRUE))[[1L]])
}

#' Get part2 of date value
#'
#' @param x A string of interest
#' @param sep A separator in the string of interest
#'
#' @returns A string with the second element of the vector returned by the
#'    \code{strsplit()} function.
#' @keywords internal
date_get_part2 <- function(x, sep) {
  if (is.na(x)) {
    return(NA)
  }
  xx <- unlist(strsplit(x, sep, fixed = TRUE))
  if (length(xx) > 1L) {
    return(xx[[2L]])
  } else {
    return(NA)
  }
}

#' Get part3 of date value
#'
#' @param x A string of interest
#' @param sep A separator in the string of interest
#'
#' @returns A string with the third element of the vector returned by the
#'    \code{strsplit()} function.
#' @keywords internal
#'
date_get_part3 <- function(x, sep) {
  if (is.na(x)) {
    return(NA)
  }
  xx <- unlist(strsplit(x, sep, fixed = TRUE))
  if (length(xx) > 2L) {
    return(xx[[3L]])
  } else {
    return(NA)
  }
}

#' Get sum of numbers from a string
#'
#' @param x A string of interest
#'
#' @returns A numeric that correspond to the sum of every digit in the provided
#'    string.
#' @keywords internal
#'
get_sum <- function(x) {
  if (nchar(x) == 2L) {
    x <- sum(as.numeric(substr(x, 1L, 1L)), as.numeric(substr(x, 2L, 2L)))
  }
  return(x)
}

#' Add an element to the report object
#'
#' @param x A data frame  or linelist
#' @param key The name of the cleaning operation
#' @param value The object to add to the report object
#'
#' @returns The input report object with an additional element
#' @export
#'
#' @examples
#' # scan through the data
#' scan_res <- scan_data(
#'   data = readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
#' )
#'
#' # Perform data cleaning
#' cleaned_data <- clean_data(
#'   data = readRDS(
#'     system.file("extdata", "test_df.RDS", package = "cleanepi")
#'   ),
#'   to_numeric = list(target_columns = "sex", lang = "en"),
#'   dictionary = NULL
#' )
#'
#' # add the data scanning result to the report
#' cleaned_data <- add_to_report(
#'   x = cleaned_data,
#'   key = "scanning_result",
#'   value = scan_res
#' )
#'
add_to_report <- function(x, key, value = NULL) {
  checkmate::assert_data_frame(x, min.rows = 1L, min.cols = 1L, null.ok = FALSE)
  checkmate::assert_character(key, any.missing = FALSE, len = 1L,
                              null.ok = FALSE)
  report                    <- attr(x, "report")
  report[[key]]             <- value
  attr(x, which = "report") <- report
  return(x)
}

#' Get the names of the columns from which duplicates will be found
#'
#' @param data A data frame or linelist
#' @param target_columns A vector of column names. For linelist data, this can
#'    be \code{linelist_tags}
#' @param cols A vector of empty and constant columns
#'
#' @returns A vector with the target column names or indexes
#'
#' @keywords internal
#'
get_target_column_names <- function(data, target_columns, cols) {
  # if NULL return all column names
  if (is.null(target_columns)) {
    target_columns <- names(data)
  }

  # extract column names if target_columns is a vector of column names
  if (length(target_columns) == 1L && target_columns != "linelist_tags") {
    idx            <- match(target_columns, names(data))
    stopifnot("Could not find some specified target column names" =
                !anyNA(idx))
    target_columns <- names(data)[idx]
  }

  # extract column names if target_columns is a vector of column indexes
  if (is.numeric(target_columns)) {
    index          <- seq_along(data)
    stopifnot("Incorrect vector of column name indices provided!" =
                all(target_columns %in% index))
    target_columns <- names(data)[target_columns]
  }

  # check for linelist object if target_columns='tags'
  if (identical(target_columns, "linelist_tags")) {
    stopifnot(
      "'linelist_tags' only works on linelist object. Please provide a vector of
              column names if you are dealing with a data frame" =
        inherits(data, "linelist")
    )
    original_tags  <- linelist::tags(data)
    target_columns <- as.character(original_tags)
  }

  # check whether target columns are part of the empty or constant columns
  if (!is.null(cols)) {
    idx              <- match(cols, target_columns)
    if (length(idx) > 0L) {
      target_columns <- target_columns[-idx]
      stopifnot("All specified columns are either constant or empty." =
                  length(target_columns) > 0L)
    }
  }

  return(target_columns)
}
