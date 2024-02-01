#' Convert provided or auto-detected character columns into numeric
#'
#' When the names of the columns to be converted is not provided, the function
#' looks for columns that contains only missing, numeric and character values
#' and where the percentage of numeric values is at least 2 times the percent of
#' character values.
#'
#' @param data the input data frame
#' @param to_numeric a vector of column names
#' @param scan_res a data frame obtained from the `scan_data()` function
#'
#' @return the input data frame where all the specified or the detected columns
#'    (as described above) have been converted into numeric
#' @export
#'
#' @examples
#' dat <- convert_to_numeric(
#'   data = readRDS(system.file("extdata", "messy_data.RDS",
#'                              package = "cleanepi")),
#'   to_numeric = "age",
#'   scan_res   = NULL
#' )
convert_to_numeric <- function(data,
                               to_numeric = NULL,
                               scan_res   = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_vector(to_numeric, any.missing = FALSE, min.len = 0L,
                           null.ok = TRUE)
  checkmate::assert_data_frame(scan_res, min.rows = 1L, min.cols = 1L,
                               null.ok = TRUE)
  if (is.null(to_numeric)) {
    if (is.null(scan_res)) {
      scan_res   <- scan_data(data = data)
    }
    to_numeric   <- detect_columns_to_convert(scan_res)
  }

  if (length(to_numeric) > 0L) {
    data <- data %>%
      dplyr::mutate(dplyr::across({{ to_numeric }}, ~ convert(.x)))
    data <- add_to_report(x     = data,
                          key   = "converted_into_numeric",
                          value = paste(to_numeric, collapse = ", "))
  }

  return(data)
}

#' Detect the numeric columns that appears as characters due to the presence of
#' some character values in the column.
#'
#' @param scan_res a data frame that corresponds to the result from the
#'    `scan_data()` function
#'
#' @return a vector of column names to be converted into numeric
#' @keywords internal
#'
detect_columns_to_convert <- function(scan_res) {
  checkmate::assert_data_frame(scan_res, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  to_numeric <- vector(mode = "character", length = 0L)
  for (col in names(scan_res)[-1L]) {
    values        <- scan_res[[col]]
    names(values) <- scan_res[["data_type"]]
    values        <- values[values > 0L]
    if ("missing" %in% names(values)) {
      values <- values[-(which(names(values) == "missing"))]
    }
    if (length(values) == 2L && "numeric" %in% names(values) &&
        "character" %in% names(values)) {
      if (values[["numeric"]] == values[["character"]] ||
          values[["numeric"]] < (2L * values[["character"]])) {
        warning(sprintf("In '%s' column, the number of numeric values is same as
                        the number of character values", col), call. = FALSE)
      } else {
        to_numeric <- c(to_numeric, col)
      }
    } else {
      next
    }
  }
  return(to_numeric)
}

#' Convert values in a character vector into numeric
#'
#' The conversion is only applied on non-missing and non-numeric values found
#' from the input vector
#'
#' @param x a vector of type character
#'
#' @return a vector of type numeric with the same length as the input vector
#' @keywords internal
#'
convert <- function(x) {
  tmp   <- x
  is_na <- which(is.na(x))
  x     <- suppressWarnings(as.numeric(x))
  if (length(is_na) > 0L) {
    xx  <- which(is.na(x))
    y   <- xx[!(xx %in% is_na)]
  }
  converted <- unlist(lapply(tmp[y], numberize::numberize, lang = "en"))
  x[y]      <- converted
  return(x)
}
