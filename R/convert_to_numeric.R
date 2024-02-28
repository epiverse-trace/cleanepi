#' Convert columns into numeric
#'
#' When the function is invoked without specifying the column names to be
#' converted, it automatically scans for columns containing exclusively missing,
#' numeric, and character values. Furthermore, it identifies columns where the
#' proportion of numeric values is at least twice the percentage of character
#' values.
#'
#' @param data A data frame
#' @param to_numeric A vector of column names
#' @param scan_res A data frame obtained from the `scan_data()` function
#'
#' @return A resulting data frame after the conversion process,
#' wherein all the specified or detected columns have been transformed into
#' numeric format.
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
    for (col in to_numeric) {
      data[[col]] <- convert(data[[col]])
    }
    data <- add_to_report(x     = data,
                          key   = "converted_into_numeric",
                          value = glue::glue_collapse(to_numeric, sep = ", "))
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
  if (all(is.numeric(x))) {
    return(as.numeric(x))
  }
  tmp    <- x
  is_na  <- which(is.na(x))
  xx     <- suppressWarnings(as.numeric(x))
  after_conversion_is_na <- NULL
  if (length(is_na) > 0L) {
    xx[is_na]              <- "i_set_this"
    after_conversion_is_na <- which(is.na(xx))
  }
  tmp[after_conversion_is_na] <- unlist(lapply(tmp[after_conversion_is_na],
                                               numberize::numberize,
                                               lang = "en"))
  return(as.numeric(tmp))
}
