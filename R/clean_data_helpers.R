#' Calculate the percentage of missing and other data type values in a vector
#' containing different data types such as numeric, Date, character, and
#' logical.
#'
#' @param x A vector of ones or a combination of various data types.
#'
#' @return A vector of 5 elements representing the percentage of missing,
#' numeric, date, character, and logical values found in the input vector.
#'
#' @keywords internal
#'
scan_columns <- function(x) {
  # --- save the variable length ---
  n_rows <- length(x)

  # --- get the proportion of NA ---
  are_na <- round((sum(is.na(x)) / n_rows), 6L)
  x      <- x[!is.na(x)]

  # --- get the proportion of numeric values ---
  tmp         <- suppressWarnings(as.numeric(x))
  are_numeric <- round((sum(!is.na(tmp)) / n_rows), 6L)

  # --- get the proportion of date values ---
  x        <- x[which(is.na(tmp))]
  are_date <- 0L
  if (!is.null(lubridate::guess_formats(x, c("ymd", "ydm", "dmy", "mdy", "myd",
                                             "dym", "Ymd", "Ydm", "dmY", "mdY",
                                             "mYd", "dYm")))) {
    x <- suppressWarnings(
      as.Date(
        lubridate::parse_date_time(
          x, orders = c("ymd", "ydm", "dmy", "mdy", "myd", "dym", "Ymd", "Ydm",
                        "dmY", "mdY", "mYd", "dYm")
        )
      )
    )
    are_date <- round((sum(!is.na(x)) / n_rows), 6L)
  }

  # --- get the proportion of logical values ---
  are_logical   <- round((sum(is.logical(x)) / n_rows), 6L)

  # --- get the proportion of character values ---
  are_character <- round((1.0 - (are_na + are_numeric +
                                   are_date + are_logical)), 6L)

  # --- return the output ---
  return(c(are_na, are_numeric, are_date, are_character, are_logical))
}

#' Scan a data frame to determine the percentage of `missing`, `numeric`,
#'    `Date`, `character`,  and `logical` values in every column.
#'
#' @param data A data frame or linelist
#'
#' @return A  data frame or linelist with the same columns as the input data
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
  scan_result           <- data.frame(t(apply(data, 2L, scan_columns)))
  names(scan_result)    <- c("missing", "numeric", "date", "character",
                             "logical")
  row_names             <- rownames(scan_result)
  rownames(scan_result) <- NULL
  scan_result           <- cbind(Field_names = row_names, scan_result)
  return(scan_result)
}
