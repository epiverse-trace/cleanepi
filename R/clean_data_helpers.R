#' Calculate the percentage of missing and other data type values in a vector
#' containing different data types such as numeric, Date, character, and
#' logical.
#'
#' @param x A vector of ones or a combination of various data types.
#'
#' @returns A vector of 5 elements representing the percentage of missing,
#' numeric, date, character, and logical values found in the input vector.
#'
#' @keywords internal
#'
scan_columns <- function(x, type) {
  # --- save the variable length ---
  n_rows <- length(x)

  # --- get the proportion of NA ---
  are_na <- round((sum(is.na(x)) / n_rows), 6L)
  x      <- x[!is.na(x)]

  # --- get the proportion of numeric values ---
  are_numeric <- 0L
  verdict     <- type == "logical" | type == "factor"
  if (!verdict) {
    tmp         <- suppressWarnings(as.numeric(x))
    are_numeric <- round((sum(!is.na(tmp)) / n_rows), 6L)
  }

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
  are_logical   <- 0
  if (type == "logical") {
    are_logical <- round((1.0 - (are_na + are_date)), 6L)
  }

  # --- get the proportion of character values ---
  are_character   <- 0L
  if (!verdict) {
    are_character <- round((1.0 - (are_na + are_numeric +
                                     are_date + are_logical)), 6L)
  }


  # --- return the output ---
  return(c(are_na, are_numeric, are_date, are_character, are_logical))
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
  #
  types         <- as.character(vapply(data, class, character(1L)))
  scan_result   <- NULL
  j             <- 1
  for (i in names(data)) {
    scan_result <- rbind(scan_result, scan_columns(data[[i]], types[j]))
    j           <- j + 1
  }
  scan_result        <- as.data.frame(scan_result)
  names(scan_result) <- c("missing", "numeric", "date", "character",
                             "logical")
  scan_result        <- cbind(Field_names = names(data), scan_result)
  return(scan_result)
}
