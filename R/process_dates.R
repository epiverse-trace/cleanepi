
#' Process date variable
#'
#' @param x an object of date class
#' @param first_date a Date object specifying the first valid date. Defaults to
#'   fifty years before the `last_date`
#' @param last_date last_date a Date object specifying the last valid date.
#'   Defaults to the current date.
#' @param check_timeframe a logical specifying whether to check if all values
#'    fall under the specified timeframe
#'
#' @return the modified input object
#'
process_dates <- function(x, first_date, last_date, check_timeframe) {
  # If the input is a date already: no guessing needed!
  if (inherits(x, c("Date", "POSIXt", "aweek"))) {
    x <- as.Date(x)
    if (check_timeframe) {
      x[x < first_date | x > last_date] <- as.Date(NA_character_)
    }
    return(x)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (!is.character(x)) {
    stop("guess dates will only work for characters and factors")
  }

  return(x)
}
