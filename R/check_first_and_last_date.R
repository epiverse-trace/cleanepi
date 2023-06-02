
#' Check date time frame
#'
#' @param first_date a Date object specifying the first valid date. Defaults to
#'   fifty years before the `last_date`
#' @param last_date last_date a Date object specifying the last valid date.
#'   Defaults to the current date.
#'
#' @return a list with the first and last date
#'
check_first_and_last_date <- function(first_date, last_date) {

  # make sure that they are single character strings in ISO 8601 format.
  iso_8601 <- "[0-9]{4}-(0|1(?=[0-2]))[0-9]-([0-2]|3(?=[0-1]))[0-9]"

  first_date_is_charcater <- is.character(first_date)
  first_date_has_len_1 <- length(first_date) == 1
  first_date_has_iso_8601 <- grepl(iso_8601, first_date, perl = TRUE)
  verdict <- first_date_is_charcater & first_date_has_len_1 &
    first_date_has_iso_8601
  if (verdict) {
    first_date <- as.Date(first_date, "%Y-%m-%d")
  }

  last_date_is_charcater <- is.character(last_date)
  last_date_has_len_1 <- length(last_date) == 1
  last_date_has_iso_8601 <- grepl(iso_8601, last_date, perl = TRUE)
  verdict <- last_date_is_charcater & last_date_has_len_1 &
    last_date_has_iso_8601
  if (verdict) {
    last_date <- as.Date(last_date, "%Y-%m-%d")
  }

  # Set the first date to 50 years before the last date if it's not set
  if (is.null(first_date) && inherits(last_date, "Date")) {
    first_date <- min(seq.Date(last_date, length.out = 2, by = "-50 years"))
  }

  if (!inherits(first_date, "Date") || !inherits(last_date, "Date")) {
    stop("first_date and last_date must be Date objects or characters in
         yyyy-mm-dd format.")
  }
  list(first_date, last_date)
}
