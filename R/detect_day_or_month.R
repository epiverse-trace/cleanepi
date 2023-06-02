#' Detect whether it's day or month
#'
#' @param x the string of interest
detect_day_or_month <- function(x) {
  f1 <- NULL
  full_days <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                 "Friday", "Saturday", "Sunday")
  abreviated_days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  all_full_months <- c(month.name, toupper(month.name), tolower(month.name))
  all_abb_months <- c(month.abb, toupper(month.abb), tolower(month.abb))
  all_full_days <- c(full_days, toupper(full_days), tolower(full_days))
  all_abb_days <- c(abreviated_days, toupper(abreviated_days),
                    tolower(abreviated_days))
  in_full_month <- x %in% all_full_months
  in_abb_month <- x %in% all_abb_months
  in_full_day <- x %in% all_full_days
  in_abb_day <- x %in% all_abb_days
  if (all(in_full_month)) {
    f1 <- "%B"
  } else if (all(in_abb_month)) {
    f1 <- "%b"
  }
  if (all(in_full_day)) {
    f1 <- "%A"
  } else if (all(in_abb_day)) {
    f1 <- "%a"
  }
  f1
}
