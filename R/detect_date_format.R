#' Detect the date format with only 1 separator
#'
#' @param x the string of interest
detect_date_format <- function(x) {
  # check the format in x
  idx <- which(is.na(x))
  if (length(idx) > 0) {
    x <- x[-idx]
  }
  if (all(numbers_only(x))) {
    f1 <- detect_simple_format(x)
  } else {
    f1 <- detect_complex_format(x)
  }
  f1
}
