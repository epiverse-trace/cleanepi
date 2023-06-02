#' function to get simple format
#'
#' @param x the string of interest
detect_simple_format <- function(x) {
  f1 <- NULL
  if (is.null(x)) f1 <- NULL
  if (all(nchar(x) == 4)) {
    f1 <- "%Y" # year with century i.e 4 digits year
  } else if (any(nchar(x) == 4) && any(nchar(x) == 2)) {
    stop("Detected different lengths in first digits of date column.\n
         Please use same number of digits or specify the date format with
         the 'format' argument.")
  } else if (all(nchar(x) == 2)) {
    tmp <- as.numeric(x)
    if (all(tmp <= 12)) {
      f1 <- "%m"
    } else if (all(tmp >= 1) && all(tmp <= 31)) {
      f1 <- "%d"
    } else {
      f1 <- "%y"
    }
  }
  f1
}
