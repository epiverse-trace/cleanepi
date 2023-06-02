#' Detect whether a string contains only numbers
#'
#' @param x the string that contains the numbers
numbers_only <- function(x) {
  !grepl("\\D", x)
}
