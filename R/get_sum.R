#' Get sum from number
#'
#' @param x the string of interest
get_sum <- function(x) {
  if (nchar(x) == 2) {
    x <- sum(as.numeric(substr(x, 1, 1)), as.numeric(substr(x, 2, 2)))
  }
  x
}
