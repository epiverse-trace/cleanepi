#' Get part1 of date value
#'
#' @param x the string of interest
#' @param sep the separator in the string of interest
get_part1 <- function(x, sep) {
  unlist(strsplit(x, sep))[1]
}
