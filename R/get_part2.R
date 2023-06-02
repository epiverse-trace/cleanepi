#' Get part2 of date value
#'
#' @param x the string of interest
#' @param sep the separator in the string of interest
get_part2 <- function(x, sep) {
  unlist(strsplit(x, sep))[2]
}
