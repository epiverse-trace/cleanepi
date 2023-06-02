#' Get part3 of date value
#'
#' @param x the string of interest
#' @param sep the separator in the string of interest
get_part3 <- function(x, sep) {
  unlist(strsplit(x, sep))[3]
}
