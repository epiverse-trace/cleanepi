#' Extract letters from a string
#'
#' @param x the string of interest
only_letters <- function(x) {
  gsub("^([[:alpha:]]*).*$", "\\1", x)
}
