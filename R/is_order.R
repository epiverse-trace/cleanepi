#' Check the sequence of the event
#'
#' @param x the string of interest
is_order <- function(x) {
  x <- as_Date(x)
  sum(order(x) == seq_along(x)) == length(x)
}
