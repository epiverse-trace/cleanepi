#' Check the length of sample IDs
#'
#' @param x the sample ID
#' @param ref the template sample ID
check_id_length <- function(x, ref) {
  res <- TRUE
  if (nchar(ref) < nchar(x)) {
    res <- FALSE
  }
  res
}
