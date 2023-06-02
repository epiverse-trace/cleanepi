#' Check the prefix of the subject IDs
#'
#' @param x the sample ID
#' @param prefix the prefix to look up to
check_prefix <- function(x, prefix) {
  res <- TRUE
  prefix_found_at <- stringr::str_locate(x, prefix)
  if (all(is.na(prefix_found_at[1, ])) ||
      (prefix_found_at[1, 1] != 1 && prefix_found_at[1, 2] != nchar(prefix))) {
    res <- FALSE
  }
  res
}
