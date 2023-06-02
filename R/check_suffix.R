#' Check the suffix of the subject IDs
#'
#' @param x the sample IDs
#' @param suffix the suffix to lookup to
check_suffix <- function(x, suffix) {
  res <- TRUE
  suffix_found_at <- as.matrix(stringr::str_locate_all(x, suffix)[[1]])
  if (all(is.na(suffix_found_at[nrow(suffix_found_at), ])) ||
      (suffix_found_at[nrow(suffix_found_at), 1] !=
       (nchar(x) - (nchar(suffix) - 1)) &&
       suffix_found_at[nrow(suffix_found_at), 2] != nchar(x))) {
    res <- FALSE
  }
  res
}
