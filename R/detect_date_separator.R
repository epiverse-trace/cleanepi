#' Detect the special character that is the separator in the date values
#'
#' @param x the string of interest
#' @returns the detected separator
detect_date_separator <- function(x) {
  sep <- NULL
  if (!is.na(x)) {
    special_characters <- c("-", "/", ",", " ")
    sep <- NULL
    for (spec_char in special_characters) {
      if (stringr::str_detect(x, spec_char)) {
        sep <- c(sep, spec_char)
      }
    }
  }
  sep
}
