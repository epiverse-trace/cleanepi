#' Detect complex format
#'
#' @param x the string of interest
detect_complex_format <- function(x) {
  f1 <- f2 <- NULL
  tmp_sep <- unique(unlist(lapply(x, detect_date_separator)))
  if (is.null(tmp_sep)) {
    f1 <- detect_simple_format(x)
    if (is.null(f1)) {
      f1 <- detect_day_or_month(x)
    }
  } else if (!is.na(tmp_sep) && length(tmp_sep) == 1) {
    p1 <- as.character(unlist(lapply(x, get_part1, tmp_sep)))
    p2 <- as.character(unlist(lapply(x, get_part2, tmp_sep)))
    f1 <- detect_simple_format(p1)
    f2 <- detect_simple_format(p2)
    if (is.null(f1)) {
      f1 <- detect_day_or_month(p1)
    }
    if (is.null(f2)) {
      f2 <- detect_day_or_month(p2)
    }
  } else {
    # stop("Unrecognised date format.\nPlease specify the date format using the
    #      'format' argument.")
    return(NULL)
  }
  format <- make_format(f1, f2, tmp_sep)
  format
}
