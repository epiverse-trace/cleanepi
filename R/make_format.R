make_format <- function(f1, f2, tmp_sep) {
  if (all(is.null(f1) & is.null(f2))) {
    stop("Unrecognised date format.\nPlease specify the date format using
         the 'format' argument.")
  } else if (all(!is.null(f1) & !is.null(f2))) {
    format <- paste0(f1, tmp_sep, f2)
  } else if (!is.null(f1) && is.null(f2)) {
    format <- f1
  } else if (!is.null(f2) && is.null(f1)) {
    format <- f2
  }
  format
}
