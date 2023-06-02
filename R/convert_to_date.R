convert_to_date <- function(data, cols, sep) {
  format <- get_format(data, cols, sep)
  if (!is.null(format)) {
    data[[cols]] <- as.Date(data[[cols]], format = format)
  } else {
    data[[cols]] <- guess_dates(data[[cols]], error_tolerance = 0.5)
  }
  data
}
