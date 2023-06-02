#' Reformat the date
#'
#' @param x the string of interest
#' @param format the date format
#'
as_Date <- function(x, format = c("ymd", "ydm", "dmy", "mdy", "myd", "dym",
                                  "Ymd", "Ydm", "dmY", "mdY", "mYd", "dYm")) {
  fmt <- lubridate::guess_formats(x, format)
  fmt <- unique(fmt)
  y <- as.Date(x, format = fmt[1])
  for (i in seq_along(fmt)[-1]) {
    na <- is.na(y)
    if (!any(na)) break
    y[na] <- as.Date(x[na], format = fmt[i])
  }
  y
}
