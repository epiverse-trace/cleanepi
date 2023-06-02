#' detect date format from date column
#'
#' @param data the input data frame
#' @param date_column_name the date column of interest
#' @param sep the separator in the date string
#'
#' @return a string with the detected date format
get_format <- function(data, date_column_name, sep) {
  format <- NULL
  data[[date_column_name]] <- as.character(data[[date_column_name]])
  part1 <- as.character(unlist(lapply(data[[date_column_name]], get_part1,
                                      sep[1])))
  part2 <- as.character(unlist(lapply(data[[date_column_name]], get_part2,
                                      sep[1])))
  part3 <- as.character(unlist(lapply(data[[date_column_name]], get_part3,
                                      sep[1])))
  f1 <- ifelse(all(is.na(part1)), NA, detect_date_format(part1))
  f2 <- ifelse(all(is.na(part2)), NA, detect_date_format(part2))
  f3 <- ifelse(all(is.na(part3)), NA, detect_date_format(part3))
  idx <- which(is.na(c(f1, f2, f3)))
  if (length(idx) == 0) {
    format <- paste0(format, f1, sep[1], f2, sep[1], f3)
  } else if (idx == 3) {
    format <- paste0(format, f1, sep[1], f2)
  } else if (idx == c(2, 3)) {
    format <- paste0(format, f1)
  } else {
    # stop("Unrecognised date format.\nPlease specify the date format using the
    #        'format' argument.")
    return(NULL)
  }
  format
}
