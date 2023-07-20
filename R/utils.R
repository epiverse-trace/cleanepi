#' Detect whether a string contains only numbers
#'
#' @param x the string that contains the numbers
numbers_only <- function(x) {
  !grepl("\\D", x)
}

#' Get part1 of date value
#'
#' @param x the string of interest
#' @param sep the separator in the string of interest
get_part1 <- function(x, sep) {
  unlist(strsplit(x, sep))[1]
}

#' Get part2 of date value
#'
#' @param x the string of interest
#' @param sep the separator in the string of interest
get_part2 <- function(x, sep) {
  unlist(strsplit(x, sep))[2]
}

#' Get part3 of date value
#'
#' @param x the string of interest
#' @param sep the separator in the string of interest
get_part3 <- function(x, sep) {
  unlist(strsplit(x, sep))[3]
}

#' Get sum from number
#'
#' @param x the string of interest
get_sum <- function(x) {
  if (nchar(x) == 2) {
    x <- sum(as.numeric(substr(x, 1, 1)), as.numeric(substr(x, 2, 2)))
  }
  x
}

#' Extract letters from a string
#'
#' @param x the string of interest
only_letters <- function(x) {
  gsub("^([[:alpha:]]*).*$", "\\1", x)
}

#' Check if date column exists in given data frame
#'
#' @param data the input data frame
#' @param date_column_name the date column of interest
check_column_existence <- function(data, date_column_name) {
  # check the column name
  if (is.null(date_column_name)) {
    idx <- which(names(data) %in% c("Date", "DATE", "date"))
    if (length(idx) == 0) {
      stop("Could not find column named as ",
           glue::glue_collapse(c("Date", "DATE", "date"), sep = " or "),
           "\nPlease specify the date column name.")
    }
    date_column_name <- names(data)[idx]
  }

  # check whether the provided column name belong to the data
  if (!all(date_column_name %in% names(data))) {
    idx <- which(!(date_column_name %in% names(data)))
    stop("Can't find column: ", date_column_name[idx])
  }
  date_column_name
}
