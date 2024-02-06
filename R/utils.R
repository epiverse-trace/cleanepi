#' Detects whether a string contains only numbers or not.
#'
#' @param x A string of numbers
#' @keywords internal
numbers_only <- function(x) {
  return(!grepl("\\D", x))
}

#' Get part1 of date value
#'
#' @param x A string of interest
#' @param sep A separator in the string of interest
#' @keywords internal
date_get_part1 <- function(x, sep) {
  if (is.na(x)) {
    return(NA)
  }
  return(unlist(strsplit(x, sep, fixed = TRUE))[[1L]])
}

#' Get part2 of date value
#'
#' @param x A string of interest
#' @param sep A separator in the string of interest
#' @keywords internal
date_get_part2 <- function(x, sep) {
  if (is.na(x)) {
    return(NA)
  }
  return(unlist(strsplit(x, sep, fixed = TRUE))[[2L]])
}

#' Get part3 of date value
#'
#' @param x A string of interest
#' @param sep A separator in the string of interest
#' @keywords internal
#'
date_get_part3 <- function(x, sep) {
  if (is.na(x)) {
    return(NA)
  }
  return(unlist(strsplit(x, sep, fixed = TRUE))[[3L]])
}

#' Get sum from number
#'
#' @param x A string of interest
#' @keywords internal
#'
get_sum <- function(x) {
  if (nchar(x) == 2L) {
    x <- sum(as.numeric(substr(x, 1L, 1L)), as.numeric(substr(x, 2L, 2L)))
  }
  return(x)
}

#' Add a report obtained from a data cleaning step to a data frame
#'
#' @param data A  data frame
#' @param report A named list or a data frame containing details from the
#'    cleaning operations.
#' @param name A character with the name of the cleaning operation. The default
#'    value is NULL.
#'
#' @return The input data frame with a report associated to it. This can be
#'    accessed using `attr(data, "report")`
#' @keywords internal
#'
add_report <- function(data, report, name = NULL) {
  # when the data is not associated with any report, make it be
  if (is.null(attr(data, "report"))) {
    tmp_report <- list()
    if (!is.null(name)) {
      tmp_report[[name]] <- report
    } else {
      tmp_report[[1L]]   <- report
    }
    attr(data, which = "report") <- tmp_report
  } else {
    # when the report object is a data frame, the user will provide the
    # name of the analysis step. This will be used to add the report to the data
    #
    # if it is a list (we expect a named list), then it will be concatenated to
    # the existing report.
    tmp_report <- attr(data, "report")
    if (!is.null(name)) {
      tmp_report[[name]] <- report
    } else {
      tmp_report[[length(tmp_report) + 1L]] <- report
    }
    attr(data, which = "report") <- tmp_report
  }

  return(data)
}


#' Add an element to the report object
#'
#' @param x A data frame  or linelist
#' @param key The name of the cleaning operation
#' @param value The object to add to the report object
#'
#' @return The input report object with an additional element
#' @keywords internal
#'
add_to_report <- function(x, key, value = NULL) {
  report   <- attr(x, "report")
  if (is.null(report)) {
    report <- list()
  }
  report[[key]]             <- value
  attr(x, which = "report") <- report
  return(x)
}
