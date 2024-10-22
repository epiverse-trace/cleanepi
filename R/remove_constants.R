#' Remove constant data i.e. empty rows and columns and constant columns
#'
#' The function iteratively removes the constant data until there are not found
#' anymore. It stores the details about the removed constant data in a form of
#' a data frame within the report object.
#'
#' @param data The input data frame or linelist
#' @param cutoff The cut-off for empty rows and columns removal. If provided,
#'    only rows and columns where the percent of missing data is greater than
#'    this cut-off will removed. Default is 1.
#'
#' @returns The input dataset without the empty rows and columns and the
#'    constant columns.
#' @export
#'
#' @examples
#' data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
#'
#' # introduce an empty column
#' data$empty_column <- NA
#'
#' # remove The constant columns, empty rows and columns where empty rows and
#' # columns are defined as those with 100% NA.
#' dat <- remove_constants(
#'   data = data,
#'   cutoff = 1
#' )
#'
#' # remove the constant columns, empty rows and columns where empty rows and
#' # columns are defined as those with 50% NA.
#' dat <- remove_constants(
#'   data = data,
#'   cutoff = 0.5
#' )
#'
#' # check the report to see what has happened
#' report <- attr(dat, "report")
#' report$constant_data
remove_constants <- function(data, cutoff = 1.0) {
  checkmate::assert_number(cutoff, lower = 0.0, upper = 1.0, na.ok = FALSE,
                           finite = TRUE, null.ok = FALSE)
  # extract the current report and save it for later use
  report <- attr(data, "report")

  # perform the constant data removal once
  # then iteratively perform constant data removal until the input data is the
  # same as the output data
  initial_data <- data
  i <- 1
  data <- perform_remove_constants(data, cutoff)

  # save details about removed data for report
  constant_data_report <- list(
    iteration_1 = list(c(
      empty_columns = data[["empty_columns"]],
      empty_rows = data[["empty_rows"]],
      constant_columns = data[["constant_columns"]]
    ))
  )
  # iteratively remove constant data
  if (!identical(initial_data, data[["data"]])) {
    different <- TRUE
    while (different) {
      i <- i + 1
      tmp_data <- data[["data"]]
      data <- perform_remove_constants(tmp_data, cutoff)
      constant_data_report[[paste0("iteration_", i)]] <- list(c(
        empty_columns = data[["empty_columns"]],
        empty_rows = data[["empty_rows"]],
        constant_columns = data[["constant_columns"]]
      ))
      if (identical(tmp_data, data[["data"]])) {
        different <- FALSE
      }
    }
  }

  # make a data frame of the information about the removed constant data
  constant_data_report <- dplyr::bind_rows(constant_data_report)

  # when some elements of the constant data were not found from a given dataset,
  # populate those elements with NA. This allows to always return a data frame
  # with the same columns.
  expected_columns <- c("empty_columns", "empty_rows", "constant_columns")
  idx <- expected_columns %in% names(constant_data_report)
  if (!all(idx)) {
    constant_data_report[, expected_columns[!idx]] <- NA
  }
  constant_data_report <- constant_data_report[, expected_columns]

  # add the iteration numbers to the report for information about what rows and
  # columns were eliminated at which iteration.
  constant_data_report <- cbind(
    iteration = seq_len(nrow(constant_data_report)),
    constant_data_report
  )

  # send a message about iterative constant data removal to alert the user
  if (nrow(constant_data_report) > 1) {
    message("Constant data was removed after ", nrow(constant_data_report),
            " iterations. See the report for more details.")
  }

  data <- data[["data"]]
  report[["constant_data"]] <- constant_data_report
  attr(data, "report") <- report
  return(data)
}

#' Perform constant data removal.
#'
#' This function is called at every iteration of the constant data removal until
#' no constant data is found.
#'
#' @inheritParams remove_constants
#'
#' @return A list with the input dataset where all empty rows and columns as
#'    well as constant columns have been removed.
#' @keywords internal
#'
perform_remove_constants <- function(data, cutoff) {
  # remove the empty rows
  missingness <- rowSums(is.na(data)) / ncol(data)
  to_remove <- missingness >= cutoff
  dat <- data[!to_remove, ]

  # report empty rows if found
  empty_rows <- NULL
  if (nrow(data) > nrow(dat)) {
    empty_rows <- toString(which(rowSums(is.na(data)) / ncol(data) >= cutoff))
  }

  # remove the empty columns
  missingness <- colSums(is.na(dat)) / nrow(dat)
  to_remove <- missingness >= cutoff
  dat <- dat[, !to_remove]

  # report empty columns if found
  empty_columns <- NULL
  removed <- setdiff(colnames(data), names(dat))
  if (length(removed) > 0L) {
    empty_columns <- toString(removed)
  }

  # remove constant columns
  data <- dat
  are_constant <- apply(
    dat,
    2,
    function(x) as.numeric(length(unique(x[!is.na(x)]))) <= 1
  )
  dat <- dat[, !are_constant, drop = FALSE]
  removed <- setdiff(colnames(data), names(dat))
  constant_columns <- NULL
  if (length(removed) > 0L) {
    constant_columns <- toString(removed)
  }

  return(list(
    data = dat,
    empty_columns = empty_columns,
    empty_rows = empty_rows,
    constant_columns = constant_columns
  ))
}
