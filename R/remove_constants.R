#' Remove constant data, including empty rows, empty columns, and
#' columns with constant values.
#'
#' The function iteratively removes constant data until none remain.
#' It records details of the removed constant data as a data frame
#' within the report object.
#'
#' @param data The input \code{<data.frame>} or \code{<linelist>}
#' @param cutoff A \code{<numeric>} value specifying the cut-off for removing
#'    constant data. The possible values vary between 0 (excluded) and 1
#'    (included). The default is 1 i.e. remove rows and columns with 100%
#'    constant data.
#'
#' @returns The input dataset where the constant data is filtered out based on
#'    specified cut-off.
#' @export
#'
#' @examples
#' data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
#'
#' # introduce an empty column
#' data$empty_column <- NA

#' # inject some missing values across some columns
#' data$study_id[3] = NA_character_
#' data$date.of.admission[3] = NA_character_
#' data$date.of.admission[4] = NA_character_
#' data$dateOfBirth[3] = NA_character_
#' data$dateOfBirth[4] = NA_character_
#' data$dateOfBirth[5] = NA_character_
#'
#' # with cutoff = 1, line 3, 4, and 5 are not removed
#' cleaned_df <- remove_constants(
#'   data = data,
#'   cutoff = 1
#' )
#'
#' # drop rows or columns with a percentage of constant values
#' # equal to or more than 50%
#' cleaned_df <- remove_constants(
#'   data = cleaned_df,
#'   cutoff = 0.5
#' )
#'
#' # drop rows or columns with a percentage of constant values
#' # equal to or more than 25%
#' cleaned_df <- remove_constants(
#'   data = cleaned_df,
#'   cutoff = 0.25
#' )
#'
#' # drop rows or columns with a percentage of constant values
#' # equal to or more than 15%
#' cleaned_df <- remove_constants(
#'   data = cleaned_df,
#'   cutoff = 0.15
#' )
#'
#' # check the report to see what has happened
#' print_report(cleaned_df, "constant_data")
remove_constants <- function(data, cutoff = 1.0) {
  checkmate::assert_number(cutoff, lower = 0.0, upper = 1.0, na.ok = FALSE,
                           finite = TRUE, null.ok = FALSE)
  # send a message about iterative constant data removal to alert the user
  if (cutoff == 0) {
    cli::cli_inform(c(
      i = tr_("Constant data was not removed. The value for the {.emph cut-off} argument must be greater than {.emph 0} and less than or equal to {.emph 1}.") # nolint: line_length_linter
    ))
    return(data)
  }

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
    cli::cli_inform(c(
      "!" = tr_("Constant data was removed after {.val {nrow(constant_data_report)}} iteration{?s}."), # nolint: line_length_linter
      i = tr_("Enter {.code print_report(dat, \"constant_data\")} for more information, where {.val dat} represents the object used to store the output from {.fn remove_constants}.") # nolint: line_length_linter
    ))
  }

  data <- data[["data"]]
  report[["constant_data"]] <- constant_data_report
  attr(data, "report") <- report
  return(data)
}

#' Remove constant data.
#'
#' This function is called at each iteration of the constant data removal
#' process until no constant data remains.
#'
#' @inheritParams remove_constants
#'
#' @return A \code{<list>} with the input dataset where all empty rows and
#'    columns as well as constant columns have been removed.
#' @keywords internal
#'
perform_remove_constants <- function(data, cutoff) {
  # remove the empty rows
  missingness <- rowSums(is.na(data)) / ncol(data)
  to_remove <- missingness >= cutoff
  dat <- if (all(to_remove)) data else data[!to_remove, ]

  # report empty rows if found
  empty_rows <- NULL
  if (any(to_remove)) {
    empty_rows <- toString(which(to_remove))
  }

  # remove the empty columns
  missingness <- colSums(is.na(dat)) / nrow(dat)
  to_remove <- missingness >= cutoff
  dat <- if (sum(to_remove) == 0) dat else dat[, !to_remove]

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
