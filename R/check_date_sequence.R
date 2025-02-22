#' Checks whether the order in a sequence of date events is chronological.
#' order.
#'
#' @description Checks whether a date sequence in a vector of specified columns
#' is in chronological order or not.
#'
#' @param data The input \code{<data.frame>} or \code{<linelist>}
#' @param target_columns A \code{<vector>} of column names for events. Users
#'    should specify at least 2 column names in the expected order. For example:
#'    \code{target_columns = c("date_symptoms_onset", "date_hospitalization",
#'    "date_death")}.
#'    When the input data is a \code{<linelist>} object, this parameter can be
#'    set to \code{linelist_tags} to apply the date sequence checking
#'    exclusively to the tagged columns.
#'    The date values in the target columns should be in the ISO8601 format,
#'    e.g., 2024-12-31. Otherwise, use the \code{standardize_dates()} function to
#'    standardize the target columns.
#'
#' @returns The input dataset. When found, the incorrect date sequences will be
#'    stored in the report and can be accessed using
#'    \code{attr(data, "report")}.
#' @export
#'
#' @examples
#' # import the data
#' data <- readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi"))
#'
#' # standardize the date values
#' data <- data %>%
#'   standardize_dates(
#'     target_columns  = c("date_first_pcr_positive_test", "date.of.admission"),
#'     error_tolerance = 0.4,
#'     format = NULL,
#'     timeframe = NULL
#'   )
#'
#' # check the date sequence in two columns
#' good_date_sequence <- check_date_sequence(
#'   data = data,
#'   target_columns = c("date_first_pcr_positive_test", "date.of.admission")
#' )
check_date_sequence <- function(data, target_columns) {
  checkmate::assert_vector(target_columns, any.missing = FALSE, min.len = 1L,
                           max.len = dim(data)[2], null.ok = FALSE,
                           unique = TRUE)
  checkmate::assert_data_frame(data, null.ok = FALSE)

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns <- get_target_column_names(data, target_columns, cols = NULL)

  missing_cols <- !target_columns %in% names(data)
  # check if all columns are part of the data frame
  if (any(missing_cols)) {
    # send a warning if some columns are not part of the data
    cli::cli_alert_info(
      tr_("Found the following unrecognised column name{?s}: {.field {target_columns[missing_cols]}}."), # nolint: line_length_linter
      wrap = TRUE
    )
    target_columns <- target_columns[!missing_cols]
    # After removing unrecognized column names, the process shall be stopped if
    # there is only one column left in `target_columns`
    if (length(target_columns) < 2L) {
      cli::cli_abort(c(
        tr_("Insufficient number of columns to compare."),
        x = tr_("At least two columns of type {.cls Date} are required for this operation."), # nolint: line_length_linter
        i = tr_("Have you provided an invalid column name?")
      ), call = NULL)
    }
  }

  # checking the date sequence
  tmp_data <- data %>% dplyr::select(dplyr::all_of(target_columns))
  order_date <- apply(tmp_data, 1L, is_date_sequence_ordered)

  # when everything is in order,
  # send a message that no incorrect sequence of event was found
  if (all(order_date)) {
    cli::cli_alert_info(
      tr_("No incorrect date sequence was detected.")
    )
    return(data)
  }

  # flag out the row indices of the incorrect sequence of events
  bad_order <- which(!order_date)
  tmp_data <- tmp_data[bad_order, ]
  # add the row numbers of incorrect records to the report
  tmp_data <- data.frame(
    cbind(row_id = bad_order, tmp_data)
  )
  # adding incorrect records to the report
  data <- add_to_report(
    x = data,
    key = "incorrect_date_sequence",
    value = tmp_data
  )
  # send a message about the presence of incorrect date sequence
  cli::cli_inform(c(
    "!" = tr_("Detected {.val {length(bad_order)}} incorrect date sequence{?s} at line{?s}: {.val {toString(bad_order)}}."), # nolint: line_length_linter
    i = tr_("Enter {.code attr(dat, \"report\")[[\"incorrect_date_sequence\"]]} to access them, where {.val dat} is the object used to store the output from this operation.") # nolint: line_length_linter
  ))

  return(data)
}

#' Check order of a sequence of date-events
#'
#' @param x A \code{<vector>} of \code{<Date>} values
#'
#' @returns `TRUE` if elements of the vector are ordered, `FALSE` otherwise.
#' @keywords internal
is_date_sequence_ordered <- function(x) {
  return(!is.unsorted(x))
}
