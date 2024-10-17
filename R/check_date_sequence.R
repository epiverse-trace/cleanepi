#' Check whether the order of the sequence of date-events is valid.
#'
#' @description Checks whether a date sequence in a vector of specified columns
#' is in order or not.
#'
#' @param data The input data frame or linelist
#' @param target_columns A vector of event column names. Users should specify at
#'    least 2 column names in the expected order. For example:
#'    \code{target_columns = c("date_symptoms_onset", "date_hospitalization",
#'    "date_death")}.
#'    When the input data is a `linelist` object, this parameter can be set to
#'    \code{linelist_tags} if you wish to the date sequence across tagged
#'    columns only.
#'    The values in this column should be in the ISO8601 format (2024-12-31).
#'    Otherwise, use the \code{standardize_dates()} function to standardize the
#'    target columns.
#'
#' @returns The input dataset. When found, the incorrect date sequences will be
#'    stored in the report where they can be accessed using
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
    warning("Removing unrecognised column name: ", target_columns[missing_cols],
            call. = FALSE)
    target_columns <- target_columns[!missing_cols]
    if (length(target_columns) < 2L) {
      stop("\nAt least 2 event dates are required!")
    }
  }

  # checking the date sequence
  tmp_data   <- data %>% dplyr::select(dplyr::all_of(target_columns))
  order_date <- apply(tmp_data, 1L, is_date_sequence_ordered)
  bad_order  <- which(!order_date)
  if (!all(order_date)) {
    tmp_data <- tmp_data[bad_order, ]
    # adding incorrect records to the report
    data     <- add_to_report(x     = data,
                              key   = "incorrect_date_sequence",
                              value = tmp_data)
    warning("Detected ", length(bad_order),
            " incorrect date sequences at line(s): ",
            toString(bad_order),
            call. = FALSE)
  }

  return(data)
}

#' Check order of a sequence of date-events
#'
#' @param x A vector of Date values
#'
#' @returns `TRUE` if elements of the vector are ordered, `FALSE` otherwise.
#' @keywords internal
is_date_sequence_ordered <- function(x) {
  return(!is.unsorted(x))
}
