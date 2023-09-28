#' Check the sequence of the event
#'
#' @param x the string of interest
#' @keywords internal
#' @noRd
is_order <- function(x) {
  x <- as_date(x)
  !is.unsorted(x)
}



#' Check the sequence of event dates
#'
#' @description This function is used to check whether date sequence in
#' the specified columns is correct or not.
#'
#' @param data the input data frame
#' @param event_cols a vector or a comma-separated list of the event columns
#'    names. Users should specify at least 2 column names in the expected order.
#'    For example: event_cols = c("DS", "DH", "DD") where DS=date of the
#'    symptoms onset, DH=date of hospitalization, DD=date of death
#' @param remove_bad_seq a Boolean to specify if rows with incorrect order
#'    should be filtered out or not. default is FALSE
#' @param report the object that will contains details about the result from the
#'    date columns standardization
#'
#' @returns rows of the input data frame with incorrect date sequence
#'    if `remove_bad_seq = FALSE`, the input data frame without those
#'    rows if not
#' @export
#'
#' @examples
#' good_date_sequence <- check_date_sequence(
#' data = readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")),
#' event_cols = c("date_first_pcr_positive_test", "date.of.admission"),
#' remove_bad_seq = FALSE,
#' report = list()
#' )
check_date_sequence <- function(data, event_cols, remove_bad_seq = FALSE,
                                report = list()) {
  checkmate::assert_vector(event_cols, any.missing = FALSE, min.len = 1L,
                           null.ok = FALSE, unique = TRUE)
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_logical(remove_bad_seq, any.missing = FALSE, len = 1L,
                            null.ok = FALSE)

  # check if input is character string
  if (all(grepl(",", event_cols, fixed = TRUE))) {
    event_cols <- as.character(unlist(strsplit(event_cols, ",", fixed = TRUE)))
    event_cols <- gsub(" ", "", event_cols, fixed = TRUE)
  }

  # check if all columns are part of the data frame
  if (!all(event_cols %in% names(data))) {
    idx <- which(!(event_cols %in% names(data)))
    event_cols <- event_cols[-idx]
    warning("\nRemoving unrecognised column name: ", event_cols[idx],
            call. = FALSE)
    if (length(event_cols) < 2L) {
      stop("\nAt least 2 event dates are required!")
    }
  }

  # check and convert to Date if required
  for (cols in event_cols) {
    if (!lubridate::is.Date(data[[cols]])) {
      data <- standardize_date(data, cols, timeframe = NULL,
                               check_timeframe = FALSE,
                               report = list(), error_tolerance = 0.5)[[1L]]
    }
  }

  # checking the date sequence
  tmp_data <- data %>% dplyr::select(dplyr::all_of(event_cols))
  order_date <- apply(tmp_data, 1L, is_order)
  bad_order <- which(!order_date)
  if (!all(order_date)) {
    tmp_data <- data[bad_order, ]
    if (remove_bad_seq) {
      data <- data[-bad_order, ]
      warning(length(bad_order),
              " incorrect date sequences were detected and removed",
              call. = FALSE)
    }
  }

  # making the report
  if (length(bad_order) > 0L) {
    if (!("incorrect_date_sequence" %in% names(report))) {
      report[["incorrect_date_sequence"]] <- list()
    }
    report[["incorrect_date_sequence"]][["date_sequence"]] <-
      glue::glue_collapse(event_cols, sep = " < ")
    report[["incorrect_date_sequence"]][["bad_sequence"]] <- tmp_data
  } else {
    report <- NULL
  }

  list(
    data = data,
    report = report
  )
}
