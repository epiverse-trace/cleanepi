#' Check whether the order of the sequence of date-events is valid
#'
#' @description Checks whether a date sequence in
#' a vector of  specified columns is in order or not.
#'
#' @param data A data frame
#' @param target_columns A vector of event column names. Users should specify at
#'    least 2 column names in the expected order.
#'    For example: target_columns = c("date_symptoms_onset",
#'    "date_hospitalization", "date_death"). When the input data is a `linelist`
#'    object, this parameter can be set to `linelist_tags` if you wish to
#'    the date sequence across tagged columns only.
#'
#' @returns The input dataset. When found, the incorrect date sequences will be
#'    stored in the report where they can be accessed using
#'    `attr(data, "report")`.
#' @export
#'
#' @examples
#' good_date_sequence <- check_date_sequence(
#'   data           = readRDS(system.file("extdata", "test_df.RDS",
#'                                        package = "cleanepi")),
#'   target_columns = c("date_first_pcr_positive_test", "date.of.admission")
#' )
check_date_sequence <- function(data, target_columns) {
  checkmate::assert_vector(target_columns, any.missing = FALSE, min.len = 1L,
                           max.len = dim(data)[2], null.ok = FALSE,
                           unique = TRUE)
  checkmate::assert_data_frame(data, null.ok = FALSE)

  # if target_column is a character string, then convert it to vector
  if (all(grepl(",", target_columns, fixed = TRUE))) {
    target_columns <- as.character(unlist(strsplit(target_columns, ",",
                                                   fixed = TRUE)))
    target_columns <- trimws(target_columns)
  }
  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns <- get_target_column_names(data, target_columns, cols = NULL)


  # check if all columns are part of the data frame
  if (!all(target_columns %in% names(data))) {
    idx            <- which(!(target_columns %in% names(data)))
    warning("\nRemoving unrecognised column name: ", target_columns[idx],
            call. = FALSE)
    target_columns <- target_columns[-idx]
    if (length(target_columns) < 2L) {
      stop("\nAt least 2 event dates are required!")
    }
  }

  # check and convert to Date if required
  for (cols in target_columns) {
    if (!lubridate::is.Date(data[[cols]])) {
      data <- standardize_dates(data, cols, timeframe = NULL,
                                error_tolerance = 0.5)
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
            glue::glue_collapse(bad_order, sep = ", "),
            call. = FALSE)
  }

  return(data)
}

#' Check order of a sequence of events
#'
#' @param x A string of interest
#' @keywords internal
is_date_sequence_ordered <- function(x) {
  return(!is.unsorted(x))
}
