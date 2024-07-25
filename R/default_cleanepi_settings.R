#' Set `clean_data()` default parameters
#'
#' When calling `clean_data()` function with `param = NULL`, the arguments
#' defined here will applied on the input data. By default, operations that
#' require the target columns to specified by the user will not be performed.
#' The default cleaning operations include: i) standardizing column names,
#' ii) detecting and removing duplicates, and iii) removing constant data.
#'
#' @return The list of the default cleaning parameters.
#' @keywords internal
#'
default_cleanepi_settings <- function() {
  params <- list(
    standardize_column_names = list(keep = NULL, rename = NULL),
    replace_missing_values = NULL,
    remove_duplicates = list(
      target_columns = NULL
    ),
    remove_constants = list(cutoff = 1L),
    standardize_dates = NULL,
    standardize_subject_ids = NULL,
    dictionary = NULL,
    to_numeric = NULL,
    check_date_sequence = NULL,
    span = NULL
  )
  return(params)
}
