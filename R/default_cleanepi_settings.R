#' Set and return \code{clean_data()} default parameters
#'
#' When \code{clean_data()} function is called without any argument, these
#' default values provided to the function's arguments will be applied on the
#' input data. By default, operations that require the target columns to be
#' specified by the user will not be performed. The default cleaning operations
#' include: i) standardizing column names, ii) detecting and removing
#' duplicates, and iii) removing constant data.
#'
#' @return The list of the default cleaning parameters.
#' @keywords internal
#'
get_default_params <- function() {
  params <- list(
    standardize_column_names = list(keep = NULL, rename = NULL),
    replace_missing_values = NULL,
    remove_duplicates = list(target_columns = NULL),
    remove_constants = list(cutoff = 1L),
    standardize_dates = NULL,
    standardize_subject_ids = NULL,
    dictionary = NULL,
    to_numeric = NULL,
    check_date_sequence = NULL
  )
  return(params)
}

#' Update \code{clean_data()} default argument's values with the user-provided
#' values
#'
#' @param defaults A list with the default arguments
#' @param params A list with the user-specified arguments
#' @param strict A boolean that specified whether to trigger an error or not
#'    when there is a difference between the list of default arguments and
#'    list of the arguments provided by the user.
#'
#' @return The updated list of parameters that will be used to perform the data
#'    cleaning.
#' @keywords internal
#'
modify_default_params <- function(defaults, params, strict = TRUE) {
  extra <- setdiff(names(params), names(defaults))
  if (strict && (length(extra) > 0L)) {
    stop("Additional invalid options: ", toString(extra))
  }
  # keep.null is needed here
  return(utils::modifyList(defaults, params, keep.null = TRUE))
}
