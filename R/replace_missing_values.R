#' Replace missing values with `NA`
#'
#' @param data A data frame or linelist
#' @param target_columns A vector of column names. If provided, the substitution
#'    of missing values will only be executed in those specified columns. When
#'    the input data is a `linelist` object, this parameter can be set to
#'    `linelist_tags` if you wish to replace missing values with NA on tagged
#'    columns only.
#' @param na_strings This is a vector of strings that represents the missing
#'    values in the columns of interest. By default, it utilizes
#'    `cleanepi::common_na_strings`. However, if the missing values string in
#'    the columns of interest is not included in this predefined vector,
#'    it can be used as the value for this argument.
#'
#' @returns The input data where missing values are replaced by `NA`.
#' @export
#'
#' @examples
#' cleaned_data <- replace_missing_values(
#'   data = readRDS(system.file("extdata", "test_df.RDS",
#'     package = "cleanepi"
#'   )),
#'   target_columns = "sex",
#'   na_strings = "-99"
#' )
#'
replace_missing_values <- function(data,
                                   target_columns = NULL,
                                   na_strings = cleanepi::common_na_strings) {
  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  cols <- get_target_column_names(data, target_columns, cols = NULL)

  # get the indices of the columns that contain the missing value characters
  tmp <- data %>%
    dplyr::select(dplyr::all_of(cols))
  indices <- colSums(apply(tmp, 2, match, na_strings), na.rm = TRUE) > 0
  cols <- names(tmp)[indices]

  # send a warning when none of the columns contains the provided missing value
  # string
  if (any(indices)) {
    # replace missing values with NA
    data <- data %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(cols), ~
        dplyr::na_if(as.character(.x), na_strings)))

    # make report
    data <- add_to_report(
      x = data,
      key = "missing_values_replaced_at",
      value = paste(cols, sep = ", ")
    )
  } else {
    warning("Could not detect missing value character!",
      "\nPlease use the appropriate strings that represents the missing",
      "values from your data.",
      call. = FALSE
    )
  }

  return(data)
}
