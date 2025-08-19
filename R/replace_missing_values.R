#' Replace missing values with \code{NA}
#'
#' @param data The input \code{<data.frame>} or \code{<linelist>}
#' @param target_columns A \code{<vector>} of column names. If provided, missing
#'    values will be substituted only in the specified columns. When the input
#'    data is a \code{<linelist>} object, this parameter can be set to
#'    \code{linelist_tags} to replace missing values with \code{NA} in the
#'    tagged columns only.
#' @param na_strings A \code{<vector>} of characters that represent the missing
#'    values in the columns of interest. By default, it utilizes
#'    \code{cleanepi::common_na_strings}. However, if the missing values string
#'    in the columns of interest is not included in this predefined vector,
#'    it can be used as the value for this argument. Matching of `data` with
#'    `na_strings` is insensitive to case and whitespace.
#'
#' @returns The input data where missing values are replaced by \code{NA}.
#' @export
#'
#' @examples
#' data <- readRDS(
#'   system.file("extdata", "test_df.RDS", package = "cleanepi")
#' )
#'
#' # replace all occurrences of '-99' with NA
#' cleaned_data <- replace_missing_values(
#'   data = data,
#'   target_columns = NULL,
#'   na_strings = "-99"
#' )
#'
#' # print the names of the columns where the replacement occurred
#' print_report(cleaned_data, "missing_values_replaced_at")
#'
replace_missing_values <- function(data,
                                   target_columns = NULL,
                                   na_strings = cleanepi::common_na_strings) {
  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  cols <- get_target_column_names(data, target_columns, cols = NULL)

  # identify the columns containing the specified missing value characters
  tmp <- data %>%
    dplyr::select(dplyr::all_of(cols))
  # normalise text for case and whitespace insensitive matching
  tmp <- as.data.frame(
    lapply(tmp, function(x) if (is.character(x)) trimws(tolower(x)) else x)
  )
  indices <- colSums(apply(tmp, 2, match, na_strings), na.rm = TRUE) > 0

  # send a warning when none of the columns contains the provided missing value
  # strings
  if (any(indices)) {
    # replace missing value strings with NA
    cols <- names(tmp)[indices]
    data[cols] <- lapply(tmp[cols], replace_with_na, na_strings)

    # report columns where the replacement happened
    data <- add_to_report(
      x = data,
      key = "missing_values_replaced_at",
      value = cols
    )
  } else {
    cli::cli_inform(c(
      "!" = tr_("Could not detect the provided missing value {cli::qty(length(na_strings))} character{?s}."), # nolint: line_length_linter
      i = tr_("Does your data contain missing value characters other than the specified ones?") # nolint: line_length_linter
    ))
  }

  return(data)
}

#' Detect and replace values with \code{NA} from a vector
#'
#' @param x A \code{<vector>} of numeric or character values
#' @param na_strings A \code{<vector>} of the values to be replaced
#'
#' @return A \code{<vector>} where the specified values were replaced with
#'    \code{NA} if found.
#' @keywords internal
#'
replace_with_na <- function(x, na_strings) {
  are_na_strings <- x %in% na_strings
  x[are_na_strings] <- NA
  return(x)
}
