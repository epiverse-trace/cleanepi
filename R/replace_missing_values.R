
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
#' @return The input data where missing values are replaced by `NA`.
#' @export
#'
#' @examples
#' cleaned_data <- replace_missing_values(
#'   data           = readRDS(system.file("extdata", "test_df.RDS",
#'                                        package = "cleanepi")),
#'   target_columns = "sex",
#'   na_strings     = "-99"
#' )
#'
replace_missing_values <- function(data,
                                   target_columns = NULL,
                                   na_strings = cleanepi::common_na_strings) {
  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  cols    <- get_target_column_names(data, target_columns, cols = NULL)

  # replace missing values with NA
  indexes <- NULL
  res     <- 0L
  for (col in cols) {
    index              <- match(col, names(data))
    names(data)[index] <- "x"
    idx                <- match(na_strings, data[["x"]])
    if (all(is.na(idx))) {
      res                <- res + 1L
      names(data)[index] <- col
      next
    }
    idx     <- which(na_strings %in% data[["x"]])
    data    <- naniar::replace_with_na(data,
                                       replace = list(x = na_strings[idx]))
    indexes <- c(indexes, col)
    names(data)[index] <- col
  }

  stopifnot("Could not detect missing value character! Please use the
  appropriate strings that represents the missing values from your data." =
              res < length(cols))

  # make report
  xx   <- ifelse(length(indexes) == ncol(data),
                 glue::glue_collapse(names(data), sep = ", "),
                 glue::glue_collapse(indexes, sep = ", "))
  data <- add_to_report(x     = data,
                        key   = "missing_values_replaced_at",
                        value = xx)

  return(data)
}
