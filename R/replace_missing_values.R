
#' Replace missing values with `NA`
#'
#' @param data the input data
#' @param target_columns a vector of column names. If provided, the missing
#'    values substitution will only be performed in those columns.
#' @param na_strings a vector of strings that represent the missing values in
#'    the columns of interest. Default values are `cleanepi::common_na_strings`.
#'    When the missing values string in the columns of interest is not part of
#'    this predefined vector, it can be used as the value for this argument.
#'
#' @return the input data where missing values are replaced by `NA`
#' @export
#'
#' @examples
#' cleaned_data <- replace_missing_values(
#'   data        = readRDS(system.file("extdata", "test_df.RDS",
#'                                     package = "cleanepi")),
#'   target_columns = "sex",
#'   na_strings     = "-99"
#' )
#'
replace_missing_values <- function(data,
                                   target_columns = NULL,
                                   na_strings = cleanepi::common_na_strings) {
  # get the target columns
  index        <- seq_len(ncol(data))
  if (!is.null(target_columns)) {
    index      <- which(names(data) == target_columns)
  }
  cols         <- names(data)[index]

  # replace missing values with NA
  res     <- 0L
  indexes <- NULL
  for (col in cols) {
    index              <- match(col, names(data))
    names(data)[index] <- "x"
    idx    <- which(na_strings %in% data[["x"]])
    if (length(idx) > 0L) {
      data <- naniar::replace_with_na(data, replace = list(x = na_strings[idx]))
      indexes <- c(indexes, col)
    } else {
      res  <- res + 1L
    }
    names(data)[index] <- col
  }
  stopifnot("Could not detect missing value character! Please use the
                appropriate strings that represents the missing values from
                your data." = res < ncol(data))

  # make report
  xx   <- ifelse(length(indexes) == ncol(data),
                 paste(names(data), collapse = ", "),
                 paste(indexes, collapse = ", "))
  data <- add_to_report(x     = data,
                        key   = "missing_values_replaced_at",
                        value = xx)

  return(data)
}
