
#' Replace missing values with `NA`
#'
#' @param data the input data
#' @param from a vector of column names. If provided, the missing values
#'    substitution will only be performed in those columns
#' @param na_comes_as the string that represents the missing values in the
#'    column of interest
#'
#' @return the input data where missing values are replaced by `NA`
#' @export
#'
#' @examples
#' cleaned_data <- replace_missing_values(
#'   data        = readRDS(system.file("extdata", "test_df.RDS",
#'                                     package = "cleanepi")),
#'   from        = "sex",
#'   na_comes_as = "-99"
#' )
#'
replace_missing_values <- function(data, from = NULL, na_comes_as = NULL) {
  # define the common NA strings
  common_na_string <- c(naniar::common_na_strings, "not available",
                        "Not Available", "NOt available", "not avail",
                        "Not Avail", "nan", "NAN", "not a number",
                        "Not A Number", "-99")

  # get the target columns
  index        <- seq_len(ncol(data))
  if (!is.null(from)) {
    index      <- which(names(data) == from)
  }
  cols         <- names(data)[index]

  # replace missing values with NA
  res <- 0L
  indexes <- NULL
  for (col in cols) {
    index              <- match(col, names(data))
    names(data)[index] <- "x"
    if (!is.null(na_comes_as)) {
      data   <- naniar::replace_with_na(data, replace = list(x = na_comes_as))
    } else {
      idx    <- which(common_na_string %in% data[["x"]])
      if (length(idx) > 0L) {
        data <- naniar::replace_with_na(data,
                                        replace = list(x = common_na_string[idx])) # nolint: line_length_linter
        indexes <- c(indexes, col)
      } else {
        res  <- res + 1L
      }
      names(data)[index] <- col
    }
  }
  stopifnot("Could not detect missing value character! Please provide the
                character that represents the missing values." =
              res < ncol(data))

  # make report
  xx   <- ifelse(length(indexes) == ncol(data),
                 paste(names(data), collapse = ", "),
                 paste(indexes, collapse = ", "))
  data <- add_to_report(x        = data,
                        name     = "missing_values_replaced_at",
                        add_this = xx)

  return(data)
}
