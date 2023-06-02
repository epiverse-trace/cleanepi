
#' Replace missing characters with NA
#'
#' @param data the input data frame
#' @param column_name the column in which to look for missing character for
#' @param na_char the character that represents the missing values in the column
#' of interest
#'
#' @return a data frame where missing values are represented by NA
replace_missing_char <- function(data, column_name, na_char = NULL) {
  index <- which(names(data) == column_name)
  names(data)[index] <- "x"
  if (!is.null(na_char)) {
    data <- naniar::replace_with_na(data,
                                    replace = list(x = na_char))
  } else {
    common_na_string <- c(naniar::common_na_strings, "not available",
                          "Not Available", "NOt available", "not avail",
                          "Not Avail", "nan", "NAN", "not a number",
                          "Not A Number", "-99")

    idx <- which(common_na_string %in% data[["x"]])
    if (length(idx) > 0) {
      data <- naniar::replace_with_na(data,
                                      replace = list(x = common_na_string[idx]))
    } else {
      stop("Could not detect missing value character! Please provide the
           character that represents the missing values.")
    }
  }
  names(data)[index] <- column_name
  data
}
