#' convert numeric into Date
#'
#' @param data the input data frame
#' @param numeric_date_columns a vector or a comma-separated list of the columns
#' names to be converted from numeric to date
#' @param ref_date the reference date
#' @param forward whether the counts started after the reference date (TRUE) or
#' not (FALSE). default is FALSE
#'
#' @return the input data frame where the column of interest are updated
#' @export
convert_numeric_to_date <- function(data, numeric_date_columns, ref_date,
                                    forward = TRUE) {
  checkmate::assert_date(ref_date, any.missing = FALSE, min.len = 1L,
                         max.len = 1L, null.ok = FALSE)
  checkmate::assert_character(numeric_date_columns, null.ok = FALSE,
                              any.missing = FALSE, min.len = 1L)
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  if (forward) {
    for (cols in numeric_date_columns) {
      data[[cols]] <- ref_date + data[[cols]]
    }
  } else {
    for (cols in numeric_date_columns) {
      data[[cols]] <- ref_date - data[[cols]]
    }
  }
  data
}
