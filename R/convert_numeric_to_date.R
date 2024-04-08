#' Convert numeric to date
#'
#' @param data The input data frame or linelist
#' @param target_columns A vector or a comma-separated list of columns names to
#'    be converted from numeric to date. When the input data is a `linelist`
#'    object, this parameter can be set to `linelist_tags` if you wish to only
#'    convert the tagged columns.
#' @param ref_date A reference date. This can also be a character string with
#'    the name of the reference column.
#' @param forward A Boolean to indicate whether the counts started after the
#'    reference date (TRUE) or not (FALSE). The default is TRUE.
#'
#' @return A data frame where the column of interest are updated
#' @export
#'
#' @examples
#' data <- readRDS(system.file("extdata", "test_df1.RDS", package = "cleanepi"))
#' data <- convert_numeric_to_date(
#'   data           = data,
#'   target_columns = "recruted_on_day",
#'   ref_date       = as.Date("2022-10-13"),
#'   forward        = TRUE
#' )
convert_numeric_to_date <- function(data, target_columns, ref_date,
                                    forward = TRUE) {
  if (!checkmate::test_character(ref_date, len = 1L, null.ok = FALSE)) {
    checkmate::assert_date(ref_date, any.missing = FALSE, min.len = 1L,
                           max.len = 1L, null.ok = FALSE)
  }
  checkmate::assert_vector(target_columns, min.len = 1, max.len = ncol(data),
                           null.ok = FALSE, any.missing = FALSE)
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)

  # if target column is character string, convert it to vector
  if (all(grepl(",", target_columns, fixed = TRUE))) {
    target_columns <- as.character(unlist(strsplit(target_columns, ",",
                                                   fixed = TRUE)))
    target_columns <- trimws(target_columns)
  }

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns <- get_target_column_names(data, target_columns, cols = NULL)

  if (is.character(ref_date)) {
    stopifnot("Unrecognised column name" = ref_date %in% colnames(data))
    ref_date <- data[[ref_date]]
  }

  if (forward) {
    for (cols in target_columns) {
      data[[cols]] <- ref_date + data[[cols]]
    }
  } else {
    for (cols in target_columns) {
      data[[cols]] <- ref_date - data[[cols]]
    }
  }
  return(data)
}
