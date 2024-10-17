#' Convert columns into numeric
#'
#' When the function is invoked without specifying the column names to be
#' converted, the target columns are the ones returned by the `scan_data()`
#' function. Furthermore, it identifies columns where the proportion of numeric
#' values is at least twice the percentage of character values and performs the
#' conversion in them.
#'
#' @param data The input data frame or linelist
#' @param target_columns A vector of the target column names. When the input
#'    data is a `linelist` object, this parameter can be set to
#'    \code{linelist_tags} if the tagged columns are those to be converted into
#'    numeric.
#' @param lang The text's language. Currently one of \code{"en"}, \code{"fr"},
#'    \code{"es"}.
#'
#' @returns A data frame wherein all the specified or detected columns have been
#'    transformed into numeric format after the conversion process.
#' @export
#'
#' @examples
#' dat <- convert_to_numeric(
#'   data = readRDS(
#'     system.file("extdata", "messy_data.RDS", package = "cleanepi")
#'   ),
#'   target_columns = "age",
#'   lang = "en"
#' )
convert_to_numeric <- function(data, target_columns = NULL,
                               lang = c("en", "fr", "es")) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_vector(target_columns, any.missing = FALSE, min.len = 0L,
                           null.ok = TRUE)
  lang <- match.arg(lang)
  if (is.null(target_columns)) {
    scan_res       <- scan_data(data = data)
    stopifnot("Please specify the target column names." = !is.na(scan_res))
    target_columns <- detect_to_numeric_columns(scan_res)
  }

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns <- get_target_column_names(data, target_columns, cols = NULL)

  stopifnot("Please specify the target columns." = length(target_columns) > 0L)
  for (col in target_columns) {
    data[[col]]  <- numberize::numberize(text = data[[col]], lang = lang)
  }
  data           <- add_to_report(x     = data,
                                  key   = "converted_into_numeric",
                                  value = toString(target_columns))
  return(data)
}

#' Detect the numeric columns that appears as characters due to the presence of
#' some character values in the column.
#'
#' @param scan_res a data frame that corresponds to the result from the
#'    `scan_data()` function
#'
#' @returns a vector of column names to be converted into numeric
#' @keywords internal
#'
detect_to_numeric_columns <- function(scan_res) {
  checkmate::assert_data_frame(scan_res, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  to_numeric <- vector(mode = "character", length = 0L)
  scan_res <- tibble::column_to_rownames(scan_res, "Field_names")
  for (col in rownames(scan_res)) {
    values <- unlist(scan_res[col, ])
    values <- values[values > 0L]
    values <- values[names(values) != "missing"]
    if (setequal(names(values), c("numeric", "character"))) {
      if (values[["numeric"]] > (2 * values[["character"]])) {
        to_numeric <- c(to_numeric, col)
      } else if (values[["numeric"]] < (2 * values[["character"]])) {
          warning(sprintf("In '%s' column, the number of numeric values", col),
                          " is the same or less than twice the number of",
                          " character values",
                  call. = FALSE)
      }
    }
  }
  return(to_numeric)
}
