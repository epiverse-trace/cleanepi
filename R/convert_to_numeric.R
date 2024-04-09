#' Convert columns into numeric
#'
#' When the function is invoked without specifying the column names to be
#' converted, it automatically scans for columns containing exclusively missing,
#' numeric, and character values. Furthermore, it identifies columns where the
#' proportion of numeric values is at least twice the percentage of character
#' values and performs the conversion in them.
#'
#' @param data The input data frame or linelist
#' @param target_columns A vector of the target column names. When the input
#'    data is a `linelist` object, this parameter can be set to `linelist_tags`
#'    if the tagged columns are those to be converted into numeric.
#' @param lang The text's language. Currently one of `"en" | "fr" | "es"`.
#'
#' @return A data frame after the conversion process, wherein all the specified
#'    or detected columns have been transformed into numeric format.
#' @export
#'
#' @examples
#' dat <- convert_to_numeric(
#'   data           = readRDS(system.file("extdata", "messy_data.RDS",
#'                                        package = "cleanepi")),
#'   target_columns = "age",
#'   lang           = "en"
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
    target_columns <- detect_to_numeric_columns(scan_res)
  }

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns   <- get_target_column_names(data, target_columns, cols = NULL)

  stopifnot("Please specify the target columns." = length(target_columns) > 0L)
  for (col in target_columns) {
    # data[[col]]    <- to_numeric_convert(data[[col]])
    data[[col]]    <- numberize::numberize(text = data[[col]], lang = lang)
  }
  data             <- add_to_report(x     = data,
                                    key   = "converted_into_numeric",
                                    value = glue::glue_collapse(target_columns,
                                                                sep = ", "))
  return(data)
}

#' Detect the numeric columns that appears as characters due to the presence of
#' some character values in the column.
#'
#' @param scan_res a data frame that corresponds to the result from the
#'    `scan_data()` function
#'
#' @return a vector of column names to be converted into numeric
#' @keywords internal
#'
detect_to_numeric_columns <- function(scan_res) {
  checkmate::assert_data_frame(scan_res, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  to_numeric <- vector(mode = "character", length = 0L)
  for (col in scan_res[["Field_names"]]) {
    idx <- match(col, scan_res[["Field_names"]])
    values        <- scan_res[idx, 2L:ncol(scan_res)]
    names(values) <- colnames(scan_res)[2L:ncol(scan_res)]
    values        <- values[which(values > 0L)]
    if ("missing" %in% names(values)) {
      values <- values[-(which(names(values) == "missing"))]
    }
    if (length(values) == 2L && "numeric" %in% names(values) &&
        "character" %in% names(values)) {
      if (values[["numeric"]] > (2.0 * values[["character"]])) {
        to_numeric <- c(to_numeric, col)
      } else if (values[["numeric"]] < (2L * values[["character"]])) {
          warning(sprintf("In '%s' column, the number of numeric values is\n
                          same as the number of character values", col),
                  call. = FALSE)
      }
    } else {
      next
    }
  }
  return(to_numeric)
}

#' Convert values in a character vector into numeric
#'
#' The conversion is only applied on non-missing and non-numeric values found
#' from the input vector
#'
#' @param x a vector of type character
#'
#' @return a vector of type numeric with the same length as the input vector
#' @keywords internal
#'
to_numeric_convert <- function(x) {
  if (all(is.numeric(x))) {
    return(as.numeric(x))
  }
  xx                        <- suppressWarnings(as.numeric(x))
  after_conversion_is_na    <- setdiff(which(is.na(xx)), which(is.na(x)))
  x[after_conversion_is_na] <- unlist(lapply(x[after_conversion_is_na],
                                             numberize::numberize,
                                             lang = "en"))
  return(as.numeric(x))
}
