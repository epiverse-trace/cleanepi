#' Convert columns into numeric
#'
#' When this function is invoked without specifying the column names to be
#' converted, the target columns are the ones returned by the \code{scan_data()}
#' function. Furthermore, it identifies columns where the proportion of numeric
#' values is at least twice the percentage of character values and performs the
#' conversion in them. The function internally makes call of the main function
#' from the {numberize}.
#'
#' @param data The input \code{<data.frame>} or \code{<linelist>}
#' @param target_columns A \code{<vector>} of the target column names. When the
#'    input data is a \code{<linelist>} object, this parameter can be set to
#'    \code{linelist_tags} to apply the conversion exclusively to the
#'    tagged columns. .
#' @param lang A \code{<character>} with the text's language. Currently one of
#'    \code{"en"}, \code{"fr"}, \code{"es"}.
#'
#' @returns A \code{<data.frame>} or \code{<linelist>} wherein all the specified
#'    or detected columns have been transformed into numeric format after the
#'    conversion process.
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
    scan_res <- scan_data(data = data)
    if (!is.data.frame(scan_res)) {
      cli::cli_abort(c(
        tr_("Automatic detection of columns to convert into numeric failed."), # nolint: line_length_linter
        x = tr_("No character column with numeric values found by {.fn scan_data}."), # nolint: line_length_linter
        i = tr_("Please specify names of the columns to convert into numeric using {.emph target_columns}.") # nolint: line_length_linter
      ))
    }
    # detect_to_numeric_columns() returns a vector of zero length when no
    # target column is identified.
    target_columns <- detect_to_numeric_columns(scan_res, data)
  }

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns <- get_target_column_names(data, target_columns, cols = NULL)

  # scan_data() might find columns where the %character is equal or less than
  # twice the %numeric. Such columns will be considered as the target columns.
  # When the %character > 2*%numeric, a warning is triggered about the presence
  # of numeric values in that column.
  # We abort here due to the zero length vector from detect_to_numeric_columns()
  if (length(target_columns) == 0) {
    cli::cli_abort(c(
      tr_("Found one or more columns with insuffisient numeric values for automatic conversion."), # nolint: line_length_linter
      i = tr_("The percentage of character values must be less than twice the numeric values for a column to be considered for automatic conversion."), # nolint: line_length_linter
      i = tr_("Please specify names of the columns to convert into numeric using {.emph target_columns}.") # nolint: line_length_linter
    ))
  }

  for (col in target_columns) {
    data[[col]]  <- numberize::numberize(text = data[[col]], lang = lang)
  }
  data <- add_to_report(
    x = data,
    key = "converted_into_numeric",
    value = toString(target_columns)
  )
  return(data)
}

#' Detect the numeric columns that appears as characters due to the presence of
#' some character values in the column.
#'
#' @inheritParams convert_to_numeric
#' @param scan_res A \code{<data.frame>} that corresponds to the result from the
#'    \code{\link{scan_data}} function
#'
#' @returns a \code{<vector>} of column names to be converted into numeric
#' @keywords internal
#'
detect_to_numeric_columns <- function(scan_res, data) {
  checkmate::assert_data_frame(scan_res, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  to_numeric <- vector(mode = "character", length = 0L)
  scan_res <- tibble::column_to_rownames(scan_res, "Field_names")
  for (col in rownames(scan_res)) {
    values <- unlist(scan_res[col, ])
    values <- values[values > 0L]
    values <- values[names(values) != "missing"]
    if (setequal(names(values), c("numeric", "character"))) {
      if (values[["character"]] <= (2 * values[["numeric"]])) {
        to_numeric <- c(to_numeric, col)
      } else if (values[["numeric"]] > 0) {
        num_values <- values[["numeric"]]
        num_values <- num_values * nrow(data)
        cli::cli_inform(c(
          "!" = tr_("Found {.val {num_values}} numeric value{?s} in {.field {col}}. "), # nolint: line_length_linter
          i = tr_("Please consider the following options:"),
          "*" = tr_("Converting characters into numeric"),
          "*" = tr_("Replacing the numeric values by {.val NA} using the {.fn replace_missing_values} function.") # nolint: line_length_linter
        ), wrap = TRUE)
      }
    }
  }
  if (length(to_numeric) > 0) {
    cli::cli_alert_info(
      tr_("The following column{?s} will be converted into numeric: {.field {to_numeric}}.") # nolint: line_length_linter
    )
  }
  return(to_numeric)
}
