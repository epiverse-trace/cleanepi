#' Get the percentage of missing and other data type values in a vector.
#' The considered data types are: `numeric`, `Date`, `character`, `logical`
#'
#' @param x a vector of 1 or a combination of the values of the type mentioned
#'    above
#'
#' @return a vector of 4 elements that represent respectively the percent of:
#'    missing, numeric, Date and character values found in the input vector.
#'
#' @keywords internal
#'
get_column_composition <- function(x) {
  # --- save the variable length ---
  n_rows <- length(x)

  # --- get the proportion of NA ---
  are_na <- round((sum(is.na(x)) / n_rows), 6)
  x      <- x[!is.na(x)]

  # --- get the proportion of numeric values ---
  tmp         <- suppressWarnings(as.numeric(x))
  are_numeric <- round((length(tmp[!is.na(tmp)]) / n_rows), 6)

  # --- get the proportion of date values ---
  x <- x[which(is.na(tmp))]
  if (!is.null(lubridate::guess_formats(x,
        c("ymd", "ydm", "dmy", "mdy", "myd", "dym","Ymd", "Ydm", "dmY", "mdY",
          "mYd", "dYm")))) {
    x = as_Date(x)
    are_date <- round((length(x[!is.na(x)]) / n_rows), 6)
  } else {
    are_date = 0
  }

  # --- get the proportion of logical values ---
  are_logical   <- round((length(x[is.logical(x)]) / n_rows), 6)

  # --- get the proportion of character values ---
  are_character <- round((1 - (are_na + are_numeric +
                                 are_date + are_logical)), 6)

  # --- return the output ---
  c(are_na, are_numeric, are_date, are_character, are_logical)
}

#' Scan a data frame object to determine the percentage of `missing`, `numeric`,
#'    `Date`, `character`, `logical` values in every column.
#'
#' @param data the input data frame
#'
#' @return a data frame with the same columns as the input data and 5 rows.
#'    These rows represent the percentage of missing, numeric, date, character,
#'    and logical values in each column.
#'
#' @export
#'
#' @examples
#' scan_result <- scan_data(
#'   data = readRDS(system.file("extdata", "messy_data.RDS",
#'                              package = "cleanepi"))
#' )
scan_data <- function(data) {
  scan_result           <- data.frame(apply(data, 2, get_column_composition))
  data_type_percent     <- data.frame(c("missing", "numeric", "date",
                                        "character", "logical"),
                                      stringsAsFactors = FALSE)
  scan_result           <- cbind(data_type_percent, scan_result)
  names(scan_result)[1] <- "data_type"
  scan_result
}

#' Check the uniqueness in values of the sample IDs column
#'
#' @param data the input data frame
#' @param id_col_name the name of the column that contains the sample IDs
#' @param report the object that will contains details about the result from the
#'    date columns standardization.
#'
#' @return a report object of type `list`. This should contain an element named
#'    as **duplicated_ids**. This is either a `data.frame` if there are rows
#'    with duplicated IDs or `NULL` if not.
#' @export
#'
#' @examples
#' \dontrun{
#' report <- check_ids_uniqueness(
#' data = readRDS(system.file("extdata", "messy_data.RDS",
#'     package = "cleanepi")),
#' id_col_name = "case_id",
#' )
#' }
check_ids_uniqueness <- function(data, id_col_name, report = list()) {
  # check for missing values in ID column
  scan_result <- scan_data(data)
  if (scan_result[[id_col_name]][1] != 0) {
    idx       <- which(is.na(data[[id_col_name]]))
    warning("\nMissing values found at ID column in lines: ",
         glue::glue_collapse(idx, ", "), call. = FALSE)
    report[["missing_ids"]] <- data[idx, ]
    data                    <- data[-idx, ]
  }

  # check for duplicates ID column
  duplicated_ids <- find_duplicates(data, id_col_name)
  if (nrow(duplicated_ids) > 0) {
    warning("\nFound duplicated IDs! See the cleaning report for more details",
            call. = FALSE)
    report[["duplicated_ids"]] <- duplicated_ids
  }

  list(
    data   = data,
    report = report
  )
}
