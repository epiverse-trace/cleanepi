#' Detect whether a string contains only numbers
#'
#' @param x the string that contains the numbers
#' @keywords internal
#' @noRd
numbers_only <- function(x) {
  !grepl("\\D", x)
}

#' Get part1 of date value
#'
#' @param x the string of interest
#' @param sep the separator in the string of interest
#' @keywords internal
#' @noRd
get_part1 <- function(x, sep) {
  if (is.na(x)) {
    return(NA)
  }
  unlist(strsplit(x, sep, fixed = TRUE))[[1L]]
}

#' Get part2 of date value
#'
#' @param x the string of interest
#' @param sep the separator in the string of interest
#' @keywords internal
#' @noRd
get_part2 <- function(x, sep) {
  if (is.na(x)) {
    return(NA)
  }
  unlist(strsplit(x, sep, fixed = TRUE))[[2L]]
}

#' Get part3 of date value
#'
#' @param x the string of interest
#' @param sep the separator in the string of interest
#' @keywords internal
#' @noRd
#'
get_part3 <- function(x, sep) {
  if (is.na(x)) {
    return(NA)
  }
  unlist(strsplit(x, sep, fixed = TRUE))[[3L]]
}

#' Get sum from number
#'
#' @param x the string of interest
#' @keywords internal
#' @noRd
#'
get_sum <- function(x) {
  if (nchar(x) == 2L) {
    x <- sum(as.numeric(substr(x, 1L, 1L)), as.numeric(substr(x, 2L, 2L)))
  }
  x
}

#' Check if date column exists in given data frame
#'
#' @param data the input data frame
#' @param date_column_name the date column of interest
#' @keywords internal
#' @noRd
#'
check_column_existence <- function(data, date_column_name) {
  # check the column name
  if (is.null(date_column_name)) {
    idx <- which(names(data) %in% c("Date", "DATE", "date"))
    if (length(idx) == 0L) {
      stop("Could not find column named as ",
           glue::glue_collapse(c("Date", "DATE", "date"), sep = " or "),
           "\nPlease specify the date column name.")
    }
    date_column_name <- names(data)[idx]
  }

  # check whether the provided column name belong to the data
  if (!all(date_column_name %in% names(data))) {
    idx <- which(!(date_column_name %in% names(data)))
    stop("Can't find column: ", date_column_name[idx])
  }
  date_column_name
}

#' Detect the numeric columns that appears as characters due to the presence of
#' some character values in the column.
#'
#' @param scan_res a data frame that corresponds to the result from the
#'    `scan_data()` function
#'
#' @return a vector of column names to be converted into numeric
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#'   scan_res <- scan_data(data = readRDS(system.file("extdata",
#'                         "messy_data.RDS",
#'                         package = "cleanepi")))
#'   to_numeric <- detect_columns_to_convert(scan_res = scan_res)
#' }
#'
detect_columns_to_convert <- function(scan_res) {
  checkmate::assert_data_frame(scan_res, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  to_numeric <- vector(mode = "character", length = 0L)
  for (col in names(scan_res)[-1L]) {
    values        <- scan_res[[col]]
    names(values) <- scan_res[["data_type"]]
    values        <- values[values > 0L]
    if ("missing" %in% names(values)) {
      values <- values[-(which(names(values) == "missing"))]
    }
    if (length(values) == 2L && "numeric" %in% names(values) &&
          "character" %in% names(values)) {
      if (values[["numeric"]] == values[["character"]] ||
            values[["numeric"]] < (2L * values[["character"]])) {
        warning(sprintf("In '%s' column, the number of numeric values is same as
                        the number of character values", col), call. = FALSE)
      } else {
        to_numeric <- c(to_numeric, col)
      }
    } else {
      next
    }
  }
  to_numeric
}

#' Convert provided or auto-detected character columns into numeric
#'
#' When the names of the columns to be converted is not provided, the function
#' looks for columns that contains only missing, numeric and character values
#' and where the percentage of numeric values is at least 2 times the percent of
#' character values.
#'
#' @param data the input data frame
#' @param report the report object
#' @param to_numeric a vector of column names
#' @param scan_res a data frame obtained from the `scan_data()` function
#'
#' @return the input data frame where all the specified or the detected columns
#'    (as described above) have been converted into numeric
#' @keywords internal
#' @noRd
#'
#' @examples
#' data = readRDS(system.file("extdata", "messy_data.RDS",
#'                package = "cleanepi"))
#' to_numeric = "age"
convert_to_numeric <- function(data,
                               report     = NULL,
                               to_numeric = NULL,
                               scan_res   = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_list(report, min.len = 0L, null.ok = TRUE)
  checkmate::assert_vector(to_numeric, any.missing = FALSE, min.len = 0L,
                           null.ok = TRUE)
  checkmate::assert_data_frame(scan_res, min.rows = 1L, min.cols = 1L,
                               null.ok = TRUE)
  if (is.null(to_numeric)) {
    if (is.null(scan_res)) {
      scan_res   <- scan_data(data = data)
    }
    to_numeric   <- detect_columns_to_convert(scan_res)
  }
  data <- data %>%
    dplyr::mutate(dplyr::across({{ to_numeric }}, ~ convert(.x)))

  report[["converted_into_numeric"]] <- glue::glue_collapse(to_numeric,
                                                            sep = ", ")
  list(
    data   = data,
    report = report
  )
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
#' @noRd
#'
#' @examples
#' x     <- c(2, "ten", NA, 2, "twenty", 6)
#' num_x <- convert(x)
#'
convert <- function(x) {
  tmp   <- x
  is_na <- which(is.na(x))
  x     <- suppressWarnings(as.numeric(x))
  if (length(is_na) > 0L) {
    xx  <- which(is.na(x))
    y   <- xx[!(xx %in% is_na)]
  }
  converted <- unlist(lapply(tmp[y], numberize::numberize, lang = "en"))
  x[y]      <- converted
  x
}


#' Convert Redcap data dictionary into {matchmaker} dictionary format
#'
#' @param metadata a `data frame` with the data dictionary associated to a
#'    Redcap project
#' @param field_column the name of the column, in the input dictionary, that
#'    contains the field names in of the Redcap project data
#' @param opt_column the name of the column, in the input dictionary, that
#'    contains the definition of the choices in every column of the Redcap
#'    project data
#'
#' @return a data frame with 4 columns. This is in the format required by the
#'    {matchmaker} R package for dictionary-based cleaning.
#' @keywords internal
#' @noRd
#'
make_readcap_dictionary <- function(metadata,
                                    field_column = "field_name",
                                    opt_column   = "select_choices_or_calculations") { # nolint: line_length_linter
  checkmate::assert_data_frame(metadata, min.rows = 1, min.cols = 1,
                               null.ok = FALSE)
  checkmate::assert_character(opt_column, len = 1, null.ok = FALSE)

  stopifnot(opt_column %in% names(metadata))

  metadata     <- metadata %>%
    dplyr::filter(!is.na(metadata[[opt_column]]))

  dictionary   <- NULL
  for (i in 1:nrow(metadata)) {
    print(i)
    dictionary <- rbind(dictionary,
                        make_metadata(metadata[[opt_column]][i],
                                      metadata[[field_column]][i]))
  }
  dictionary
}

#' Make data dictionary for 1 field
#'
#' @param x a `vector` choices from the input data
#' @param field_column a `character` with the name of the field where the
#'    choices are made.
#'
#' @return a `data frame` with the dictionary in the format that is accepted by
#'    the {matchmaker} package.
#' @keywords internal
#' @noRd
#'
make_metadata <- function(x, field_column) {
  splits          <- unlist(strsplit(x, "|", fixed = TRUE))
  splits          <- trimws(splits)
  combined_splits <- lapply(splits, get_meta_rows)
  combined_splits <- do.call(rbind.data.frame, combined_splits)
  res             <- data.frame(options = combined_splits[, 1L],
                                values  = combined_splits[, 2L],
                                grp     = rep(field_column,
                                              nrow(combined_splits)),
                                orders  = seq_len(nrow(combined_splits)))
  rownames(res)   <- NULL
  res
}

#' Split a character and remove white space at the beginning and the end
#'
#' @param x a `character` to be split
#'
#' @keywords internal
#' @noRd
#'
#' @returns a `vector` of character
get_meta_rows <- function(x) {
  x <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
}
