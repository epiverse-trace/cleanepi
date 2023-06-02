#' Check whether the subject IDs comply with the expected format
#'
#' @param data the data frame of interest
#' @param id_column_name the name of the column with the subject IDs. If not
#' specified, the first column will be considered by default
#' @param format the expected subject IDs format
#' @param prefix the prefix used in the subject IDs
#' @param suffix the prefix used in the subject IDs
#' @param range a vector with the range of numbers in the sample IDs
#' @param remove a Boolean to specify whether to remove rows with incorrect
#' @param verbose a Boolean to specify whether print the detected incorrect
#'    subject IDs
#' @param report the report object
#' sample IDs. default is FALSE
#' @returns if found, the function return a list with 2 elements: the cleaned
#'    data frame with correct subject IDs and a report containing the rows of
#'    the input data frame with incorrect subject IDs
#' @examples
#' dat <- check_subject_ids(
#' data = data.table::fread(system.file("extdata", "test.txt",
#' package = "cleanepi")),
#' id_column_name = "study_id",
#' format = "PS000P2",
#' prefix = "PS",
#' suffix = "P2",
#' range = c(1,100),
#' remove = FALSE,
#' verbose = TRUE,
#' report = list()
#' )
#' @export
check_subject_ids <- function(data, id_column_name = NULL, format,
                             prefix = NULL, suffix = NULL, range = NULL,
                             remove = FALSE, verbose = FALSE, report = list()) {
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_character(id_column_name, null.ok = TRUE,
                              any.missing = FALSE, len = 1)
  checkmate::assert_character(format, len = 1, null.ok = FALSE,
                              any.missing = FALSE)
  checkmate::assert_character(prefix, len = 1, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_character(suffix, len = 1, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_vector(range, any.missing = FALSE, min.len = 2,
                           null.ok = TRUE, unique = TRUE, max.len = 2)
  checkmate::assert_logical(remove, any.missing = FALSE, len = 1,
                            null.ok = FALSE)
  checkmate::assert_logical(verbose, any.missing = FALSE, len = 1,
                            null.ok = FALSE)
  subject_id_col_name <- ifelse(!is.null(id_column_name), id_column_name,
                                names(data)[1])

  bad_rows <- NULL
  # check prefix of subject IDs
  if (!is.null(prefix)) {
    prefix_check <- as.logical(as.character(lapply(data[[subject_id_col_name]],
                                                  check_prefix, prefix)))
    idx <- which(!(prefix_check))
    if (length(idx) > 0) {
      bad_rows <- c(bad_rows, idx)
      failed_prefix <- data[[subject_id_col_name]][idx]
    }
    if (verbose) {
      message("\nSample IDs with wrong prefix:")
      print(failed_prefix)
    }
  }

  # check suffix of subject IDs
  if (!is.null(suffix)) {
    suffix_check <- as.logical(as.character(lapply(data[[subject_id_col_name]],
                                                   check_suffix, suffix)))
    idx <- which(!(suffix_check))
    if (length(idx) > 0) {
      bad_rows <- c(bad_rows, idx)
      failed_suffix <- data[[subject_id_col_name]][idx]
    }
    if (verbose) {
      message("\nSample IDs with wrong suffix:")
      print(failed_suffix)
    }
  }

  # detect subject IDs that do not match the provided format
  length_check <- as.logical(as.character(lapply(data[[subject_id_col_name]],
                                                 check_id_length, format)))
  idx <- which(!(length_check))
  if (length(idx) > 0) {
    bad_rows <- c(bad_rows, idx)
    failed_length <- data[[subject_id_col_name]][idx]
    if (verbose) {
      message("\nSample IDs with wrong incorrect length:")
      print(failed_length)
    }
  }

  # check the numbers in the sample IDs
  if (!is.null(range)) {
    numbers_in <- as.numeric(unlist(lapply(data[[subject_id_col_name]],
                                          readr::parse_number)))
    idx <- which(!(numbers_in >= min(range) & numbers_in <= max(range)))
    if (length(idx) > 0) {
      bad_rows <- c(bad_rows, idx)
      failed_range <- data[[subject_id_col_name]][idx]
    }
    if (verbose) {
      message("\nSample IDs with wrong numbers:")
      print(failed_range)
    }
  }

  # remove the incorrect rows
  bad_rows <- unique(bad_rows)
  if (!("incorrect_subject_id" %in% names(report))) {
    report[["incorrect_subject_id"]] <- NULL
  }
  report[["incorrect_subject_id"]] <- rbind(report[["incorrect_subject_id"]],
                                            data[bad_rows, ]
  )
  if (remove) {
    data <- data[-bad_rows, ]
  }

  # report the rows with incorrect subject IDs


  list(
    data = data,
    report = report
    )
}
