#' Check whether the subject IDs comply with the expected format. When incorrect
#' IDs are found, the function sends a warning and the user can call the
#' `correct_subject_ids()` function to correct them.
#'
#' @param data The input data frame or linelist
#' @param target_columns A vector of column names with the subject ids.
#' @param prefix A prefix used in the subject IDs
#' @param suffix A suffix used in the subject IDs
#' @param range A vector with the range of numbers in the sample IDs
#' @param nchar An integer that represents the expected number of characters in
#'    the subject ids.
#'
#' @returns The input dataset with a warning if incorrect subject ids were found
#'
#' @examples
#' dat <- check_subject_ids(
#'   data           = readRDS(system.file("extdata", "test_df.RDS",
#'                                        package = "cleanepi")),
#'   target_columns = "study_id",
#'   prefix         = "PS",
#'   suffix         = "P2",
#'   range          = c(1, 100),
#'   nchar          = 7
#' )
#' @export
check_subject_ids <- function(data,
                              target_columns,
                              prefix         = NULL,
                              suffix         = NULL,
                              range          = NULL,
                              nchar          = NULL) {
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_character(target_columns, null.ok = FALSE,
                              any.missing = FALSE, len = 1L)
  checkmate::assert_character(prefix, len = 1L, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_character(suffix, len = 1L, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_vector(range, any.missing = FALSE, min.len = 2L,
                           null.ok = TRUE, unique = TRUE, max.len = 2L)
  checkmate::assert_numeric(nchar, null.ok = TRUE, any.missing = FALSE,
                            len = 1L)

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)

  # coerce id column to character
  if (is.numeric(data[[target_columns]]) || is.factor(data[[target_columns]])) {
    data[[target_columns]] <- as.character(data[[target_columns]])
  }

  # check for missing and duplicated ids
  data     <- check_subject_ids_oness(data, target_columns)
  bad_rows <- NULL

  # check prefix of subject IDs
  if (!is.null(prefix)) {
    prefix_check    <- startsWith(data[[target_columns]], prefix)
    idx             <- which(!(prefix_check))
    if (length(idx) > 0L) {
      bad_rows      <- c(bad_rows, idx)
    }
  }

  # check suffix of subject IDs
  if (!is.null(suffix)) {
    suffix_check <- endsWith(data[[target_columns]], suffix)
    idx          <- which(!(suffix_check))
    if (length(idx) > 0L) {
      bad_rows   <- c(bad_rows, idx)
    }
  }

  # detect subject IDs that do not match the provided format
  if (!is.null(nchar)) {
    length_check <- as.logical(as.character(lapply(data[[target_columns]],
                                                   check_subject_ids_length,
                                                   nchar)))
    idx          <- which(length_check)
    if (length(idx) > 0L) {
      bad_rows   <- c(bad_rows, idx)
    }
  }

  # check the numbers in the sample IDs
  if (!is.null(range)) {
    numbers_in <- as.numeric(unlist(lapply(data[[target_columns]],
                                           readr::parse_number)))
    idx        <- which(!(numbers_in >= min(range) & numbers_in <= max(range)))
    if (length(idx) > 0L) {
      bad_rows <- c(bad_rows, idx)
    }
  }

  # remove the incorrect rows
  if (!is.null(bad_rows)) {
    bad_rows     <- unique(bad_rows)
    tmp_report   <- data.frame(idx = bad_rows,
                               ids = data[[target_columns]][bad_rows])
    bad_rows     <- glue::glue_collapse(bad_rows, sep = ", ")
    warning("Detected incorrect subject ids at lines: ", bad_rows,
            "\nUse the correct_subject_ids() function to adjust them.",
            call. = FALSE)
    data         <- add_to_report(x     = data,
                                  key   = "incorrect_subject_id",
                                  value = tmp_report)
  }

  return(data)
}

#' Correct the wrong subject IDs based on the user-provided values.
#'
#' After detecting incorrect subject IDs from the `check_subject_ids()`
#' function, use this function to provide the correct IDs and perform the
#' substitution.
#'
#' @inheritParams check_subject_ids
#' @param correction_table A data frame with the following two columns:
#'  \enumerate{
#'    \item `from`: a column with the wrong subject IDs,
#'    \item `to`: a column with the values to be used to substitute the
#'        incorrect ids.
#'  }
#'
#' @return The input dataset where all subject ids comply with the expected
#'    format.
#' @export
#'
#' @examples
#' # detect the incorrect subject ids
#' dat <- check_subject_ids(
#'   data           = readRDS(system.file("extdata", "test_df.RDS",
#'                                        package = "cleanepi")),
#'   target_columns = "study_id",
#'   prefix         = "PS",
#'   suffix         = "P2",
#'   range          = c(1, 100),
#'   nchar          = 7
#' )
#'
#' # generate the correction table
#' correction_table <- data.frame(
#'   from = c("P0005P2", "PB500P2", "PS004P2-1"),
#'   to   = c("PB005P2", "PB050P2", "PS004P2")
#' )
#'
#' # perform the correction
#' dat <- correct_subject_ids(
#'   data             = dat,
#'   target_columns   = "study_id",
#'   correction_table = correction_table
#' )
correct_subject_ids <- function(data, target_columns, correction_table) {
  checkmate::assert_data_frame(correction_table, any.missing = FALSE,
                               min.rows = 1L, ncols = 2L, null.ok = FALSE,
                               col.names = "named")
  checkmate::assert_names(names(correction_table),
                          identical.to = c("from", "to"))

  stopifnot("Some ids in the correction table were not found in the input data"
            = all(correction_table[["from"]] %in% data[[target_columns]]))

  # perform the substitution
  idx                         <- match(correction_table[["from"]],
                                       data[[target_columns]])
  data[[target_columns]][idx] <- correction_table[["to"]]

  # check whether substitution did not introduce any duplicate
  data                        <- check_subject_ids_oness(data, target_columns)

  return(data)
}

#' Checks the uniqueness in values of the sample IDs column
#'
#' @param data the input data frame
#' @param id_col_name the name of the column that contains the sample IDs
#'
#' @return the input data frame with and extra element in its attributes when
#'    there are missing or duplicated IDs.
#' @keywords internal
#'
check_subject_ids_oness <- function(data, id_col_name) {
  # check for missing values in ID column
  if (anyNA(data[[id_col_name]])) {
    idx                <- which(is.na(data[[id_col_name]]))
    warning("\nMissing values found on ID column in lines: ",
            glue::glue_collapse(idx, sep = ", "), call. = FALSE)
    data <- add_to_report(x     = data,
                          key   = "missing_ids",
                          value = glue::glue_collapse(idx, sep = ", "))
  }

  # check for duplicates ID column
  duplicated_ids       <- find_duplicates(data, id_col_name)
  tmp_report           <- attr(duplicated_ids, "report")
  if (!is.null(tmp_report) && "duplicated_rows" %in% names(tmp_report) &&
      nrow(tmp_report[["duplicated_rows"]]) > 0L) {
    dups               <- tmp_report[["duplicated_rows"]]
    data               <- add_to_report(x     = data,
                                        key   = "duplicated_ids",
                                        value = dups)
  }

  return(data)
}

#' Check the length of sample IDs
#'
#' @param x the sample ID
#' @param ref the expected number of characters in the sample ids
#' @keywords internal
check_subject_ids_length <- function(x, ref) {
  return(nchar(x) != ref)
}
