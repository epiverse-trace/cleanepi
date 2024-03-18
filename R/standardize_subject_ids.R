#' Check whether the subject IDs comply with the expected format. When incorrect
#' IDs are found, the function sends a message and the user can call the
#' `correct_subject_ids()` function to correct them.
#'
#' @param data The input data frame or linelist
#' @param id_column_name A column name of  the subject IDs. If not
#'    specified, the first column will be considered by default.
#' @param format A  format of the subject IDs
#' @param prefix A prefix used in the subject IDs
#' @param suffix A suffix used in the subject IDs
#' @param range A vector with the range of numbers in the sample IDs
#' @param remove A logical that is used to whether to remove the incorrect
#'    subject IDs or not. Default is `FALSE`.
#'
#' @returns A cleaned data frame with only the correct subject IDs if
#'    `remove = TRUE`, or the input dataset otherwise. The incorrect subject ids
#'    will be stored in the report object.
#'
#' @examples
#' dat <- check_subject_ids(
#'   data           = readRDS(system.file("extdata", "test_df.RDS",
#'                                        package = "cleanepi")),
#'   id_column_name = "study_id",
#'   format         = NULL,
#'   prefix         = "PS",
#'   suffix         = "P2",
#'   range          = c(1, 100),
#'   remove         = TRUE
#' )
#' @export
check_subject_ids <- function(data,
                              id_column_name,
                              format         = NULL,
                              prefix         = NULL,
                              suffix         = NULL,
                              range          = NULL,
                              remove         = FALSE) {
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_character(id_column_name, null.ok = FALSE,
                              any.missing = FALSE, len = 1L)
  checkmate::assert_character(format, len = 1L, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_character(prefix, len = 1L, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_character(suffix, len = 1L, null.ok = TRUE,
                              any.missing = FALSE)
  checkmate::assert_vector(range, any.missing = FALSE, min.len = 2L,
                           null.ok = TRUE, unique = TRUE, max.len = 2L)
  checkmate::assert_logical(remove, any.missing = FALSE, len = 1L,
                            null.ok = FALSE)

  data     <- check_subject_ids_oness(data, id_column_name)
  bad_rows <- NULL
  # check prefix of subject IDs
  if (!is.null(prefix)) {
    prefix_check    <- startsWith(data[[id_column_name]], prefix)
    idx             <- which(!(prefix_check))
    if (length(idx) > 0L) {
      bad_rows      <- c(bad_rows, idx)
    }
  }

  # check suffix of subject IDs
  if (!is.null(suffix)) {
    suffix_check <- endsWith(data[[id_column_name]], suffix)
    idx          <- which(!(suffix_check))
    if (length(idx) > 0L) {
      bad_rows   <- c(bad_rows, idx)
    }
  }

  # detect subject IDs that do not match the provided format
  if (!is.null(format)) {
    length_check <- as.logical(as.character(lapply(data[[id_column_name]],
                                                   check_subject_ids_length,
                                                   format)))
    idx          <- which(!(length_check))
    if (length(idx) > 0L) {
      bad_rows   <- c(bad_rows, idx)
    }
  }

  # check the numbers in the sample IDs
  if (!is.null(range)) {
    numbers_in <- as.numeric(unlist(lapply(data[[id_column_name]],
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
                               ids = data[[id_column_name]][bad_rows])
    if (remove) {
      data       <- data[-bad_rows, ]
      message(nrow(tmp_report), " incorrect subject IDs were removed. See the ",
              "report for more details.")
    } else {
      message("Found ", nrow(tmp_report), " incorrect subject IDs. Use the ",
              "'correct_subject_ids()' function to correct them. See the ",
              "report for more details.")
    }
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
#'   id_column_name = "study_id",
#'   format         = NULL,
#'   prefix         = "PS",
#'   suffix         = "P2",
#'   range          = c(1, 100),
#'   remove         = FALSE
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
#'   id_column_name   = "study_id",
#'   correction_table = correction_table
#' )
correct_subject_ids <- function(data, id_column_name, correction_table) {
  checkmate::assert_data_frame(correction_table, any.missing = FALSE,
                               min.rows = 1L, ncols = 2L, null.ok = FALSE)

  stopifnot("Some ids in the correction table were not found in the input data"
            = all(correction_table[["from"]] %in% data[[id_column_name]]))

  # perform the substitution
  idx                         <- match(correction_table[["from"]],
                                       data[[id_column_name]])
  data[[id_column_name]][idx] <- correction_table[["to"]]

  # check whether substitution did not introduce any duplicate
  data                        <- check_subject_ids_oness(data, id_column_name)

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
#' @param ref the template sample ID
#' @keywords internal
check_subject_ids_length <- function(x, ref) {
  return(nchar(ref) >= nchar(x))
}
