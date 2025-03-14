#' Check whether the subject IDs comply with the expected format. When incorrect
#' IDs are found, the function sends a warning and the user can call the
#' \code{\link{correct_subject_ids}} function to correct them.
#'
#' @param data The input \code{<data.frame>} or \code{<linelist>}
#' @param target_columns A \code{<vector>} of column names with the subject ids.
#' @param prefix A \code{<character>} with the expected prefix used in the
#'    subject IDs
#' @param suffix A \code{<character>} with the expected suffix used in the
#'    subject IDs
#' @param range A \code{<vector>} with the expected range of numbers in the
#'    subject IDs
#' @param nchar An \code{<integer>} that represents the expected number of
#'    characters in the subject ids.
#'
#' @returns The input dataset with a warning if incorrect subject ids were found
#'
#' @examples
#' data <- readRDS(
#'   system.file("extdata", "test_df.RDS", package = "cleanepi")
#' )
#'
#' # make first and last subject IDs the same
#' data$study_id[10] <- data$study_id[1]
#'
#' # set subject ID number 9 to NA
#' data$study_id[9] <- NA
#'
#' # detect the incorrect subject ids i.e. IDs that do not have any or both of
#' # the followings:
#' # - starts with 'PS',
#' # - ends with 'P2',
#' # - has a number within 1 and 100,
#' # - contains 7 characters.
#' dat <- check_subject_ids(
#'   data = data,
#'   target_columns = "study_id",
#'   prefix = "PS",
#'   suffix = "P2",
#'   range = c(1, 100),
#'   nchar = 7
#' )
#'
#' # display rows with invalid subject ids
#' print_report(dat, "incorrect_subject_id")
#' @export
check_subject_ids <- function(data,
                              target_columns,
                              prefix = NULL,
                              suffix = NULL,
                              range = NULL,
                              nchar = NULL) {
  checkmate::assert_data_frame(data, null.ok = FALSE)
  checkmate::assert_character(target_columns, null.ok = FALSE,
                              any.missing = FALSE, len = 1L)
  checkmate::assert_vector(prefix, min.len = 1L, null.ok = TRUE,
                           any.missing = FALSE)
  checkmate::assert_vector(suffix, min.len = 1L, null.ok = TRUE,
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
  res_check <- check_subject_ids_oness(data, target_columns)
  data <- res_check[["data"]]
  report <- res_check[["report"]]

  # we will use regular expressions to match on prefix and suffix
  regex_match  <- paste0(
    "^", paste(prefix, collapse = "|"), # starts with prefix
    ".*",
    paste(suffix, collapse = "|"), "$"  # ends with suffix
  )
  bad_rows <- which(!grepl(regex_match, data[[target_columns]]))

  # the above also detects empty rows in the subject IDs column
  # we will substract missing IDs from the bad IDs (IDs with wrong prefix and
  # suffix)
  num_missing_ids <- 0
  if ("idx_missing_ids" %in% names(report)) {
    missing_ids <- as.numeric(unlist(
      strsplit(report[["idx_missing_ids"]], ", ", fixed = TRUE)
    ))
    num_missing_ids <- length(missing_ids)
    bad_rows <- bad_rows[!bad_rows %in% missing_ids]
  }

  # the usage of regular expression to determine whether numbers belong to a
  # specified range is not trivial. we use an approach where we parse numbers
  # only.
  if (!is.null(range)) {
    numbers_in <- as.numeric(unlist(lapply(data[[target_columns]],
                                           readr::parse_number)))
    bad_rows <- c(
      bad_rows,
      which(!(numbers_in >= min(range) & numbers_in <= max(range)))
    )
  }

  # detect subject IDs where the number of characters is not as expected
  if (!is.null(nchar)) {
    bad_rows <- c(bad_rows, which(nchar(data[[target_columns]]) != nchar))
  }

  # when all subject ids comply with the expected format,
  # send a message that no incorrect subject ids was found
  if (is.null(report) && length(bad_rows) == 0) {
    cli::cli_alert_info(
      tr_("No incorrect subject id was detected.")
    )
    return(data)
  }

  # determine row indices with incorrect subject ids, and
  # report them
  bad_rows <- sort(unique(bad_rows))
  num_bad_rows <- length(bad_rows)
  if (num_bad_rows > 0) {
    tmp_report <- data.frame(
      idx = bad_rows,
      ids = data[[target_columns]][bad_rows]
    )
    report[["invalid_subject_ids"]] <- tmp_report
  }

  # add to the report
  data <- add_to_report(
    x = data,
    key = "incorrect_subject_id",
    value = report
  )
  # send message to the user
  num_duplicated_ids <- ifelse( # nolint: object_usage_linter
    "duplicated_ids" %in% names(report),
    nrow(report[["duplicated_ids"]]),
    0
  )

  # send message to the user
  num_missing_ids <- ifelse( # nolint: object_usage_linter
    "idx_missing_ids" %in% names(report),
    length(report[["idx_missing_ids"]]),
    0
  )
  num_duplicated_ids <- ifelse( # nolint: object_usage_linter
    "duplicated_ids" %in% names(report),
    nrow(report[["duplicated_ids"]]),
    0
  )
  cli::cli_inform(c(
    "!" = tr_("Detected {.val {cli::no({num_missing_ids})}} missing, {.val {cli::no({num_duplicated_ids})}} duplicated, and {.val {cli::no({nrow(report$invalid_subject_ids) - num_missing_ids})}} incorrect subject IDs."), # nolint: line_length_linter
    "i" = tr_("Enter {.code print_report(data = dat, \"incorrect_subject_id\")} to access them, where {.val dat} is the object used to store the output from this operation."), # nolint: line_length_linter
    "i" = tr_("You can use the {.fn correct_subject_ids} function to correct {cli::qty(length(bad_rows))} {?it/them}.") # nolint: line_length_linter
  ))

  return(data)
}

#' Checks the uniqueness in values of the sample IDs column
#'
#' @inheritParams check_subject_ids
#' @param id_col_name A \code{<character>} with the name of the column that
#'    contains the sample IDs
#'
#' @returns the input \code{<data.frame>} with and extra element in its
#'    attributes when there are missing or duplicated IDs.
#' @keywords internal
#'
check_subject_ids_oness <- function(data, id_col_name) {
  report <- NULL
  # check for missing values in ID column
  if (anyNA(data[[id_col_name]])) {
    idx <- which(is.na(data[[id_col_name]]))
    report[["idx_missing_ids"]] <- toString(idx)
  }

  # check for duplicates ID column
  duplicated_ids <- suppressMessages(find_duplicates(data, id_col_name))
  tmp_report <- attr(duplicated_ids, "report")[["found_duplicates"]]
  if (!is.null(tmp_report) &&
      "duplicated_rows" %in% names(tmp_report) &&
      nrow(tmp_report[["duplicated_rows"]]) > 0L) {
    num_dup_rows <- nrow(tmp_report[["duplicated_rows"]]) # nolint: object_usage_linter
    dups <- tmp_report[["duplicated_rows"]]
    report[["duplicated_ids"]] <- dups
  }

  return(list(
    data = data,
    report = report
  ))
}


#' Correct the wrong subject IDs based on the user-provided values.
#'
#' After detecting incorrect subject IDs from the \code{check_subject_ids()}
#' function, use this function to provide the correct IDs and perform the
#' substitution.
#'
#' @inheritParams check_subject_ids
#' @param correction_table A \code{<data.frame>} with the following two columns:
#'  \describe{
#'    \item{from}{a column with the wrong subject IDs}
#'    \item{to}{a column with the values to be used to substitute the
#'        incorrect ids.}
#'  }
#'
#' @returns The input dataset where all subject ids comply with the expected
#'    format.
#' @export
#'
#' @examples
#' data <- readRDS(
#'   system.file("extdata", "test_df.RDS", package = "cleanepi")
#' )
#' # detect the incorrect subject ids i.e. IDs that do not have any or both of
#' # the followings:
#' # - starts with 'PS',
#' # - ends with 'P2',
#' # - has a number within 1 and 100,
#' # - contains 7 characters.
#' dat <- check_subject_ids(
#'   data = data,
#'   target_columns = "study_id",
#'   prefix = "PS",
#'   suffix = "P2",
#'   range = c(1, 100),
#'   nchar = 7
#' )
#'
#' # display rows with invalid subject ids
#' print_report(dat, "incorrect_subject_id")
#'
#' # generate the correction table
#' correction_table <- data.frame(
#'   from = c("P0005P2", "PB500P2", "PS004P2-1"),
#'   to = c("PB005P2", "PB050P2", "PS004P2")
#' )
#'
#' # perform the correction
#' dat <- correct_subject_ids(
#'   data = dat,
#'   target_columns = "study_id",
#'   correction_table = correction_table
#' )
correct_subject_ids <- function(data, target_columns, correction_table) {
  checkmate::assert_data_frame(correction_table, any.missing = FALSE,
                               min.rows = 1L, ncols = 2L, null.ok = FALSE,
                               col.names = "named")
  checkmate::assert_names(names(correction_table),
                          identical.to = c("from", "to"))
  if (!all(correction_table[["from"]] %in% data[[target_columns]])) {
    cli::cli_abort(c(
      tr_("Some IDs specified in the correction table were not found in the input data."), # nolint: line_length_linter
      i = tr_("Values in the {.field from} column of the correction table must be part of the detected incorrect subject IDs.") # nolint: line_length_linter
    ))
  }

  # perform the substitution
  idx <- match(correction_table[["from"]], data[[target_columns]])
  data[[target_columns]][idx] <- correction_table[["to"]]

  # check whether substitution did not introduce any duplicate
  data <- check_subject_ids_oness(data, target_columns)[["data"]]

  return(data)
}
