#' Clean data
#'
#' @description this function is used to clean up messy data frames. This
#'    include the cleaning of column names, detecting and removing of
#'    duplicates, empty records and columns, constant columns, replacing missing
#'    values by NA, converting character columns into Date when they contain a
#'    certain number of Date values, detecting subject IDs with wrong format
#'
#' @param data the input data frame
#' @param params a list of parameters that define what cleaning operations will
#'    be applied on the input data. Possible parameters are:
#' \enumerate{
#'   \item `remove_duplicates`: whether to remove duplicated records or not. If
#'        `TRUE`, the `remove` argument of the `remove_duplicate()` function
#'        will automatically be set to `-1` i.e. to keep only the first instance
#'        of duplicated rows.
#'        When the user only needs to detect duplicated rows in the dataset, use
#'        the `find_duplicates()` function.
#'   \item `target_columns`: a vector of columns names or indices to consider
#'        when looking for duplicates. When the input data is a `linelist`
#'        object, this parameter can be set to `tags` if you wish to look for
#'        duplicates across the tagged variables only. Only used when
#'        `remove_duplicates=TRUE`
#'   \item `replace_missing`: whether to replace the missing value characters
#'         with NA or not. default is FALSE
#'   \item `na_comes_as`: the characters that represent the missing values in
#'         the data frame. Only used when `replace_missing=TRUE`
#'   \item `check_timeframe`: a logical to determine whether to check if the
#'         dates fall under the given time frame of not. default: TRUE
#'   \item `timeframe`: a vector of 2 elements of Date class that specifies the
#'         first and last date. If provided, all Dates in the data frame must be
#'         within this range or set to NA during the cleaning.
#'   \item `error_tolerance`: a number between 0 and 1 indicating the proportion
#'         of entries which cannot be identified as dates to be tolerated; if
#'         this proportion is exceeded, the original vector is returned, and a
#'         message is issued; defaults to 0.1 (10 percent)
#'   \item `subject_id_col_name`: the name of the column in the data frame with
#'         the subject IDs
#'   \item `subject_id_format`: the expected subject format
#'   \item `prefix`: the prefix used in the subject IDs
#'   \item `suffix`: the prefix used in the subject IDs
#'   \item `range`: a vector with the range of numbers in the sample IDs
#'   }
#'
#' @return a list of the following 2 elements:
#'  \enumerate{
#'    \item `data`: the cleaned data frame according to the user-specified
#'          parameters
#'    \item `report`: a list with the information about the effects of the
#'          the cleaning steps
#'  }
#' @export
#'
#' @details
#' If `check_timeframe = TRUE` and `timeframe = NULL`, the timeframe will be
#' today's date and the same date 50 years before.
#'
#' in `clean_data()`, duplicated rows will be identified across the
#' user-specified or all columns. Once detected, all occurrences of the
#' duplicated rows will be removed except the first. If you only need to find
#' and remove specific duplicates, use the `find_duplicates()` then
#' `remove_duplicates()` functions.
#'
#' @examples
#' cleaned_data <- clean_data(
#' data = readRDS(system.file("extdata", "test_df.RDS", package = "cleanepi")),
#' params = list(
#'   remove_duplicates = TRUE,
#'   target_columns = NULL,
#'   replace_missing = TRUE,
#'   na_comes_as = "-99",
#'   check_timeframe = TRUE,
#'   timeframe = as.Date(c("1973-05-29", "2023-05-29")),
#'   error_tolerance = 0.5,
#'   subject_id_col_name = "study_id",
#'   subject_id_format = "PS000P2",
#'   prefix = "PS",
#'   suffix = "P2",
#'   range = c(1, 100)
#'   )
#' )
#'
clean_data <- function(data,
                       params = list(remove_duplicates = FALSE,
                                     target_columns = NULL,
                                     replace_missing = TRUE,
                                     na_comes_as = NULL,
                                     check_timeframe = TRUE,
                                     timeframe = NULL,
                                     error_tolerance = 0.5,
                                     subject_id_col_name = NULL,
                                     subject_id_format = NULL,
                                     prefix = "PS",
                                     suffix = "P2",
                                     range = c(1, 100)
                                    )
                       ) {
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1)
  checkmate::assert_list(params, min.len = 1, null.ok = TRUE)

  report <- list()
  # clean the column names based on janitor package
  R.utils::cat("\ncleaning column names")
  res <- clean_col_names(data, report)
  data <- res$data
  report <- res$report

  # replace missing values with NA
  if (params$replace_missing) {
    R.utils::cat("\nreplacing missing values with NA")
    for (cols in names(data)) {
      data <- replace_missing_char(data, cols, params$na_comes_as)
    }
  }

  # remove empty records and columns
  R.utils::cat("\nremoving empty rows and columns")
  dat <- data %>%
    janitor::remove_empty(c("rows", "cols"))
  report <- report_cleaning(data, dat, state = "remove_empty", report = report)
  data <- dat

  # remove constant columns
  R.utils::cat("\nremoving constant columns")
  dat <- data %>% janitor::remove_constant()
  report <- report_cleaning(data, dat, state = "remove_constant",
                           report = report)
  data <- dat

  # check for subject IDs uniqueness
  stopifnot("params$subject_id_col_name must be provided." =
              !is.null(params$subject_id_col_name))
  R.utils::cat("\nchecking for subject IDs uniqueness")
  report <- check_ids_uniqueness(
    data = data,
    id_col_name = params$subject_id_col_name,
    report = report
  )

  # remove duplicated records
  R.utils::cat("\nremoving duplicated rows")
  if (params$remove_duplicates) {
    dat <- remove_duplicates(data, params$target_columns,
                             remove = NULL, report)
    data <- dat$data
    report <- dat$report
  }


  # standardize date columns
  R.utils::cat("\nstandardising date columns")
  dat <- standardize_date(
    data = data,
    date_column_name = NULL,
    format = NULL,
    timeframe = params$timeframe,
    check_timeframe = params$check_timeframe,
    report,
    error_tolerance = params$error_tolerance
  )
  report <- dat$report
  report <- report_cleaning(data, dat$data, state = "standardize_date",
                           report = report)
  data <- dat[[1]]

  # check the subject IDs
  if (!is.null(params$subject_id_format)) {
    R.utils::cat("\nchecking subject IDs format")
    tmp_res <- check_subject_ids(data = data,
                             id_column_name = params$subject_id_col_name,
                             format = params$subject_id_format,
                             prefix = params$prefix, suffix = params$suffix,
                             range = params$range, remove = TRUE,
                             verbose = FALSE, report = report
                            )
    data <- tmp_res[[1]]
    report <- tmp_res[[2]]
  }

  # this is where to call the reporting function
  timeframe <- params$timeframe
  report$params <- as.data.frame(do.call(rbind, params)) %>%
    dplyr::rename("value1" = "V1", "value2" = "V2")
  report$params[which(rownames(report$params) == "timeframe"), ] <-
    as.character(timeframe)

  # return the final object
  list(
    data = data,
    report = report
    )
}
