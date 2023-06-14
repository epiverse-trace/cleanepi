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
#'   \item `remove_duplicates`: whether to detect duplicated records or not.
#'         default is TRUE
#'   \item `duplicates_from`: a vector of columns names to use when looking for
#'         duplicates. When the input data is a `linelist` object, this
#'         parameter can be set to `tags` if you wish to look for duplicates on
#'         tagged variables. Only used when `remove_duplicates=TRUE`
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
#' today's date and the same date 50 years before
#'
#' @examples
#' cleaned_data <- clean_data(
#' data = data.table::fread(system.file("extdata", "test.txt",
#' package = "cleanepi")),
#' params = list(
#'   remove_duplicates = TRUE,
#'   duplicates_from = NULL,
#'   replace_missing = TRUE,
#'   na_comes_as = "-99",
#'   check_timeframe = TRUE,
#'   timeframe = as.Date(c("1973-05-29", "2023-05-29")),
#'   error_tolerance = 0.1,
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
                                     duplicates_from = NULL,
                                     replace_missing = FALSE,
                                     na_comes_as = NULL,
                                     check_timeframe = TRUE,
                                     timeframe = NULL,
                                     error_tolerance = 0.1,
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
  data <- clean_col_names(data)

  # replace missing values with NA
  if (params$replace_missing) {
    for (cols in names(data)) {
      data <- replace_missing_char(data, cols, params$na_comes_as)
    }
  }

  # remove empty records and columns
  R.utils::cat("\nremoving empty rows and columns")
  dat <- data %>%
    janitor::remove_empty(c("rows", "cols"))
  report <- report_cleaning(data, dat, state = "remove_empty", report = report)

  # remove constant columns
  data <- dat
  R.utils::cat("\nremoving constant columns")
  dat <- data %>% janitor::remove_constant()
  report <- report_cleaning(data, dat, state = "remove_constant",
                           report = report)

  # remove duplicated records
  data <- dat
  if (params$remove_duplicates) {
    R.utils::cat("\nremoving duplicated rows")
    if (!is.null(params$duplicates_from)) {
      dat <- remove_duplicates(data, params$duplicates_from)
    } else {
      dat <- data %>% dplyr::distinct()
    }
  }
  report <- report_cleaning(data, dat, state = "remove_dupliates",
                           report = report)

  # standardize date columns
  R.utils::cat("\nstandardising date columns")
  data <- dat
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
  R.utils::cat("\nchecking subject IDs format")
  if (!is.null(params$subject_id_format)) {
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
  report$params <- as.data.frame(do.call(rbind, params)) %>%
    dplyr::rename("value1" = "V1", "value2" = "V2")

  # return the final object
  list(
    data = data,
    report = report
    )
}
