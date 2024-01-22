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
#'         dates fall under the given time frame of not. default: FALSE
#'   \item `timeframe`: a vector of 2 elements of Date class that specifies the
#'         first and last date. If provided, all Dates in the data frame must be
#'         within this range or set to NA during the cleaning.
#'   \item `error_tolerance`: a number between 0 and 1 indicating the proportion
#'         of entries which cannot be identified as dates to be tolerated; if
#'         this proportion is exceeded, the original vector is returned, and a
#'         message is issued; defaults to 0.1 (10 percent)
#'   \item `subject_id_col_name`: the name of the column in the data frame with
#'          the subject IDs
#'   \item `subject_id_format`: the expected subject format
#'   \item `prefix`: the prefix used in the subject IDs
#'   \item `suffix`: the prefix used in the subject IDs
#'   \item `range`: a vector with the range of numbers in the subject IDs
#'   \item `dictionary`: an object of type data frame. This is the data
#'         dictionary that will be used to clean the specified columns. Use
#'         `?clean_using_dictionary` for more details.
#'   \item `range`: a vector with the range of numbers in the sample IDs
#'   \item `keep`: a vector of column names to be kept as they appear
#'          in the original data. default is `NULL`
#'   }
#'
#' @return a list of the following 2 elements:
#'  \enumerate{
#'    \item `data`: the cleaned data frame according to the user-specified
#'          parameters
#'    \item `report`: an object of type list with the details from every
#'          cleaning operation.
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
#'   data   = readRDS(system.file("extdata", "test_df.RDS",
#'                                package = "cleanepi")),
#'   params = list(
#'     keep                = NULL,
#'     remove_duplicates   = TRUE,
#'     target_columns      = NULL,
#'     replace_missing     = TRUE,
#'     na_comes_as         = "-99",
#'     check_timeframe     = TRUE,
#'     timeframe           = as.Date(c("1973-05-29", "2023-05-29")),
#'     error_tolerance     = 0.5,
#'     subject_id_col_name = "study_id",
#'     subject_id_format   = "PS000P2",
#'     prefix              = "PS",
#'     suffix              = "P2",
#'     range               = c(1, 100),
#'     dictionary          = NULL))
#'
clean_data <- function(data,
                       params = list(
                         keep                   = NULL,
                         replace_missing_values = list(from        = NULL,
                                                       na_comes_as = NULL),
                         remove_duplicates   = list(target_columns    = NULL,
                                                    rm_empty_rows    = "all",
                                                    rm_empty_cols    = "all",
                                                    rm_constant_cols = TRUE),
                         standardize_date = list(target_columns   = NULL,
                                                 error_tolerance = 0.5,
                                                 format          = NULL,
                                                 timeframe       = NULL),
                         standardize_subject_ids = list(id_col_name = "id",
                                                        format      = NULL,
                                                        prefix      = NULL,
                                                        suffix      = NULL,
                                                        range       = NULL),
                         dictionary          = NULL,
                         to_numeric = NULL)
                       ) {
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  checkmate::assert_list(params, min.len = 0L, null.ok = TRUE)

  ## -----
  ## | we choose to use snake_cases for both variable and column names
  ## | are cleaned based using {base r} and {epitrix} packages.
  ## | Column names in 'keep' will not be modified.
  ## -----
  R.utils::cat("\ncleaning column names")
  data <- standardize_column_names(x = data, keep = params[["keep"]])

  ## -----
  ## | we choose to standardize on the value for the missing data.
  ## | missing values will be replaced with NA
  ## | missing values from the original data can be provided by the
  ## | user if know, or inferred internally otherwise.
  ## -----
  if (!is.null(params[["replace_missing_values"]])) {
    R.utils::cat("\nreplacing missing values with NA")
    data <- replace_missing_values(
      data,
      from        = params[["replace_missing_values"]][["from"]],
      na_comes_as = params[["replace_missing_values"]][["na_comes_as"]]
    )
  }

  ## -----
  ## | The existence of duplicated records can be genuine. But duplication is
  ## | generally introduced by mistake. We looks for and remove duplicates to
  ## | minimise potential issues during data analysis. When no column is
  ## | provided, duplicates are identified across all column. Otherwise, the
  ## | duplicates will only be considered from the specified columns.
  ## |
  ## | Empty rows and columns will also be removed. So will the constant columns
  ## | be.
  ## -----
  R.utils::cat("\nremoving duplicated rows")
  if (!is.null(params[["remove_duplicates"]])) {
    data <- remove_duplicates(
      data,
      target_columns   = params[["remove_duplicates"]][["target_columns"]],
      remove           = params[["remove_duplicates"]][["remove"]],
      rm_empty_rows    = params[["remove_duplicates"]][["rm_empty_rows"]],
      rm_empty_cols    = params[["remove_duplicates"]][["rm_empty_cols"]],
      rm_constant_cols = params[["remove_duplicates"]][["rm_constant_cols"]]
    )
  }

  ## -----
  ## | Date columns are expected in 'year-month-day' format. The will will
  ## | detect and convert columns with Date values. This conversion will make it
  ## | easy to apply the functions that operate on variables of type Date.
  ## -----
  R.utils::cat("\nstandardising date columns")
  if (!("error_tolerance" %in% names(params))) {
    params[["error_tolerance"]] <- 0.5
  }
  dat <- standardize_date(
    data             = data,
    date_column_name = NULL,
    format           = NULL,
    timeframe        = params[["timeframe"]],
    check_timeframe  = params[["check_timeframe"]],
    report,
    error_tolerance  = params[["error_tolerance"]]
  )
  report <- dat[["report"]]
  report <- report_cleaning(data, dat[["data"]],
                            state  = "standardize_date",
                            report = report)
  data <- dat[[1L]]

  ## -----
  ## | The uniqueness of the IDs is checked here to ensure that there is no
  ## | redundant subject ID.
  ## -----
  stopifnot("'subject_id_col_name' must be provided in the list of cleaning
            parameters." = !is.null(params[["subject_id_col_name"]]))
  R.utils::cat("\nchecking for subject IDs uniqueness")
  dat <- check_ids_uniqueness(
    data        = data,
    id_col_name = params[["subject_id_col_name"]],
    report      = report
  )
  report <- dat[["report"]]
  data   <- dat[["data"]]

  ## -----
  ## | We check how the format of the subject IDs complies with the expected
  ## | format to detect typos or incorrect entries. This will result in a tidy
  ## | subject ID column where all values are in the correct format.
  ## -----
  if (!is.null(params[["subject_id_format"]])) {
    R.utils::cat("\nchecking subject IDs format")
    tmp_res <- check_subject_ids(
      data           = data,
      format         = params[["subject_id_format"]],
      id_column_name = params[["subject_id_col_name"]],
      prefix         = params[["prefix"]],
      suffix         = params[["suffix"]],
      range          = params[["range"]],
      remove         = TRUE,
      verbose        = FALSE,
      report         = report
    )
    data   <- tmp_res[[1L]]
    report <- tmp_res[[2L]]
  }

  ## -----
  ## | knowing the composition of every column of the data will help in deciding
  ## | about what actions can be taken for a specific column.
  ## | Note that any modification made to a column will be reported in the
  ## | report object.
  ## -----
  scan_result <- scan_data(data = data)

  ## -----
  ## | We convert the few character values into numeric when they are found in a
  ## | numeric column. This ensures that the values in a numeric column are
  ## | homogeneous.
  ## -----
  if (!("to_numeric" %in% names(params))) {
    params[["to_numeric"]] <- NULL
  }
  tmp_res <- convert_to_numeric(data       = data,
                                report     = report,
                                to_numeric = params[["to_numeric"]],
                                scan_res   = scan_result)
  data    <- tmp_res[["data"]]
  report  <- tmp_res[["report"]]

  ## -----
  ## The values in some columns are coded and their correspondent expressions
  ## will be stored in a data dictionary file. We implement this function to
  ## replace these coded values with the exact values from the data dictionary.
  ## We also account for the
  ## -----
  if (!is.null(params[["dictionary"]])) {
    data <- clean_using_dictionary(data, params[["dictionary"]])
  }


  # this is where to call the reporting function
  report[["params"]] <- params

  # return the final object
  list(
    data   = data,
    report = report
  )
}
