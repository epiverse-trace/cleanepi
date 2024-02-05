#' Clean data
#'
#' @description Cleans up messy data frames by performing several operations. These Include
#'    cleaning of column names, detecting and removing
#'    duplicates, empty records and columns, constant columns, replacing missing
#'    values by NA, converting character columns into dates when they contain a
#'    certain number of date values, and detecting subject IDs with wrong format
#'
<<<<<<< HEAD
#' @param data A data frame
#' @param params A list of parameters that define what cleaning operations will
#'    be applied on the input data. Possible parameters are:
#' \enumerate{
#'   \item `remove_duplicates`: A logical variable to indicate whether to remove duplicated records or not. If
#'        set `TRUE`, it calls the `remove_duplicate()` function
#'        with parameter `remove` set to `NULL` i.e., to keep only the first instance
#'        of duplicated rows. If you only want to detect duplicated rows in the dataset, use
#'        the `find_duplicates()` function.
#'   \item `target_columns`: A vector of columns names or indices to consider
#'        when looking for duplicates. this
#'        parameter can be set to `tags`from which duplicates to be removed.
#'   \item `replace_missing`: A logical variable that indicates whether to replace missing value characters
#'          with NA or not. The default value is `FALSE`.
#'   \item `na_comes_as`: A string that represents the missing values in
#'         the data frame. This only required when `replace_missing=TRUE`.
#'   \item `check_timeframe`: A logical variable that determines whether to check if the
#'         dates fall under the given time frame of not. default: `FALSE`.
#'   \item `timeframe`: A vector of 2 dates that specifies the
#'         first and last date. If provided, all Dates in the data frame must be
#'         within this range or set to NA during the cleaning.
#'   \item `error_tolerance`: A number between 0 and 1 indicating the proportion
#'         of entries which cannot be identified as dates to be tolerated; if
#'         this proportion is exceeded, the original vector is returned, and a
#'         message is issued; defaults to 0.1 (10 percent).
#'   \item `subject_id_col_name`: A column name of subject IDs
#'   \item `subject_id_format`: A expected subject format
#'   \item `prefix`: A prefix used in the subject IDs.
#'   \item `suffix`: A suffix used in the subject IDs.
#'   \item `range`: A vector with the range of numbers in the subject IDs.
#'   \item `dictionary`: A data frame of data
#'         dictionary that will be used to clean the specified columns. Use
#'         `?clean_using_dictionary` for more details.
#'   \item `range`: A vector with the range of numbers in the sample IDs
#'   \item `keep`: A vector of column names to be kept as they appear
#'          in the original data. default is `NULL`.
#'   \item `standardize_date`: A list of parameters that will be used to
=======
#' @param data the input data frame
#' @param params a list of parameters that define what cleaning operations will
#'    be applied on the input data. Possible values are:
#' \enumerate{
#'   \item `keep`: a vector of column names to be kept as they appear in the
#'      original data. All column names will be standardized if this is `NULL`
#'      (the default value).
#'   \item `replace_missing_values`: a list of parameters to be used when
#'      replacing the missing values by `NA`. These parameters are the inputs
#'      for the `replace_missing_values()` function.
#'   \item `remove_duplicates`: a list of arguments that defines the list of
#'      columns to be considered when looking for duplicates. It also contains
#'      arguments that determine how constant rows and columns will be handled.
#'      They are the input values for the `remove_duplicates()` function.
#'   \item `standardize_date`: a list of parameters that will be used to
>>>>>>> 018c12509ed66ff094a405cc4c4b5fe6666d6188
#'      standardize the date values from the input data. They represent the
#'      input values for the `standardize_dates()` function.
#'   \item `standardize_subject_ids`: a list of parameters that are needed to
#'      check the IDs that comply with the expect format. These arguments are
#'      the input values of the `check_subject_ids()`.
#'   \item `to_numeric`: a vector of column names. When provided, the values in
#'      these columns will be converted into numeric.
#'   \item `dictionary`: an data frame that will be used to substitute the
#'      current values in the specified columns the those in the dictionary. It
#'      is the main argument for the `clean_using_dictionary()` function.
#'   }
#'
<<<<<<< HEAD
#' @return a list of the following 2 elements:
#'  \enumerate{
#'    \item `data`: A  clean data frame  according to the user-specified
#'          parameters.
#'    \item `report`: A list with  details from each
#'          cleaning operation considered.
#'  }
#'
=======
#' @return the cleaned data frame according to the user-specified parameters
>>>>>>> 018c12509ed66ff094a405cc4c4b5fe6666d6188
#' @export
#'
#' @examples
#' Here `keep = NULL` i.e. column names standardization will be performed across
#' all columns
#'
#' Parameters for substituting missing values with NA:
#' replace_missing_values = list(target_columns = NULL, na_strings = "-99")
#'
#' Parameters for duplicates removal across all columns (target_columns = NULL)
#' remove_duplicates = list(target_columns   = NULL,
#'                          rm_empty_rows    = TRUE,
#'                          rm_empty_cols    = TRUE,
#'                          rm_constant_cols = TRUE)
#'
#' Parameters for dates standardization
#' standardize_date = list(target_columns  = NULL,
#'                         error_tolerance = 0.5,
#'                         format          = NULL,
#'                         timeframe       = as.Date(c("1973-05-29",
#'                                                     "2023-05-29")))
#'
#' Parameters for subject IDs standardization
#' standardize_subject_ids = list(id_col_name,
#'                                format      = NULL,
#'                                prefix      = "PS",
#'                                suffix      = "P2",
#'                                range       = c(1, 100))
#'
#' `to_numeric = "sex"`: the 'sex' column will be converted into numeric
#'
#' `dictionary = NULL`: the dictionary-based cleaning will not be performed here
#'
#' cleaned_data <- clean_data(
#'   data   = readRDS(system.file("extdata", "test_df.RDS",
#'                                package = "cleanepi")),
#'   params = list(
#'     keep                    = NULL,
#'     replace_missing_values  = replace_missing_values,
#'     remove_duplicates       = remove_duplicates,
#'     standardize_date        = standardize_date,
#'     standardize_subject_ids = standardize_subject_ids,
#'     to_numeric              = "sex"
#'     dictionary             = NULL
#'   )
#' )
#'
clean_data <- function(
    data,
    params = list(
      keep                   = NULL,
      replace_missing_values = list(target_columns = NULL,
                                    na_strings = cleanepi::common_na_strings),
      remove_duplicates      = list(target_columns   = NULL,
                                    rm_empty_rows    = TRUE,
                                    rm_empty_cols    = TRUE,
                                    rm_constant_cols = TRUE),
     standardize_date        = list(target_columns  = NULL,
                                    error_tolerance = 0.5,
                                    format          = NULL,
                                    timeframe       = NULL),
     standardize_subject_ids = list(id_col_name = "id",
                                    format      = NULL,
                                    prefix      = NULL,
                                    suffix      = NULL,
                                    range       = NULL),
     dictionary              = NULL,
     to_numeric              = NULL)
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
      target_columns = params[["replace_missing_values"]][["target_columns"]],
      na_strings     = params[["replace_missing_values"]][["na_strings"]]
    )
  }

  ## -----
  ## | The existence of duplicated records can be genuine. But duplication is
  ## | generally introduced by mistake. We looks for and remove duplicates to
  ## | minimize potential issues during data analysis. When no column is
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
  data <- standardize_dates(
    data            = data,
    target_columns  = params[["standardize_date"]][["target_columns"]],
    format          = params[["standardize_date"]][["format"]],
    timeframe       = params[["standardize_date"]][["timeframe"]],
    error_tolerance = params[["standardize_date"]][["error_tolerance"]]
  )

  ## -----
  ## | We check whether the format of the subject IDs complies with the expected
  ## | format to detect typos or incorrect entries. This will result in a tidy
  ## | subject ID column where all values are in the correct format.
  ## | The uniqueness of the IDs is also checked here to ensure that there is no
  ## | redundant subject ID.
  ## -----
  if (!is.null(params[["standardize_subject_ids"]])) {
    R.utils::cat("\nchecking subject IDs format")
    stopifnot("'id_col_name' must be provided." =
                !is.null(params[["standardize_subject_ids"]][["id_col_name"]]))
    data <- check_subject_ids(
      data           = data,
      format         = params[["standardize_subject_ids"]][["format"]],
      id_column_name = params[["standardize_subject_ids"]][["id_col_name"]],
      prefix         = params[["standardize_subject_ids"]][["prefix"]],
      suffix         = params[["standardize_subject_ids"]][["suffix"]],
      range          = params[["standardize_subject_ids"]][["range"]]
    )
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
  if (!is.null(params[["to_numeric"]])) {
    R.utils::cat("\nconverting",
                 paste(params[["to_numeric"]], collapse = ", "),
                 "into numeric")
    data <- convert_to_numeric(data       = data,
                               to_numeric = params[["to_numeric"]],
                               scan_res   = scan_result)
  }

  ## -----
  ## The values in some columns are coded and their correspondent expressions
  ## will be stored in a data dictionary file. We implement this function to
  ## replace these coded values with the exact values from the data dictionary.
  ## We also account for the
  ## -----
  if (!is.null(params[["dictionary"]])) {
    R.utils::cat("\nperforming dictionary-based cleaning")
    data <- clean_using_dictionary(data, params[["dictionary"]])
  }


  # this is where to call the report printing function
  print_report(report)

  # return the final object
  return(data)
}
