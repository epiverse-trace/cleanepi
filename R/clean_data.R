#' Clean and standardize data
#'
#' @description Cleans up messy data frames by performing several operations.
#'    These include among others: cleaning of column names, detecting and
#'    removing duplicates, empty records and columns, constant columns,
#'    replacing missing values by NA, converting character columns into dates
#'    when they contain a certain number of date values, detecting subject
#'    IDs with wrong formats, etc.
#'
#' @param data The input data frame or linelist
#' @param ... A list of cleaning operations to be applied on the input data.
#'    The acceptable arguments for \code{...} are:
#' \describe{
#'   \item{`standardize_column_names`}{A list with the arguments needed to
#'      standardize the column names. The elements of this list are the
#'      input for the \code{\link{standardize_column_names}} function.}
#'   \item{`replace_missing_values`}{A list of parameters to be used when
#'      replacing the missing values by `NA`. The elements of the list are the
#'      inputs for the \code{\link{replace_missing_values}} function.}
#'   \item{`remove_duplicates`}{A list with the arguments that define the
#'      columns and other parameters to be considered when looking for
#'      duplicates. They are the input values for the
#'      \code{\link{remove_duplicates}} function.}
#'   \item{`remove_constants`}{A list with the parameters that define whether to
#'      remove constant data or not. The values are the input for the
#'      \code{\link{remove_constants}} function.}
#'   \item{`standardize_dates`}{A list of parameters that will be used to
#'      standardize the date values from the input data. They represent the
#'      input values for the \code{\link{standardize_dates}} function.}
#'   \item{`standardize_subject_ids`}{A list of parameters that are needed to
#'      check the IDs that comply with the expect format. These arguments are
#'      the input values of the \code{\link{check_subject_ids}}.}
#'   \item{`to_numeric`}{A list with the parameters needed to convert the
#'      specified columns into numeric. When provided, the parameters will be
#'      the input values for the \code{\link{convert_to_numeric}}.}
#'   \item{`dictionary`}{A data frame that will be used to substitute the
#'      current values in the specified columns the those in the dictionary. It
#'      is the main argument for the \code{\link{clean_using_dictionary}}
#'      function.}
#'   \item{`check_date_sequence`}{A list of arguments to be used when
#'      determining whether the sequence of date events is respected across all
#'      rows of the input data. The value in this list are the input for the
#'      \code{\link{check_date_sequence}} function.}
#'   }
#'
#' @returns The cleaned input date according to the user-specified parameters.
#'    This is associated with a data cleaning report that can be accessed using
#'    \code{attr(cleaned_data, "report")}
#'
#' @export
#'
#' @examples
#' # Parameters for column names standardization
#' standardize_column_names <- list(keep = NULL, rename = NULL)
#'
#' # parameters to remove constant columns, empty rows and columns
#' remove_constants <- list(cutoff = 1)
#'
#' # Parameters for substituting missing values with NA:
#' replace_missing_values <- list(target_columns = NULL, na_strings = "-99")
#'
#' # Parameters for duplicates removal across all columns
#' remove_duplicates <- list(target_columns   = NULL)
#'
#' # Parameters for dates standardization
#' standardize_dates <- list(
#'   target_columns = NULL,
#'   error_tolerance = 0.4,
#'   format = NULL,
#'   timeframe = as.Date(c("1973-05-29", "2023-05-29")),
#'   orders = list(
#'     world_named_months = c("Ybd", "dby"),
#'     world_digit_months = c("dmy", "Ymd"),
#'     US_formats = c("Omdy", "YOmd")
#'   )
#' )
#'
#' # Parameters for subject IDs standardization
#' standardize_subject_ids <- list(
#'   target_columns = "study_id",
#'   prefix = "PS",
#'   suffix = "P2",
#'   range = c(1, 100),
#'   nchar = 7
#' )
#'
#' # convert the 'sex' column into numeric
#' to_numeric <- list(target_columns = "sex", lang = "en")
#'
#' # the dictionary-based cleaning will not be performed here
#' dictionary = NULL
#'
#' # no need to check for the sequence of date events
#' check_date_sequence <- NULL
#'
#' cleaned_data <- clean_data(
#'   data = readRDS(
#'     system.file("extdata", "test_df.RDS", package = "cleanepi")
#'   ),
#'   standardize_column_names = standardize_column_names,
#'   remove_constants = remove_constants,
#'   replace_missing_values = replace_missing_values,
#'   remove_duplicates = remove_duplicates,
#'   standardize_dates = standardize_dates,
#'   standardize_subject_ids = standardize_subject_ids,
#'   to_numeric = to_numeric,
#'   dictionary = NULL,
#'   check_date_sequence = NULL
#' )
#'
clean_data <- function(data, ...) {
  checkmate::assert_data_frame(data, null.ok = FALSE, min.cols = 1L)
  # get the data cleaning operations defined by the user
  params <- list(...)

  # get the default parameters
  default_params <- get_default_params()

  # modify the default parameters with the user-provided parameters
  params <- modify_default_params(default_params, params, FALSE)

  # sanity check the list of parameters
  checkmate::assert_list(params, min.len = 1L, max.len = 10L, null.ok = TRUE)
  checkmate::check_names(
    params,
    subset.of = c("standardize_column_names", "remove_constants",
                  "replace_missing_values", "remove_duplicates",
                  "standardize_dates", "standardize_subject_ids",
                  "to_numeric", "dictionary", "check_date_sequence")
  )

  ## -----
  ## | we choose to use snake_cases for both variable and column names
  ## | are cleaned based using {base r} and {epitrix} packages.
  ## | Column names in 'keep' will not be modified.
  ## -----
  if (!is.null(params[["standardize_column_names"]])) {
    cli::cli_alert_info(
      tr_("\ncleaning column names")
    )
    data <- standardize_column_names(
      data   = data,
      keep   = params[["standardize_column_names"]][["keep"]],
      rename = params[["standardize_column_names"]][["rename"]]
    )
  }

  ## -----
  ## | we choose to standardize on the value for the missing data.
  ## | missing values will be replaced with NA.
  ## | Missing values from the original data can be provided by the
  ## | user if known, or inferred internally otherwise.
  ## -----
  if (!is.null(params[["replace_missing_values"]])) {
    cli::cli_alert_info(
      tr_("\nreplacing missing values with NA")
    )
    data <- replace_missing_values(
      data           = data,
      target_columns = params[["replace_missing_values"]][["target_columns"]],
      na_strings     = params[["replace_missing_values"]][["na_strings"]]
    )
  }

  ## -----
  ## | we can choose to remove the constant columns, the empty rows and columns
  ## -----
  if (!is.null(params[["remove_constants"]])) {
    cli::cli_alert_info(
      tr_("\nremoving the constant columns, empty rows and columns")
    )
    data <- remove_constants(
      data   = data,
      cutoff = params[["remove_constants"]][["cutoff"]]
    )
  }

  ## -----
  ## | The existence of duplicated records can be genuine. But duplication is
  ## | generally introduced by mistake. We looks for and remove duplicates to
  ## | minimize potential issues during data analysis. When no column is
  ## | provided, duplicates are identified across all column. Otherwise, the
  ## | duplicates will only be considered from the specified columns.
  ## -----
  if (!is.null(params[["remove_duplicates"]])) {
    cli::cli_alert_info(
      tr_("\nremoving duplicated rows")
    )
    data <- remove_duplicates(
      data,
      target_columns = params[["remove_duplicates"]][["target_columns"]]
    )
  }

  ## -----
  ## | Date columns are expected in 'year-month-day' format. The will will
  ## | detect and convert columns with Date values. This conversion will make it
  ## | easy to apply the functions that operate on variables of type Date.
  ## -----
  if (!is.null(params[["standardize_dates"]])) {
    cli::cli_alert_info(
      tr_("\nstandardizing Date columns")
    )
    data <- standardize_dates(
      data            = data,
      target_columns  = params[["standardize_dates"]][["target_columns"]],
      format          = params[["standardize_dates"]][["format"]],
      timeframe       = params[["standardize_dates"]][["timeframe"]],
      error_tolerance = params[["standardize_dates"]][["error_tolerance"]],
      orders          = params[["standardize_dates"]][["orders"]]
    )
  }

  ## -----
  ## | We check whether the format of the subject IDs complies with the expected
  ## | format to detect typos or incorrect entries. This will result in a tidy
  ## | subject ID column where all values are in the correct format.
  ## | The uniqueness of the IDs is also checked here to ensure that there is no
  ## | redundant subject ID.
  ## -----
  if (!is.null(params[["standardize_subject_ids"]])) {
    cli::cli_alert_info(
      tr_("\nchecking subject IDs format")
    )
    if (is.null(params[["standardize_subject_ids"]][["target_columns"]])) {
      cli::cli_abort(c(
        tr_("You did not specify a value for `target_columns`."),
        "*" = tr_("Here `target_columns` is the name of the column that unique identifies the individuals in your data."), # nolint: line_length_linter
        "*" = tr_("Type {.code ?check_subject_ids} to see the help on the corresponding function.") # nolint: line_length_linter
      ))
    }
    data <- check_subject_ids(
      data           = data,
      target_columns = params[["standardize_subject_ids"]][["target_columns"]],
      prefix         = params[["standardize_subject_ids"]][["prefix"]],
      suffix         = params[["standardize_subject_ids"]][["suffix"]],
      range          = params[["standardize_subject_ids"]][["range"]],
      nchar          = params[["standardize_subject_ids"]][["nchar"]]
    )
  }

  ## -----
  ## | We convert the few character values into numeric when they are found in a
  ## | numeric column. This ensures that the values in a numeric column are
  ## | homogeneous.
  ## -----
  if (!is.null(params[["to_numeric"]])) {
    target_columns <- params[["to_numeric"]][["target_columns"]]
    cli::cli_alert_info(
      tr_("\nconverting {.code {toString(target_columns)}} into numeric")
    )
    data <- convert_to_numeric(
      data           = data,
      target_columns = target_columns,
      lang           = params[["to_numeric"]][["lang"]]
    )
  }

  ## -----
  ## The values in some columns are coded and their correspondent expressions
  ## will be stored in a data dictionary file. We implement this function to
  ## replace these coded values with the exact values from the data dictionary.
  ## -----
  if (!is.null(params[["dictionary"]])) {
    cli::cli_alert_info(
      tr_("\nperforming dictionary-based cleaning")
    )
    data <- clean_using_dictionary(data, params[["dictionary"]])
  }

  ## -----
  ## The sequence of date events provided in the target column will be checked
  ## for conformity. When rows with incorrect date sequences are found, they
  ## will be flagged out.
  ## -----
  if (!is.null(params[["check_date_sequence"]])) {
    cli::cli_alert_info(
      tr_("\nchecking whether date sequences are respected")
    )
    data <- check_date_sequence(
      data           = data,
      target_columns = params[["check_date_sequence"]][["target_columns"]]
    )
  }

  # return the final object
  return(data)
}
