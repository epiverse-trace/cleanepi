#' Perform dictionary-based cleaning
#'
#' @param data The input \code{<data.frame>} or \code{<linelist>}
#' @param dictionary A \code{<data.frame>} with the dictionary associated with
#'    the input data. This is expected to be compatible with the
#'    \pkg{matchmaker} package and must contain the following four columns:
#'    \describe{
#'      \item{`options`}{This column contains the current values used to
#'          represent the different groups in the input data frame (required).}
#'      \item{`values`}{The values that will be used to replace the current
#'          options (required).}
#'      \item{`grp`}{The name of the columns where every option belongs to
#'          (required).}
#'      \item{`orders`}{This defines the user-defined order of different options
#'          (optional).}
#'    }
#'
#' @returns A \code{<data.frame>} or \code{<linelist>} where the target options
#'    have been replaced with their corresponding values in the columns
#'    specified in the data dictionary.
#' @export
#'
#' @examples
#' data <- readRDS(
#'   system.file("extdata", "messy_data.RDS", package = "cleanepi")
#' )
#' dictionary <- readRDS(
#'   system.file("extdata", "test_dict.RDS", package = "cleanepi")
#' )
#'
#' # adding an option that is not defined in the dictionary to the 'gender'
#' # column
#' data$gender[2] <- "homme"
#' cleaned_df <- clean_using_dictionary(
#'   data = data,
#'   dictionary = dictionary
#' )
#'
#' # print the report
#' print_report(cleaned_df, "misspelled_values")
clean_using_dictionary <- function(data, dictionary) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_data_frame(dictionary, min.rows = 1L, max.cols = 4L,
                               null.ok = FALSE)
  # abort if the provided data dictionary does not contain the following three
  # column names: options, values, grp.
  if (!all(c("options", "values", "grp") %in% names(dictionary))) {
    mandatory_columns <- c("options", "values", "grp") # nolint: object_usage_linter
    all_columns <- c(mandatory_columns, "orders") # nolint: object_usage_linter
    cli::cli_abort(c(
      tr_("Incorrect data dictionary format."),
      "*" = tr_("The value for the {.emph dictionary} argument must be a {.cls data.frame} with the following columns: {.field {toString(all_columns)}}."), # nolint: line_length_linter
      "*" = tr_("The following columns are mandatory: {.field {toString(mandatory_columns)}}.") # nolint: line_length_linter
    ))
  }

  # abort if the specified column names in the 'grp' column are not found in the
  # input data
  if (!all(unique(dictionary[["grp"]]) %in% names(data))) {
    cli::cli_abort(c(
      tr_("Incorrect column names specified in column {.field grp} of the data dictionary."), # nolint: line_length_linter
      x = tr_("Values in {.field grp} column of the data dictionary must be found in the input data frame."), # nolint: line_length_linter
      i = tr_("Did you enter an incorrect column name?")
    ))
  }

  # detect misspelled options in the columns to clean
  misspelled_options <- detect_misspelled_options(data, dictionary)

  # correct the misspelled options if they exist
  if (length(misspelled_options) > 0L) {
    print_misspelled_values(data, misspelled_options)
    cli::cli_inform(c(
      i = tr_("You can either: "),
      "*" = tr_("correct the misspelled {cli::qty(length(misspelled_options))} option{?s} from the input data, or"), # nolint: line_length_linter
      "*" = tr_("add {cli::qty(length(misspelled_options))} {?it/them} to the dictionary using the {.fn add_to_dictionary} function.") # nolint: line_length_linter
    ))
    misspelled_report <- construct_misspelled_report(misspelled_options, data)
    # add the result to the reporting object
    data <- add_to_report(
      x = data,
      key = "misspelled_values",
     value = misspelled_report
    )
  }
  # perform the dictionary-based cleaning
  data <- suppressWarnings(
    matchmaker::match_df(
      data,
      dictionary = dictionary,
      from = "options",
      to = "values",
      by = "grp")
  )

  return(data)
}


#' Build the report for the detected misspelled values during dictionary-based
#' data cleaning operation
#'
#' @param misspelled_options A \code{<list>} with the detected misspelled values
#'    in the columns of interest.
#' @inheritParams clean_using_dictionary
#'
#' @returns A \code{<data.frame>} the details about where in the input data the
#'    misspelled values were found.
#' @keywords internal
#'
construct_misspelled_report <- function(misspelled_options, data) {
  checkmate::assert_list(misspelled_options, null.ok = FALSE)
  result <- NULL
  for (opts in names(misspelled_options)) {
    res <- data.frame(
      idx = misspelled_options[[opts]],
      column = rep(opts, length(misspelled_options[[opts]])),
      value = data[[opts]][misspelled_options[[opts]]]
    )
    result <- c(result, list(res))
  }
  result <- dplyr::bind_rows(result)
  return(result)
}

#' Make data dictionary for 1 field
#'
#' @param x A \code{<vector>} of choices from the input data
#' @param field_column A \code{<character>} with the name of the field where the
#'    choices are made.
#'
#' @returns A \code{<data.frame>} with the dictionary in the format that is
#'    accepted by the \pkg{matchmaker} package.
#' @keywords internal
#'
dictionary_make_metadata <- function(x, field_column) {
  splits <- trimws(unlist(strsplit(x, "|", fixed = TRUE)))
  combined_splits <- lapply(splits, function(x) {
    return(trimws(unlist(strsplit(x, ",", fixed = TRUE))))
  })
  combined_splits <- do.call(rbind.data.frame, combined_splits)
  res <- data.frame(
    options = combined_splits[, 1L],
    values = combined_splits[, 2L],
    grp = rep(field_column, nrow(combined_splits)),
    orders = seq_len(nrow(combined_splits))
  )
  rownames(res) <- NULL
  return(res)
}

#' Add an element to the data dictionary
#'
#' @inheritParams clean_using_dictionary
#' @param option A \code{<vector>} of characters with the new options that need
#'    to be added to the dictionary.
#' @param value A \code{<vector>} of characters with the values to be used when
#'    replacing the new options.
#' @param grp A \code{<vector>} of characters with the name of the column that
#'    contains the option of interest.
#' @param order A \code{<vector>} of numeric values with the order of the new
#'    option.
#'
#' @returns A \code{<data.frame>}. This is the new data dictionary with
#'    an additional line that contains the details about the new options.
#' @export
#'
#' @examples
#' test <- add_to_dictionary(
#'   dictionary = readRDS(
#'     system.file("extdata", "test_dict.RDS", package = "cleanepi")
#'   ),
#'   option = "ml",
#'   value = "male",
#'   grp = "gender",
#'   order = NULL
#'  )
add_to_dictionary <- function(dictionary,
                              option,
                              value,
                              grp,
                              order = NULL) {
  checkmate::assert_vector(option, min.len = 1L, null.ok = FALSE,
                           any.missing = FALSE)
  checkmate::assert_vector(value, min.len = 1L, null.ok = FALSE,
                           any.missing = FALSE)
  checkmate::assert_vector(grp, min.len = 1L, null.ok = FALSE,
                           any.missing = FALSE)
  checkmate::assert_numeric(order, any.missing = TRUE, lower = 1L,
                            null.ok = TRUE)

  # select the lines in the data dictionary where the column name is the same as
  # the value of the grp argument
  max_order <- max(dictionary[["orders"]])
  tmp_dictionary <- dictionary %>%
    dplyr::filter(.data$grp == grp)

  dictionary <- dictionary %>%
    dplyr::filter(.data$grp != grp)

  # make the new order
  new_order <- order
  if (is.null(order)) {
    new_order <- max_order + seq_along(option)
  }

  # make the data frame with the new option
  new_option <- data.frame(
    options = option,
    values = value,
    grp = grp,
    orders = new_order
  )
  tmp_dictionary <- rbind(tmp_dictionary, new_option)
  dictionary <- rbind(dictionary, tmp_dictionary)
  return(dictionary)
}

#' Detect misspelled options in columns to be cleaned
#'
#' @inheritParams clean_using_dictionary
#'
#' @returns A \code{<list>} with the indexes of the misspelled values in every
#'    column that needs to be cleaned.
#' @keywords internal
#'
detect_misspelled_options <- function(data, dictionary) {
  cols_to_modify <- unique(dictionary[["grp"]])
  outliers <- list()
  for (col in cols_to_modify) {
    unique_values <- unique(data[[col]])[!is.na(unique(data[[col]]))]
    temp_dict <- dictionary %>%
      dplyr::filter(.data$grp == col)
    opts <- c(temp_dict[["options"]], unique(temp_dict[["values"]]))
    m <- match(unique_values, opts)
    if (anyNA(m)) {
      outliers[[col]] <- which(data[[col]] == unique_values[is.na(m)])
    }
  }
  return(outliers)
}


#' Print the detected misspelled values
#'
#' @inheritParams clean_using_dictionary
#' @param misspelled_options A \code{<list>} with the misspelled values found in
#'    the different columns of the input data.
#'
#' @returns Prints out the misspelled values from the column of interest
#' @keywords internal
#'
print_misspelled_values <- function(data, misspelled_options) {
  for (option in names(misspelled_options)) { # nolint: return_linter
    undefined_opts <- data[[option]][[misspelled_options[[option]]]] # nolint: object_usage_linter
    cli::cli_alert_warning(
      tr_("Cannot replace {.val {toString(undefined_opts)}} present in column {.field {option}} but not defined in the dictionary.") # nolint: line_length_linter
    )
  }
}
