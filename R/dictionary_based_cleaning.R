#' Perform dictionary-based cleaning
#'
#' @param data A data frame
#' @param dictionary A data dictionary associated with the input data.
#' @returns A data frame with cleaned values in the target columns specified
#'    in the data dictionary.
#' @export
#'
#' @examples
#' data           <- readRDS(system.file("extdata", "messy_data.RDS",
#'                                       package = "cleanepi"))
#' data$gender[2] <- "homme"
#' cleaned_df     <- clean_using_dictionary(
#'   data       = data,
#'   dictionary = readRDS(system.file("extdata", "test_dict.RDS",
#'                                    package = "cleanepi"))
#' )
clean_using_dictionary <- function(data, dictionary) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_data_frame(dictionary, min.rows = 1L, max.cols = 4L,
                               null.ok = FALSE)
  stopifnot(all(c("options", "values", "grp") %in% names(dictionary)),
            all(unique(dictionary[["grp"]]) %in% names(data)))

  # detect misspelled options in the columns to clean
  misspelled_options <- detect_misspelled_options(data, dictionary)

  # correct the misspelled options if they exist
  if (length(misspelled_options) > 0L) {
    print_misspelled_values(misspelled_options)
    message("Please add the misspelled options to the data dictionary using",
            " the add_to_dictionary() function.")
    misspelled_report <- construct_misspelled_report(misspelled_options, data)
    # add the result to the reporting object
    data              <- add_to_report(x     = data,
                                       key   = "misspelled_values",
                                       value = misspelled_report)
  }
  # perform the dictionary-based cleaning
  data                <- suppressWarnings(
    matchmaker::match_df(data,
                         dictionary = dictionary,
                         from       = "options",
                         to         = "values",
                         by         = "grp")
  )

  return(data)
}


#' Build the report for the detected misspelled values during dictionary-based
#' data cleaning operation
#'
#' @param misspelled_options A list with the detected misspelled values in the
#'    columns of interest.
#' @inheritParams clean_using_dictionary
#'
#' @returns A data frame the details about where in the input data the
#'    misspelled values were found.
#' @keywords internal
#'
construct_misspelled_report <- function(misspelled_options, data) {
  checkmate::assert_list(misspelled_options, null.ok = FALSE)
  result  <- NULL
  for (opts in names(misspelled_options)) {
    res   <- data.frame(idx    = misspelled_options[[opts]],
                        column = rep(opts, length(misspelled_options[[opts]])),
                        value  = data[[opts]][misspelled_options[[opts]]])
    result <- rbind(result, res)
  }
  return(result)
}


#' Convert Redcap data dictionary into \{matchmaker\} dictionary format
#'
#' @param metadata A data frame with the data dictionary associated to a
#'    Redcap project
#' @param field_column The name of the column, in the input dictionary, that
#'    contains the field names in of the Redcap project data
#' @param opt_column The name of the column, in the input dictionary, that
#'    contains the definition of the choices in every column of the Redcap
#'    project data
#' @param field_type A `character` with the name of the column that contains the
#'    field type information
#'
#' @returns A data frame with 4 columns. This is in the format required by the
#'    \{matchmaker\} R package for dictionary-based cleaning.
#' @keywords internal
#'
make_readcap_dictionary <- function(metadata,
                                    field_column,
                                    opt_column,
                                    field_type) {
  checkmate::assert_data_frame(metadata, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_character(opt_column, len = 1L, null.ok = FALSE)
  checkmate::assert_character(field_type, len = 1L, null.ok = FALSE)
  checkmate::assert_character(field_column, len = 1L, null.ok = FALSE)

  stopifnot(opt_column %in% names(metadata))

  metadata     <- metadata[which(!is.na(metadata[[opt_column]]) &
                                   metadata[[field_type]] != "calc"), ]
  dictionary   <- NULL
  for (i in seq_len(nrow(metadata))) {
    dictionary <- rbind(dictionary,
                        dictionary_make_metadata(metadata[[opt_column]][i],
                                                 metadata[[field_column]][i]))
  }
  return(dictionary)
}

#' Make data dictionary for 1 field
#'
#' @param x A vector choices from the input data
#' @param field_column A character with the name of the field where the
#'    choices are made.
#'
#' @returns A data frame with the dictionary in the format that is accepted by
#'    the \{matchmaker\} package.
#' @keywords internal
#'
dictionary_make_metadata <- function(x, field_column) {
  splits          <- trimws(unlist(strsplit(x, "|", fixed = TRUE)))
  combined_splits <- lapply(splits, function(x) { trimws(unlist(strsplit(x, ",", fixed = TRUE))) }) # nolint: line_length_linter
  combined_splits <- do.call(rbind.data.frame, combined_splits)
  res             <- data.frame(options = combined_splits[, 1L],
                                values  = combined_splits[, 2L],
                                grp     = rep(field_column,
                                              nrow(combined_splits)),
                                orders  = seq_len(nrow(combined_splits)))
  rownames(res)   <- NULL
  return(res)
}

#' Add an element to the data dictionary
#'
#' @param dictionary A data frame with the data dictionary
#' @param option A vector of strings with the new options that need to be added
#'    to the dictionary.
#' @param value A vector with the values to be used when replacing the new
#'    options.
#' @param grp A vector with the name of the column that contains the option
#'    of interest.
#' @param order A numeric with the order of the new option.
#'
#' @returns An object of type data frame. This is the new data dictionary with an
#'    additional line that contains the details about the new options.
#' @export
#'
#' @examples
#' test <- add_to_dictionary(
#'   dictionary = readRDS(system.file("extdata", "test_dict.RDS",
#'                        package = "cleanepi")),
#'   option     = "ml",
#'   value      = "male",
#'   grp        = "gender",
#'   order      = NULL
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
  max_order      <- max(dictionary[["orders"]])
  tmp_dictionary <- dictionary %>%
    dplyr::filter(.data$grp == grp)

  dictionary     <- dictionary %>%
    dplyr::filter(.data$grp != grp)

  # make the new order
  new_order      <- order
  if (is.null(order)) {
    new_order    <- max_order + seq_along(option)
  }

  # make the data frame with the new option
  if (length(value) == 1L) {
    value        <- rep(value, length(option))
  }
  if (length(grp) == 1L) {
    grp          <- rep(grp, length(option))
  }
  new_option     <- data.frame(options = option,
                               values  = value,
                               grp     = grp,
                               orders  = new_order)
  tmp_dictionary <- rbind(tmp_dictionary, new_option)
  dictionary     <- rbind(dictionary, tmp_dictionary)
  return(dictionary)
}

#' Detect misspelled options in columns to be cleaned
#'
#' @inheritParams clean_using_dictionary
#'
#' @returns A list with the indexes of the misspelled values in every column
#'    that needs to be cleaned.
#' @keywords internal
#'
detect_misspelled_options <- function(data, dictionary) {
  cols_to_modify  <- unique(dictionary[["grp"]])
  outliers        <- list()
  for (col in cols_to_modify) {
    unique_values <- unique(data[[col]])[!is.na(unique(data[[col]]))]
    temp_dict     <- dictionary %>%
      dplyr::filter(.data$grp == col)
    opts          <- c(temp_dict[["options"]], unique(temp_dict[["values"]]))
    m             <- match(unique_values, opts)
    if (anyNA(m)) {
      outliers[[col]] <- which(data[[col]] == unique_values[is.na(m)])
    }
  }
  return(outliers)
}


#' Print the detected misspelled values
#'
#' @param misspelled_options A list with the misspelled values found in
#'    the different columns of the input data.
#'
#' @returns Prints out the misspelled values from the column of interest
#' @keywords internal
#'
print_misspelled_values <- function(misspelled_options) {
  for (opts in names(misspelled_options)) {
    message("\nDetected misspelled values at lines ",
            paste(misspelled_options[[opts]], sep = ", "),
            " of column '", opts, "'")
  }
}
