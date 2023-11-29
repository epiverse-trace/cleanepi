#' Convert Redcap data dictionary into {matchmaker} dictionary format
#'
#' @param metadata a `data frame` with the data dictionary associated to a
#'    Redcap project
#' @param field_column the name of the column, in the input dictionary, that
#'    contains the field names in of the Redcap project data
#' @param opt_column the name of the column, in the input dictionary, that
#'    contains the definition of the choices in every column of the Redcap
#'    project data
#' @param field_type a `character` with the name of the column that contains the
#'    field type information
#'
#' @return a data frame with 4 columns. This is in the format required by the
#'    {matchmaker} R package for dictionary-based cleaning.
#' @keywords internal
#' @noRd
#'
#' @examples
#' test <- make_readcap_dictionary(
#'   metadata     = system.file("extdata", "test_readcap_dictionary.RDS",
#'                              package = "cleanepi"),
#'   field_column = "field_name",
#'   opt_column   = "select_choices_or_calculations",
#'   field_type   = "field_type"
#' )
#'
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

  metadata     <- metadata %>%
    dplyr::filter(!is.na(metadata[[opt_column]]),
                  metadata[[field_type]] != "calc")

  dictionary   <- NULL
  for (i in seq_len(nrow(metadata))) {
    dictionary <- rbind(dictionary,
                        make_metadata(metadata[[opt_column]][i],
                                      metadata[[field_column]][i]))
  }
  dictionary
}

#' Make data dictionary for 1 field
#'
#' @param x a `vector` choices from the input data
#' @param field_column a `character` with the name of the field where the
#'    choices are made.
#'
#' @return a `data frame` with the dictionary in the format that is accepted by
#'    the {matchmaker} package.
#' @keywords internal
#' @noRd
#'
make_metadata <- function(x, field_column) {
  splits          <- trimws(unlist(strsplit(x, "|", fixed = TRUE)))
  combined_splits <- lapply(splits, function(x) { trimws(unlist(strsplit(x, ",", fixed = TRUE))) }) # nolint: line_length_linter
  combined_splits <- do.call(rbind.data.frame, combined_splits)
  res             <- data.frame(options = combined_splits[, 1L],
                                values  = combined_splits[, 2L],
                                grp     = rep(field_column,
                                              nrow(combined_splits)),
                                orders  = seq_len(nrow(combined_splits)))
  rownames(res)   <- NULL
  res
}


#' Perform dictionary-based cleaning
#'
#' @param data the input data
#' @param dictionary a `data frame` with the data dictionary associated to the
#'    input data. This should be in the format
#'
#' @returns a `data frame` with cleaned values in the target columns specified
#'    in the data dictionary.
#' @export
#'
#' @examples
#' cleaned_df <- clean_using_dictionary(
#'   data       = readRDS(system.file("extdata", "messy_data.RDS",
#'                        package = "cleanepi")),
#'   dictionary = readRDS(system.file("extdata", "test_dict.RDS",
#'                        package = "cleanepi"))
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
    stop("Please add the misspelled options to the data dictionary using the ",
         "add_to_dictionnary() function.")
  } else {
    # perform the dictionary-based cleaning
    data   <- matchmaker::match_df(data,
                                   dictionary = dictionary,
                                   from       = "options",
                                   to         = "values",
                                   by         = "grp")

    # add the result to the reporting object
    data   <- add_report(data,
                         unique(dictionary[["grp"]]),
                         name = "columns_modified_based_on_dictionary")
  }

  data
}

#' Add an element to the data dictionary
#'
#' @param dictionary a data frame with the data dictionary
#' @param option a `character` with the new option to add to the dictionary
#' @param value a `character` with the value the new option should be replaced
#'    with
#' @param grp a `character` with the name of the column that contains the option
#'    of interest
#' @param order a `numeric` with the order of the new option
#'
#' @return an object of type data frame. This is the new data dictionary with an
#'    additional line that contains the details about the new options.
#' @export
#'
#' @examples
#' test <- add_to_dictionnary(
#'   dictionary = readRDS(system.file("extdata", "test_dict.RDS",
#'                        package = "cleanepi")),
#'   option     = "ml",
#'   value      = "male",
#'   grp        = "gender",
#'   order      = NULL
#'  )
add_to_dictionnary <- function(dictionary,
                               option,
                               value,
                               grp,
                               order = NULL) {
  checkmate::assert_character(option, len = 1L, any.missing = FALSE,
                              null.ok = FALSE)
<<<<<<< HEAD
  checkmate::assert_character(value, len = 1L, any.missing = TRUE,
                              null.ok = FALSE)
  checkmate::assert_character(grp, len = 1L, any.missing = FALSE,
                              null.ok = FALSE)
  checkmate::assert_numeric(order, any.missing = TRUE, lower = 1L,
=======
  checkmate::assert_character(value, len = 1L, any.missing = FALSE,
                              null.ok = FALSE)
  checkmate::assert_character(grp, len = 1L, any.missing = FALSE,
                              null.ok = FALSE)
  checkmate::assert_numeric(order, any.missing = FALSE, lower = 1L,
>>>>>>> 7ea018e (Add dictionary-based cleaning functions (#33))
                            null.ok = TRUE)

  # select the lines in the data dictionary where the column name is the same as
  # the value of the grp argument
  tmp_dictionary <- dictionary %>%
    dplyr::filter(grp == grp)

  dictionary     <- dictionary %>%
    dplyr::filter(grp != grp)

  # make the new order
  new_order      <- ifelse(is.null(order),
                           max(tmp_dictionary[["orders"]]) + 1L,
                           order)

  # make the data frame with the new option
  new_option     <- data.frame(options = option,
                               values  = value,
                               grp     = grp,
                               orders  = new_order)
  tmp_dictionary <- rbind(tmp_dictionary, new_option)
  dictionary     <- rbind(dictionary, tmp_dictionary)
  dictionary
}

#' Detect misspelled options in columns to be cleaned
#'
#' @inheritParams clean_using_dictionary
#'
#' @return a `list` with the indexes of the misspelled values in every column
#'    that needs to be cleaned
#' @keywords internal
#' @noRd
#'
detect_misspelled_options <- function(data, dictionary) {
  grp <- NULL
  cols_to_modify  <- unique(dictionary[["grp"]])
  outliers        <- list()
  for (col in cols_to_modify) {
    unique_values <- unique(data[[col]])[!is.na(unique(data[[col]]))]
    temp_dict     <- dictionary %>%
      dplyr::filter(grp == col)
    opts          <- c(temp_dict[["options"]], unique(temp_dict[["values"]]))
    m             <- match(unique_values, opts)
    which(!(unique(data[[col]]) %in% dictionary[["options"]]))
    if (anyNA(m)) {
      outliers[[col]] <- which(data[[col]] == unique_values[which(is.na(m))])
    }
  }
  outliers
}


#' Print the detected misspelled values
#'
#' @param misspelled_options a name `list` with the misspelled values found in
#'    the different columns of the input data
#'
#' @keywords internal
#' @noRd
#'
print_misspelled_values <- function(misspelled_options) {
  for (opts in names(misspelled_options)) {
    message("\nDetected misspelled values at lines ",
            glue::glue_collapse(misspelled_options[[opts]], sep = ", "),
            " of column '", opts, "'")
  }
}
