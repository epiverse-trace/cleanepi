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
    dplyr::filter(!is.na(metadata[[opt_column]]) &
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
  splits          <- unlist(strsplit(x, "|", fixed = TRUE))
  splits          <- trimws(splits)
  combined_splits <- lapply(splits, get_meta_rows)
  combined_splits <- do.call(rbind.data.frame, combined_splits)
  res             <- data.frame(options = combined_splits[, 1L],
                                values  = combined_splits[, 2L],
                                grp     = rep(field_column,
                                              nrow(combined_splits)),
                                orders  = seq_len(nrow(combined_splits)))
  rownames(res)   <- NULL
  res
}

#' Split a character and remove white space at the beginning and the end
#'
#' @param x a `character` to be split
#'
#' @keywords internal
#' @noRd
#'
#' @returns a `vector` of character
get_meta_rows <- function(x) {
  x <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
}


#' Perform dictionary-based cleaning
#'
#' @param data the input data
#' @param dictionary a `data frame` with the data dictionary associated to the
#'    input data. This should be in the format
#' @param correct a `logical` specifying whether to correct the misspelled
#'    values in the data or not. default is `FALSE`
#'
#' @returns a `data frame` with cleaned values in the target columns specified in
#'    the data dictionary.
#' @export
#'
#' @examples
#' cleaned_df <- clean_using_dictionary(
#'   data       = system.file("extdata","messy_data.RDS",package = "cleanepi"),
#'   dictionary = system.file("extdata","test_dict.RDS",package = "cleanepi"),
#'   correct    = FALSE
#' )
clean_using_dictionary <- function(data, dictionary, correct = FALSE) {
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
    if (!correct) {
      print_misspelled_values(misspelled_options)
      message("Please correct them first or use `correct=TRUE` for ",
              "auto-correction")
      cleaned <- NULL
    } else {
      res    <- correct_misspelled_options(data, dictionary, misspelled_options)
      data   <- add_report(res[["data"]],
                           res[["report"]],
                           name = "misspel_correction")

      # perform the dictionary-based cleaning
      cleaned <- matchmaker::match_df(data,
                                      dictionary = dictionary,
                                      from       = "options",
                                      to         = "values",
                                      by         = "grp")
      cleaned <- add_report(cleaned,
                            unique(dictionary[["grp"]]),
                            name = "columns_modified_based_on_dictionary")
    }
  }

  cleaned
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
    temp_dict     <- dictionary |>
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

#' Correct the misspelled values with their closest values from the data
#' dictionary
#'
#' @inheritParams clean_using_dictionary
#' @param outliers
#'
#' @importFrom stringdist stringdist
#' @keywords internal
#' @noRd
correct_misspelled_options <- function(data, dictionary, outliers) {
  grp <- NULL
  checkmate::assert_list(outliers, min.len = 1L, null.ok = FALSE)
  for (target_col in names(outliers)) {
    target_values    <- dictionary |>
      dplyr::filter(grp == target_col)
    target_values    <- unique(target_values[["values"]])
    misspelled_words <- data[[target_col]][outliers[[target_col]]]
    target_dist      <- do.call("rbind", lapply(misspelled_words,
                                                stringdist::stringdist,
                                                target_values))
    rownames(target_dist) <- misspelled_words
    colnames(target_dist) <- target_values
    data[[target_col]][outliers[[target_col]]] <-
      colnames(target_dist)[apply(target_dist, 1L, which.min)]
    report <- data.frame(misspelled   = misspelled_words,
                         corrected_to = colnames(target_dist)[apply(target_dist,
                                                                    1L,
                                                                    which.min)])
  }
  list(
    data   = data,
    report = report
  )
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
