#' Convert Redcap data dictionary into {matchmaker} dictionary format
#'
#' @param metadata a `data frame` with the data dictionary associated to a
#'    Redcap project
#' @param field_column the name of the column, in the input dictionary, that
#'    contains the field names in of the Redcap project data
#' @param opt_column the name of the column, in the input dictionary, that
#'    contains the definition of the choices in every column of the Redcap
#'    project data
#'
#' @return a data frame with 4 columns. This is in the format required by the
#'    {matchmaker} R package for dictionary-based cleaning.
#' @keywords internal
#' @noRd
#'
make_readcap_dictionary <- function(metadata,
                                    field_column = "field_name",
                                    opt_column   = "select_choices_or_calculations") { # nolint: line_length_linter
  checkmate::assert_data_frame(metadata, min.rows = 1, min.cols = 1,
                               null.ok = FALSE)
  checkmate::assert_character(opt_column, len = 1, null.ok = FALSE)

  stopifnot(opt_column %in% names(metadata))

  metadata     <- metadata %>%
    dplyr::filter(!is.na(metadata[[opt_column]]))

  dictionary   <- NULL
  for (i in 1:nrow(metadata)) {
    print(i)
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
#'
#' @return
#' @export
#'
#' @examples
clean_using_dictionary <- function(data, dictionary) {
  checkmate::assert_data_frame(data, min.rows = 1L, min.cols = 1L,
                               null.ok = FALSE)
  checkmate::assert_data_frame(dictionary, min.rows = 1L, max.cols = 4L,
                               null.ok = FALSE)
  stopifnot(all(c("options", "values", "grp")) %in% names(dictionary) &&
              all(unique(dictionary[["grp"]])) %in% names(data))
  cleaned <- matchmaker::match_df(data,
                                  dictionary = dictionary,
                                  from       = "options",
                                  to         = "values",
                                  by         = "grp")
  cleaned
}
