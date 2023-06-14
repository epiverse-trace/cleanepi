#' Remove duplicates based on selected columns from a data frame
#' or linelist object.
#'
#' @param data The input data frame.
#' @param selected_columns A vector of column names to use when looking for
#'    duplicates. When the input data is a `linelist` object, this
#'    parameter can be set to `tags` if you wish to look for duplicates on
#'    tagged columns.
#' @param remove A vector of duplicate indices to be removed.
#' Duplicate indices are unique identifiers for all rows in the original
#' data frame or linelist that are duplicates of each other based on the
#' `selected_columns`.
#' If remove = NULL (default value), the first duplicate is kept and
#' the rest of the duplicates in the group are removed.
#' @param report A list with the information about the effects of the
#' cleaning steps.
#'
#' @return A `list` with elements data (the filtered dataset) and report.
#' @export
#'
#' @examples
#' no_dups <- remove_duplicates(
#'   data = readRDS(system.file("extdata", "test_linelist.RDS",
#'     package = "cleanepi"
#'   )),
#'   selected_columns = "tags",
#'   report = list()
#' )
#'
remove_duplicates <- function(data, selected_columns,
                              remove = NULL, report = list()) {
  if (is.null(selected_columns)) {
    selected_columns <- names(data)
  }

  # check for linelist object if selected_columns='tags'
  x_class <- class(data)
  if (all(length(selected_columns) == 1 & selected_columns == "tags")) {
    stopifnot(
      "'tags' only works on linelist object. Please provide a vector of
              column names if you are dealing with a data frame" =
        "linelist" %in% x_class
    )
    original_tags <- linelist::tags(data)
    selected_columns <- as.character(original_tags)
  }

  # extract column names if selected_columns is a vector of column indexes
  if (is.numeric(selected_columns)) {
    selected_columns <- names(data)[selected_columns]
  }

  # find duplicates
  dups <- find_duplicates(data, selected_columns)

  # remove duplicates (by keeping the first instance of the duplicate in each
  # duplicate group)
  if (remove == -1) {
    data$row_id <- seq_len(nrow(data))
    data <- data %>%
      dplyr::distinct(dplyr::pick(selected_columns), .keep_all = TRUE)
  } else {
    data <- data[-remove, ]
  }

  if (nrow(dups) > 0) {
    if (!("remove_dupliates" %in% names(report))) {
      report[["remove_dupliates"]] <- list()
      report[["remove_dupliates"]][["all_dups"]] <- dups
      idx <- which(!(dups$row_id %in% data$row_id))
      report[["remove_dupliates"]][["removed_dups"]] <- dups[idx, ]
      report[["remove_dupliates"]][["duplicates_checked_from"]] <-
        glue::glue_collapse(selected_columns, sep = ", ")
    }
  }

  list(
    data = data,
    report = report
  )
}



#' Identify and return duplicated rows in a data frame or linelist.
#'
#' @param data The input data frame or linelist.
#' @param selected_columns A vector of columns names or indices to consider when
#'    looking for duplicates. When the input data is a `linelist` object,
#'    this parameter can be set to `tags` if you wish to look for
#'    duplicates across the tagged variables only.
#'
#' @return Data frame or linelist of all duplicated rows with following 2
#'    additional columns:
#'    \enumerate{
#'      \item `row_id`: the indices of the duplicated rows from the input data.
#'          Users can choose from these indices, which row they consider as
#'          redundant in each group of duplicates.
#'      \item `group_id`: a unique identifier associated to each group of
#'          duplicates.
#'    }
#'
#' @export
#'
#' @examples
#' dups <- find_duplicates(
#'   data = readRDS(system.file("extdata", "test_linelist.RDS",
#'     package = "cleanepi"
#'   )),
#'   selected_columns = "tags"
#' )
#'
find_duplicates <- function(data, selected_columns) {
  # find duplicates
  dups <- data %>%
    dplyr::group_by(dplyr::pick(selected_columns)) %>%
    dplyr::mutate(num_dups = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_id = seq_len(nrow(data))) %>%
    dplyr::arrange(dplyr::pick(selected_columns)) %>%
    dplyr::filter(num_dups > 1) %>%
    dplyr::select(-c(num_dups)) %>%
    dplyr::group_by(dplyr::pick(selected_columns)) %>%
    dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
    dplyr::select(row_id, group_id, dplyr::everything())

  dups
}
