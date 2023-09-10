#' Remove duplicates based on selected columns from a data frame
#' or linelist object.
#'
#' @param data The input data frame.
#' @param target_columns A vector of column names to use when looking for
#'    duplicates. When the input data is a `linelist` object, this
#'    parameter can be set to `tags` if you wish to look for duplicates on
#'    tagged columns.
#' @param remove A vector of duplicate indices to be removed.
#'    Duplicate indices are unique identifiers for all rows in the original
#'    data frame or linelist that are duplicates of each other based on the
#'    `target_columns`.
#'    If remove = NULL (default value), the first duplicate is kept and
#'    the rest of the duplicates in the group are removed.
#' @param report A list with the information about the effects of the
#'    cleaning steps.
#'
#' @return A `list` with elements data (the filtered dataset) and report.
#' @export
#'
#' @examples
#' no_dups <- remove_duplicates(
#'   data           = readRDS(system.file("extdata", "test_linelist.RDS",
#'                              package = "cleanepi")),
#'   target_columns = "tags",
#'   remove         = NULL,
#'   report         = list()
#' )
#'
remove_duplicates <- function(data, target_columns,
                              remove = NULL, report = list()) {
  # get the target column names
  target_columns <- get_target_column_names(data, target_columns)

  # extract column names if target_columns is a vector of column indexes
  if (is.numeric(target_columns)) {
    target_columns <- names(data)[target_columns]
  }

  # find duplicates
  dups <- find_duplicates(data, target_columns)
  data$row_id <- seq_len(nrow(data))

  # remove duplicates
  if (is.null(remove)) {
    # remove duplicates by keeping the first instance of the duplicate in each
    # duplicate group
    data <- data %>%
      dplyr::select(dplyr::all_of({{ target_columns }})) %>%
      dplyr::distinct(.keep_all = TRUE)
  } else {
    # remove duplicates from user specified rows
    data <- data[-remove, ]
  }

  if (nrow(dups) > 0) {
      report[["remove_duplicates"]] <- list()
      report[["remove_duplicates"]][["all_dups"]] <- dups
      idx <- which(!(dups$row_id %in% data$row_id))
      report[["remove_duplicates"]][["removed_dups"]] <- dups[idx, ]
      report[["remove_duplicates"]][["duplicates_checked_from"]] <-
        glue::glue_collapse(target_columns, sep = ", ")
  }

  if ("row_id" %in% names(data)) {
    data <- data %>% dplyr::select(-c(row_id))
  }

  list(
    data = data,
    report = report
  )
}



#' Identify and return duplicated rows in a data frame or linelist.
#'
#' @param data The input data frame or linelist.
#' @param target_columns A vector of columns names or indices to consider when
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
#'     package = "cleanepi")),
#'   target_columns = c("dt_onset", "dt_report", "sex", "outcome")
#' )
#'
find_duplicates <- function(data, target_columns) {
  # get the target column names
  target_columns <- get_target_column_names(data, target_columns)

  # find duplicates
  num_dups <- row_id <- group_id <- NULL
  dups <- data %>%
    dplyr::group_by(dplyr::pick({{ target_columns }})) %>%
    dplyr::mutate(num_dups = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_id = seq_len(nrow(data))) %>%
    dplyr::arrange(dplyr::pick({{ target_columns }})) %>%
    dplyr::filter(num_dups > 1) %>%
    dplyr::select(-c(num_dups)) %>%
    dplyr::group_by(dplyr::pick({{ target_columns }})) %>%
    dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
    dplyr::select(row_id, group_id, dplyr::everything())

  dups
}


#' Get the names of the columns from which duplicates will be found
#'
#' @param data the input dataset
#' @param target_columns the user specified target column name
#'
#' @return a `vector` with the target column names or indexes
#'
#' @keywords internal
#'
get_target_column_names <- function(data, target_columns) {
  if (is.null(target_columns)) {
    return(names(data))
  }

  # check for linelist object if target_columns='tags'
  x_class <- class(data)
  if (all(length(target_columns) == 1 && target_columns == "tags")) {
    stopifnot(
      "'tags' only works on linelist object. Please provide a vector of
              column names if you are dealing with a data frame" =
        "linelist" %in% x_class
    )
    original_tags  <- linelist::tags(data)
    target_columns <- as.character(original_tags)
  }

  target_columns
}
