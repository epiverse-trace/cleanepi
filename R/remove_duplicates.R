
#' Remove duplicates from a data frame or linelist object
#'
#' @param data the input data frame
#' @param duplicates_from a vector of columns names to use when looking for
#'    duplicates. When the input data is a `linelist` object, this
#'    parameter can be set to `tags` if you wish to look for duplicates on
#'    tagged variables. Only used when `remove_duplicates=TRUE`
#' @param remove a vector of the indices of the duplicated rows to be removed.
#'    Default is `-1` i.e. to keep only the first instance of duplicated rows.
#' @param report a list with the information about the effects of the
#'    cleaning steps
#'
#' @return a list with 2 elements: the filtered dataset and the report object
#' @export
#'
#' @examples
#' no_dups <- remove_duplicates(
#'   data = readRDS(system.file("extdata", "test_linelist.RDS",
#'   package = "cleanepi")),
#'   duplicates_from = "tags",
#'   remove = -1,
#'   report = list()
#' )
#'
remove_duplicates <- function(data, duplicates_from,
                              remove = -1, report = list()) {
  if (is.null(duplicates_from)) {
    duplicates_from <- names(data)
  }

  # check for linelist object if duplicates_from='tags'
  x_class <- class(data)
  if (all(length(duplicates_from) == 1 & duplicates_from == "tags")) {
    stopifnot("'tags' only works on linelist object. Please provide a vector of
              column names if you are dealing with a data frame" =
                "linelist" %in% x_class)
    original_tags <- linelist::tags(data)
    duplicates_from <- as.character(original_tags)
  }

  # extract column names if duplicates_from is a vector of column indexes
  if (is.numeric(duplicates_from)) {
    duplicates_from <- names(data)[duplicates_from]
  }

  # find duplicates
  dups <- find_duplicates(data, duplicates_from)

  # remove duplicates (by keeping the first instance of the duplicate in each
  # duplicate group)
  if (remove == -1) {
    data$row_id <- seq_len(nrow(data))
    data <- data %>%
      dplyr::distinct(dplyr::pick(duplicates_from), .keep_all = TRUE)
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
        glue::glue_collapse(duplicates_from, sep = ", ")
    }
  }

  list(
    data = data,
    report = report
  )
}



#' Identify duplicated rows in a data frame or linelist
#'
#' @param data the input data frame or linelist
#' @param duplicates_from a vector of columns names or indices to consider when
#'    looking for duplicates. When the input data is a `linelist` object,
#'    this parameter can be set to `tags` if you wish to look for
#'    duplicates across the tagged variables only.
#'
#' @return a data frame or linelist of all duplicated rows with following 2
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
#' data = readRDS(system.file("extdata", "test_linelist.RDS",
#'   package = "cleanepi")),
#' duplicates_from = "tags"
#' )
#'
find_duplicates <- function(data, duplicates_from) {
  # find duplicates
  dups <- data %>%
    dplyr::group_by(dplyr::pick(duplicates_from)) %>%
    dplyr::mutate(num_dups = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_id = seq_len(nrow(data))) %>%
    dplyr::arrange(dplyr::pick(duplicates_from)) %>%
    dplyr::filter(num_dups > 1) %>%
    dplyr::select(-c(num_dups)) %>%
    dplyr::group_by(dplyr::pick(duplicates_from)) %>%
    dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
    dplyr::select(row_id, group_id, dplyr::everything())

  dups
}
