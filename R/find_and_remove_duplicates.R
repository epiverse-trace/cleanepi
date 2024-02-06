#' Remove duplicates and constant rows and columns
#'
#' @description
#' Removes duplicates and noise such as  empty rows and
#' columns, and constant columns. These operations are
#' automatically performed by default unless specified otherwise.
#' Users can specify a set columns to consider when removing
#' duplicates with the 'target_columns' argument.
#'
#' @param data A input data frame or linelist.
#' @param target_columns A vector of column names to use when looking for
#'    duplicates. When the input data is a `linelist` object, this
#'    parameter can be set to `tags` if you wish to look for duplicates on
#'    tagged columns only. Default is `NULL`.
#' @param remove A vector of duplicate indices to be removed. Duplicate indices
#'    are unique identifiers for all rows in the original data frame or linelist
#'    that are duplicates of each other based on the `target_columns`.
#'    If remove = `NULL` (default value), the first duplicate is kept and
#'    the rest of the duplicates in the group are removed.
#' @param rm_empty_rows A logical variable that is used to specify whether to remove
#'     empty rows or not. The default  value is `TRUE`.
#' @param rm_empty_cols A logical variable that is used to specify whether to remove
#'     empty columns or not. The default value is `TRUE`.
#' @param rm_constant_cols A logical variable that is used to specify whether to remove
#'     constant columns or not. The default value is `TRUE`.
#'
#' @return A  data frame or linelist  without the duplicates values and nor constant columns.
#' @export
#'
#' @examples
#' no_dups <- remove_duplicates(
#'   data             = readRDS(system.file("extdata", "test_linelist.RDS",
#'                                          package = "cleanepi")),
#'   target_columns   = "tags",
#'   remove           = NULL,
#'   rm_empty_rows    = TRUE,
#'   rm_empty_cols    = TRUE,
#'   rm_constant_cols = TRUE
#' )
#'
remove_duplicates <- function(data,
                              target_columns   = NULL,
                              remove           = NULL,
                              rm_empty_rows    = TRUE,
                              rm_empty_cols    = TRUE,
                              rm_constant_cols = TRUE) {

  # remove the empty rows and columns
  report <- attr(data, "report")
  dat    <- data %>%
    janitor::remove_empty(c("rows", "cols"))
  cols     <- rows <- NULL
  add_this <- "none"
  idx      <- which(!(names(data) %in% names(dat)))
  if (length(idx) > 0L) {
    cols <- names(data)[idx]
    if (!is.null(cols)) {
      add_this <- paste(cols, collapse = ", ")
    }
  }
  dat      <- add_to_report(x     = dat,
                            key   = "empty_columns",
                            value = add_this)
  if (nrow(summary(arsenal::comparedf(data, dat))[["obs.table"]]) > 0L) {
    rows   <- summary(arsenal::comparedf(data,
                                         dat))[["obs.table"]][["observation"]]
    if (!is.null(rows)) {
      add_this <- rows
    }
  }

  # remove constant columns
  add_this <- "none"
  data     <- dat
  dat      <- data %>% janitor::remove_constant()
  idx      <- which(!(names(data) %in% names(dat)))
  if (length(idx) > 0L) {
    add_this <- paste(names(data)[idx], collapse = ", ")
    cols     <- c(cols, names(data)[idx])
  }
  dat        <- add_to_report(x     = dat,
                              key   = "constant_columns",
                              value = add_this)

  # get the target column names
  if (is.null(target_columns)) {
    target_columns <- names(dat)
  }
  target_columns   <- get_target_column_names(dat, target_columns, cols)

  # find duplicates
  add_this         <- "none"
  dups             <- find_duplicates(dat, target_columns)

  # remove duplicates
  if (is.null(remove)) {
    # remove duplicates and keep the first instance of the duplicate in each
    # duplicate group
    dat <- dat %>%
      dplyr::distinct_at({{ target_columns }}, .keep_all = TRUE)
  } else {
    # remove duplicates from user specified rows
    dat <- dat[-remove, ]
  }


  if (nrow(dups) > 0L) {
    dups[["row_id"]] <- NULL
    add_this         <- list()
    add_this[["duplicated_rows"]]         <- dups
    add_this[["duplicates_checked_from"]] <- paste(target_columns,
                                                   collapse = ", ")
    add_this[["removed_duplicates"]]      <- dplyr::anti_join(dups, dat)
  }

  dat        <- add_to_report(x     = dat,
                              key   = "duplicates",
                              value = add_this)
  tmp_report <- attr(dat, "report")
  report     <- c(report, tmp_report)
  attr(dat, which = "report") <- report
  return(dat)
}



#' Identify and return duplicated rows in a data frame or linelist.
#'
#' @param data A data frame or linelist.
#' @param target_columns A vector of columns names or indices to consider when
#'    looking for duplicates. When the input data is a `linelist` object, this
#'    parameter can be set to `tags`from which duplicates to be removed.
#'    Its default value is `NULL`, which considers duplicates across all columns.
#'
#' @return A data frame or linelist of all duplicated rows with following 2
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
#'   data           = readRDS(system.file("extdata", "test_linelist.RDS",
#'                                        package = "cleanepi")),
#'   target_columns = c("dt_onset", "dt_report", "sex", "outcome")
#' )
#'
find_duplicates <- function(data, target_columns = NULL) {
  # get the target column names
  if (is.null(target_columns)) {
    target_columns <- names(data)
  }
  target_columns   <- get_target_column_names(data, target_columns, cols = NULL)

  # find duplicates
  num_dups <- row_id <- group_id <- NULL
  dups <- data %>%
    dplyr::group_by_at(dplyr::vars(target_columns)) %>%
    dplyr::mutate(num_dups = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row_id = seq_len(nrow(data))) %>%
    dplyr::arrange(dplyr::pick(target_columns)) %>%
    dplyr::filter(num_dups > 1L) %>%
    dplyr::select(-c(num_dups)) %>%
    dplyr::group_by_at(dplyr::vars(target_columns)) %>%
    dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
    dplyr::select(row_id, group_id, dplyr::everything())

  return(dups)
}


#' Get the names of the columns from which duplicates will be found
#'
#' @param data A dataframe or linelist
#' @param target_columns A vector of column names
#' @param cols A vector of empty and constant columns
#'
#' @return A vector with the target column names or indexes
#'
#' @keywords internal
#'
get_target_column_names <- function(data, target_columns, cols) {
  if (is.null(target_columns)) {
    return(names(data))
  }

  # extract column names if target_columns is a vector of column indexes
  if (is.numeric(target_columns)) {
    target_columns <- names(data)[target_columns]
  }

  # check for linelist object if target_columns='tags'
  if (all(length(target_columns) == 1L && target_columns == "tags")) {
    stopifnot(
      "'tags' only works on linelist object. Please provide a vector of
              column names if you are dealing with a data frame" =
        inherits(data, "linelist")
    )
    original_tags  <- linelist::tags(data)
    target_columns <- as.character(original_tags)
  }

  # check whether target columns are part of the empty or constant columns
  if (!is.null(cols)) {
    idx <- which(cols %in% target_columns)
    if (length(idx) > 0L) {
      target_columns <- target_columns[-idx]
      stopifnot("All specified columns are either constant or empty." =
                  length(target_columns) > 0L)
    }
  }

  return(target_columns)
}
