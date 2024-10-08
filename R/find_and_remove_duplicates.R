#' Remove duplicates
#'
#' @description
#' When removing duplicates, users can specify a set columns to consider with
#' the 'target_columns' argument.
#'
#' @param data A input data frame or linelist.
#' @param target_columns A vector of column names to use when looking for
#'    duplicates. When the input data is a `linelist` object, this
#'    parameter can be set to `linelist_tags` if you wish to look for duplicates
#'    on tagged columns only. Default is `NULL`.
#'
#' @returns A  data frame or linelist  without the duplicates values and nor
#'    constant columns.
#' @export
#'
#' @examples
#' no_dups <- remove_duplicates(
#'   data           = readRDS(system.file("extdata", "test_linelist.RDS",
#'                                        package = "cleanepi")),
#'   target_columns = "linelist_tags"
#' )
#' @importFrom rlang .data
#'
remove_duplicates <- function(data, target_columns = NULL) {

  # setting up the variables below to NULL to avoid linters
  report <- attr(data, "report")
  dat    <- data

  # get the correct names in case some have been modified - see the
  # `retrieve_column_names()` function for more details
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns <- get_target_column_names(dat, target_columns, cols = NULL)

  # find duplicates
  dups       <- find_duplicates(dat, target_columns)
  tmp_report <- attr(dups, "report")
  if ("duplicated_rows" %in% names(tmp_report) &&
      nrow(tmp_report[["duplicated_rows"]]) > 0L) {
    dups     <- tmp_report[["duplicated_rows"]]
    report   <- c(report, tmp_report)
    dat      <- dat %>%
      dplyr::mutate(row_id = seq_len(nrow(dat)))
  }

  # remove duplicates and keep the first instance of the duplicate in each
  # duplicate group
  dat <- dat %>%
    dplyr::distinct_at({{ target_columns }}, .keep_all = TRUE)

  if ("duplicated_rows" %in% names(tmp_report) &&
      nrow(tmp_report[["duplicated_rows"]]) > 0L) {
    tmp_target_columns <- c("row_id", target_columns)
    to_be_removed      <- suppressMessages(dplyr::anti_join(dups, dat) %>%
        dplyr::select({{ tmp_target_columns }}))
    report[["removed_duplicates"]] <- to_be_removed
  }

  attr(dat, which = "report") <- report
  return(dat)
}



#' Identify and return duplicated rows in a data frame or linelist.
#'
#' @param data A data frame or linelist.
#' @param target_columns A vector of columns names or indices to consider when
#'    looking for duplicates. When the input data is a `linelist` object, this
#'    parameter can be set to `tags`from which duplicates to be removed.
#'    Its default value is `NULL`, which considers duplicates across all
#'    columns.
#'
#' @returns A data frame or linelist of all duplicated rows with following 2
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
#' @importFrom rlang .data
#'
find_duplicates <- function(data, target_columns = NULL) {
  # get the target column names
  target_columns <- retrieve_column_names(data, target_columns)
  target_columns <- get_target_column_names(data, target_columns, cols = NULL)

  # We are not using janitor::find_dupes() because we want to record the row
  # number
  dups <- data %>%
    dplyr::mutate(row_id = seq_len(nrow(data)), .before = 1) %>%
    dplyr::filter(dplyr::n() > 1L, .by = dplyr::all_of(target_columns)) %>%
    dplyr::arrange(dplyr::pick(target_columns)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(target_columns))) %>%
    dplyr::mutate(
      group_id = dplyr::cur_group_id(),
      .after = "row_id"
    )

  if (nrow(dups) > 0L) {
    message("Found ", nrow(dups), " duplicated rows in the dataset. Please",
            " consult the report for more details.")
    to_be_shown <- dups %>%
      dplyr::select(c("row_id", "group_id", {{ target_columns }}))
    data <- add_to_report(x     = data,
                          key   = "duplicated_rows",
                          value = to_be_shown)
    data <- add_to_report(x     = data,
                          key   = "duplicates_checked_from",
                          value = toString(target_columns))
  }
  return(data)
}
