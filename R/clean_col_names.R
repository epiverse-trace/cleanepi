#' clean column names of a data frame
#'
#' @param x the input data frame
#' @param report a list with the information about the effects of the
#'          the cleaning steps
#' @param keep A vector of column names to keep as-is. Default is `NULL`.
#'
#' @return the a list with 2 elements: input data frame with a knit column names
#'          and the report object
#' @keywords internal
#' @noRd
#'
clean_col_names <- function(x, report = list(), keep = NULL) {
  original_names <- col_names <- colnames(x)

  # in case the user wants to keep some column names as they are,
  # they should be provided as value for the keep arguments.
  # here we will make sure to spare those column names from been modified

  # check if the column names to be kept exist
  if (!is.null(keep)) {
    stopifnot(
      "\nIncorrect column names were provided: " =
        all(keep %in% original_names)
    )
  }

  # get the indexes of the column names to not change
  idx <- match(keep, original_names)

  # We have opted for snake case in the column names. Thus, all camel cases will
  # be converted into snake cases.
  if (length(idx) > 0L) {
    tmp_col_names <- snakecase::to_snake_case(col_names[-idx])
    cleaned_names <- epitrix::clean_labels(tmp_col_names)
    # make column name unique
    unique_names <- make.unique(cleaned_names, sep = "_")
    # update the column names
    col_names[-idx] <- unique_names
  } else {
    col_names <- snakecase::to_snake_case(col_names)
    col_names <- epitrix::clean_labels(col_names)
    # make column name unique
    col_names <- make.unique(col_names, sep = "_")
  }

  # detect modified column names from the previous command
  original_name <- new_name <- NULL
  xx <- as.data.frame(cbind(
    original_name = original_names,
    new_name = col_names
  )) |>
    dplyr::mutate(
      original_name = as.character(original_name),
      new_name = as.character(new_name)
    )
  idx <- which(xx[["original_name"]] != xx[["new_name"]])
  if (length(idx) > 0L) {
    report[["modified_column_names"]] <- xx[idx, ]
  }

  names(x) <- col_names
  list(
    data   = x,
    report = report
  )
}
