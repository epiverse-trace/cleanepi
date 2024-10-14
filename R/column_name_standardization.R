#' Standardize column names of a data frame or linelist
#'
#' All columns names will be reformatted to use the snake case. When the
#' conversion to snake case does not work as expected, use the `keep` and/or
#' `rename` arguments to reformat the column name properly.
#'
#' @param data The input data frame or linelist.
#' @param keep A vector of column names to maintain as they are. When dealing
#'    with a linelist, this can be set to `linelist_tags`, to maintain the
#'    tagged column names. The Default is `NULL`.
#' @param rename A named vector of column names to be renamed. This should be in
#'    the form of `c(new_name1 = "old_name1", new_name2 = "old_name2")` for
#'    example.
#'
#' @returns A data frame or linelist with easy to work with column names.
#'
#' @export
#' @examples
#' # do not rename 'date.of.admission'
#' cleaned_data <- standardize_column_names(
#'   data = readRDS(system.file("extdata", "test_df.RDS",
#'                              package = "cleanepi")),
#'   keep = "date.of.admission"
#' )
#'
#' # do not rename 'date.of.admission', but rename 'dateOfBirth' and 'sex' to
#' # 'DOB' and 'gender' respectively
#' cleaned_data <- standardize_column_names(
#'   data   = readRDS(system.file("extdata", "test_df.RDS",
#'                                package = "cleanepi")),
#'   keep   = "date.of.admission",
#'   rename = c(DOB = "dateOfBirth", gender = "sex")
#' )
#'
standardize_column_names <- function(data, keep = NULL, rename = NULL) {
  checkmate::assert_vector(keep, min.len = 0L, max.len = ncol(data),
                           null.ok = TRUE,
                           any.missing = FALSE)
  checkmate::assert_character(rename, min.len = 0L, null.ok = TRUE,
                              any.missing = FALSE)
  before <- colnames(data)

  # when rename is not NULL, get the indices of the old column names as a vector
  # and name them with the new names
  if (!is.null(rename)) {
    new_names    <- names(rename)
    current_names <- unname(rename)
    stopifnot(
      "Unrecognised column names specified in 'rename'" =
        all(current_names %in% before),
      "Replace column names already exists" =
        !any(new_names %in% before)
    )
    rename        <- match(current_names, before)
    names(rename) <- new_names
  }

  # when keep is 'linelist_tags', keep the tagged variables
  # also account for when target columns are provided as a vector or column
  # name or column indices or NULL
  keep <- get_target_column_names(data,
                                  target_columns = keep,
                                  cols           = NULL)
  kept <- before %in% keep

  # if they're anything apart from ASCII e.g. arabic, throw error
  # TODO replace snakecase with fixed list of diacritics swapable to English
  # TODO e.g. é,ê,è = e
  after <- make.unique(
    snakecase::to_snake_case(before, transliterations = "Latin-ASCII"),
    sep = "_"
  )
  if (!all(kept)) {
    after[kept]  <- before[kept]
  }
  after[rename]  <- names(rename)
  colnames(data) <- after
  colnames_info  <- data.frame(before, after)
  data           <- add_to_report(data, "colnames", colnames_info)
  return(data)
}

#' Get column names
#'
#' When several performing data cleaning operations using the `clean_data()`
#' function, the input column names might be altered by after the column names
#' cleaning. As a consequence of this, some cleaning operations will fail due to
#' the column names mismatch. This function is provided to anticipate on this
#' scenario, hence providing continuity between the cleaning operations.
#'
#' @param data the input data. It can also be a modified data generated in
#'    intermediate cleaning operations.
#' @param target_columns a vector of target column names
#'
#' @returns a vector of column names to be used for the target cleaning
#'    operations
#' @keywords internal
#'
retrieve_column_names <- function(data, target_columns) {
  # when 'linelist_tags' is provided, it will be returned as is
  if (identical(target_columns, "linelist_tags")) {
    return(target_columns)
  }

  # extract the report object to make it easily accessible
  report    <- attr(data, "report")
  if (!"colnames" %in% names(report)) {
    return(target_columns)
  }

  # when no target column is provided, it will return NULL
  if (is.null(target_columns)) {
    return(NULL)
  }

  # detect the current names
  # identify the old names
  new_names      <- target_columns[target_columns %in% names(data)]
  target_columns <- target_columns[!(target_columns %in% names(data))]
  if ("colnames" %in% names(report) &&
      all(target_columns %in% report[["colnames"]][["before"]])) {
    all_column_names <- report[["colnames"]]
    idx              <- match(target_columns, all_column_names[["before"]])
    new_names        <- c(new_names, all_column_names[["after"]][idx])
  }

  return(new_names)
}
