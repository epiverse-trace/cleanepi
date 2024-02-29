#' Standardize column names of a data frame or linelist
#'
#' @param data The input data frame or linelist.
#' @param keep A vector of column names to maintain as they are. When dealing
#'    with a linelist, this can be set to `linelist_tags`, to maintain the
#'    tagged column names. The Default is `NULL`.
#' @param rename an expression used to specify how to rename some columns.
#'
#' @return A data frame with easy to work with column names.
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
#'   rename = c("dateOfBirth = DOB, sex=gender")
#' )
#'
standardize_column_names <- function(data, keep = NULL, rename = NULL) {
  checkmate::assert_vector(keep, min.len = 0L, null.ok = TRUE,
                           any.missing = FALSE)
  checkmate::assert_character(rename, min.len = 0L, null.ok = TRUE,
                              any.missing = FALSE)
  before <- colnames(data)

  # when rename is not NULL, get the new column names
  rename <- get_new_names(original_names = before,
                         target_columns = rename)

  # when keep is 'linelist_tags', keep the tagged variables
  keep   <- get_target_column_names(data,
                                    target_columns = keep,
                                    cols           = NULL)

  # if they're anything apart from ASCII e.g. arabic, throw error
  # TODO replace snakecase with fixed list of diacritics swapable to English
  # TODO e.g. é,ê,è = e
  after  <- make.unique(
    snakecase::to_snake_case(before, transliterations = "Latin-ASCII"),
    sep = "_"
  )
  kept           <- which(before %in% keep)
  after[kept]    <- before[kept]
  after[rename]  <- names(rename)
  colnames(data) <- after

  colnames_info <- data.frame(before, after)
  data          <- add_to_report(data, "colnames", colnames_info)
  return(data)
}

#' Get the indices of the columns to be renamed
#'
#' @param original_names A vector with the column names of the input data
#' @param target_columns The expression that specifies how the column names
#'    should be renamed
#'
#' @return A named vector of the indices of the columns to be renamed
#' @keywords internal
#'
get_new_names <- function(original_names, target_columns) {
  if (is.null(target_columns)) {
    idx          <- seq_along(original_names)
    names(idx)   <- original_names
    return(idx)
  }
  target_columns <- unlist(strsplit(target_columns, ",", fixed = TRUE))
  target_columns <- strsplit(target_columns, "=", fixed = TRUE)
  curent         <- new <- NULL
  for (i in seq_along(target_columns)) {
    curent       <- c(curent, trimws(target_columns[[i]][[1L]]))
    new          <- c(new, trimws(target_columns[[i]][[2L]]))
  }
  stopifnot("Unrecognised column names specified in 'rename'" =
              all(curent %in% original_names))
  idx            <- match(curent, original_names)
  names(idx)     <- new
  return(idx)
}
