#' Standardize column names of a data frame or line list
#'
#' All columns names will be reformatted to  snake_case. When the
#' conversion to snakecase does not work as expected, use the \code{keep} and/or
#' \code{rename} arguments to reformat the column name properly.
#'
#' @param data The input \code{<data.frame>} or \code{<linelist>}.
#' @param keep A \code{<vector>} of column names to maintain as they are. When
#'    dealing with a \code{<linelist>}, this can be set to \code{linelist_tags},
#'    to maintain the tagged column names. The Default is \code{NULL}.
#' @param rename A named \code{<vector>} of column names to be renamed. This
#'    should be in the form of
#'    \code{c(new_name1 = "old_name1", new_name2 = "old_name2")} for example.
#'
#' @returns A \code{<data.frame>} or \code{<linelist>} with easy to work with
#'    column names.
#'
#' @export
#' @examples
#' # do not rename 'date.of.admission'
#' cleaned_data <- standardize_column_names(
#'   data = readRDS(
#'     system.file("extdata", "test_df.RDS", package = "cleanepi")
#'   ),
#'   keep = "date.of.admission"
#' )
#'
#' # do not rename 'date.of.admission', but rename 'dateOfBirth' and 'sex' to
#' # 'DOB' and 'gender' respectively
#' cleaned_data <- standardize_column_names(
#'   data = readRDS(
#'     system.file("extdata", "test_df.RDS", package = "cleanepi")
#'   ),
#'   keep = "date.of.admission",
#'   rename = c(DOB = "dateOfBirth", gender = "sex")
#' )
#'
standardize_column_names <- function(data, keep = NULL, rename = NULL) {
  checkmate::assert_vector(keep, min.len = 0L, max.len = ncol(data),
                           null.ok = TRUE,
                           any.missing = FALSE)
  checkmate::assert_character(rename, min.len = 0L, null.ok = TRUE,
                              any.missing = FALSE)
  # save the initial column names for comparison matters
  before <- colnames(data)

  # when rename is not NULL, get the indices of the old column names as a vector
  # and name them with the new names
  idx_rename <- numeric()
  if (!is.null(rename)) {
    new_names <- names(rename)
    current_names <- unname(rename)
    # abort if any of the specified column to be renamed are not part of the
    # input data frame
    are_cols_present <- current_names %in% before
    if (!all(are_cols_present)) {
      incorrect_col_names <- current_names[!are_cols_present] # nolint: object_usage_linter
      cli::cli_abort(c(
        tr_("Cannot rename {cli::qty(length(incorrect_col_names))} {?an/ } unrecognised column name{?s} specified in {.emph rename} argument: {.val {toString(incorrect_col_names)}}."), # nolint: line_length_linter
        i = tr_("Make sure that the columns to be renamed are part of the input data."), # nolint: line_length_linter
        i = tr_("To rename columns, use: {.emph rename = c(new_name1 = 'old_name1', new_name2 = 'old_name2')}.") # nolint: line_length_linter
      ), call = NULL)
    }
    # abort if any of the new name is already a column name in the input data
    # frame
    if (any(new_names %in% before)) {
      existing_cols <- new_names[which(new_names %in% before)] # nolint: object_usage_linter
      cli::cli_abort(c(
        tr_("The provided replace column {cli::qty(length(existing_cols))} name{?s} already exist."), # nolint: line_length_linter
        i = tr_("All new names must be different from existing column names."),
        "x" = tr_("You must use {cli::qty(length(existing_cols))} {?a/ } different name{?s} for the following column{?s}: {.field {toString(existing_cols)}}.") # nolint: line_length_linter
      ))
    }
    rename <- idx_rename <- match(current_names, before)
    names(rename) <- new_names
  }

  # when keep is 'linelist_tags', keep the tagged variables
  # also account for when target columns are provided as a vector or column
  # name or column indices or NULL
  keep <- get_target_column_names(data, target_columns = keep, cols = NULL)
  kept <- before %in% keep
  if (all(kept)) idx_keep <- numeric() else idx_keep <- which(kept)

  # make the column names unique
  # keep and rename must be preserved as requested by the user. for this reason
  # we set them to "leaveY" before the transformation (where Y represents the
  # indices from 1 to total number of columns to keep and rename)
  idx <- unique(c(idx_keep, idx_rename))
  x <- before
  x[idx] <- paste0("leave", seq_along(idx))
  colnames(data) <- x
  after <- colnames(janitor::clean_names(data))

  if (!all(kept)) {
    after[kept] <- before[kept]
  }
  after[rename] <- names(rename)

  # because of the {janitor} transformation, the resulting names could be
  # similar to the one we want to keep.
  # we need to make them unique in order to differentiate them with the one to
  # be kept or rename
  after <- make_unique_column_names(after, kept, before, rename)


  # apply the new names to the data frame and make the report
  colnames(data) <- after
  colnames_info <- data.frame(before, after)
  data <- add_to_report(data, "colnames", colnames_info)
  return(data)
}

#' Make column names unique when duplicated column names are found after the
#' transformation
#'
#' @param after A \code{<vector>} with the transformed column names
#' @param kept A \code{<logical>} vector where column names to keep are set to
#'    TRUE
#' @param before A \code{<vector>} with the initial column names
#' @param rename A \code{<vector>} with the indices of the column names to be
#'    renamed
#'
#' @return An adjusted \code{<vector>} if there were duplicated names introduced
#'    due to the transformation
#' @keywords internal
#'
make_unique_column_names <- function(after, kept, before, rename) {
  # detect those that are similar to the ones we want to keep or rename
  # make them unique
  t <- table(after)
  duplicates <- names(t[t > 1])

  # do not proceed if there is no duplicates
  if (length(duplicates) == 0) {
    return(after)
  }

  for (dup in duplicates) {
    # only consider column names starting with the one of interest
    are_duplicates <- grepl(sprintf("^%s", dup), after)

    # split them to access the length of each element
    splits <- strsplit(
      after[are_duplicates], paste0(dup, "_"), fixed = TRUE
    )
    sizes <- as.numeric(lapply(splits, length))

    if (all(sizes == 1)) {
      # if no element has length > 1, append 1:n to each duplicates
      after[are_duplicates] <- paste(dup, seq_along(sizes), sep = "_")
    } else {
      # if some element has length > 1, identify those where second element is a
      # number and use that number as a reference.
      idx <- which(sizes > 1)
      are_numeric <- suppressWarnings(
        as.numeric(
          lapply(splits[idx], function(x) {
            unlist(x)[[2]]
          })
        )
      )

      # use the numeric values extracted from the splits to choose what number
      # will be appended to the duplicates
      if (all(is.na(are_numeric))) {
        after[are_duplicates][sizes == 1] <- paste(
          dup, seq_len(sum(sizes == 1)), sep = "_"
        )
      } else {
        reference <- max(are_numeric, na.rm = TRUE)
        add <- seq_len(sum(sizes == 1) + 1)
        after[are_duplicates][sizes == 1] <- paste0(
          dup, "_", add[!(add == reference)]
        )
      }
    }
  }

  # we want to preserve column names specified in 'keep' and 'rename'
  if (!all(kept)) {
    after[kept] <- before[kept]
  }
  after[rename] <- names(rename)

  return(after)
}
