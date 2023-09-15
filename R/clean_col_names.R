#' clean column names of a data frame
#'
#' @param x the input data frame
#' @param report a list with the information about the effects of the
#'          the cleaning steps
#'
#' @return the a list with 2 elements: input data frame with a knit column names
#'          and the report object
#'
clean_col_names <- function(x, report = list()) {
  original_names <- col_names <- colnames(x)
  # standardize the column names
  col_names <- snakecase::to_snake_case(col_names)
  cleaned_names <- epitrix::clean_labels(col_names)

  # make column name unique
  unique_names <- make.unique(cleaned_names, sep = "_")

  # detect modified column names from the previous command
  original_name <- new_name <- NULL
  xx <- data.frame(cbind(original_name = original_names,
                         new_name = unique_names)) %>%
    dplyr::mutate(original_name = as.character(original_name),
                  new_name = as.character(new_name))
  idx <- which(xx[["original_name"]] != xx[["new_name"]])
  if (length(idx) > 0L) {
    report[["modified_column_names"]] <- xx[idx, ]
  }

  names(x) <- unique_names
  list(
    data = x,
    report = report
  )
}
